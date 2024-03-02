#Installing packages
install.packages("sf")
install.packages("dplyr")
install.packages("leaflet")
install.packages("ggplot2")
install.packages("mapview")

#Loading the libraries
library(sf)
library(dplyr)
library(leaflet)
library(ggplot2)


#Creating function to read and validate geometries 
read_and_make_valid <- function(file) {
  sf_obj <- st_read(file, quiet = TRUE)  # Reads KML file into an sf object
  sf_obj <- st_make_valid(sf_obj)  # Fixes any invalid geometries
  
  # Drops Z (elevation) and M (measure) coordinates if present to simplify geometries
  if ("XYZ" %in% st_geometry_type(sf_obj) || "XYZM" %in% st_geometry_type(sf_obj)) {
    sf_obj <- st_zm(sf_obj, what = "Z")
  }
  if ("XYM" %in% st_geometry_type(sf_obj) || "XYZM" %in% st_geometry_type(sf_obj)) {
    sf_obj <- st_zm(sf_obj, what = "M")
  }
  return(sf_obj)
}

#Reading the KML files
cookstove_files <- list.files(path = "KML file/CDM_cookstove/Africa", pattern = "\\.kml$", full.names = TRUE)
redd_files <- list.files(path = "KML file/VCS_REDD/Africa", pattern = "\\.kml$", full.names = TRUE)

#Creating list of sf object for cookstove and REDD
cookstove_sf_list <- lapply(cookstove_files, read_and_make_valid)
redd_sf_list <- lapply(redd_files, read_and_make_valid)

#Transforming all geometries to a common CRS (WGS 84)
cookstove_sf_list <- lapply(cookstove_sf_list, function(sf) st_transform(sf, 4326))
redd_sf_list <- lapply(redd_sf_list, function(sf) st_transform(sf, 4326))


###################################################

cookstove_sf_list[[7]]  <- st_make_valid(cookstove_sf_list[[7]])
cookstove_sf_list[[12]] <- st_make_valid(cookstove_sf_list[[12]])

## Drop trouble maker ##
# the fourth row of the eleventh element of redd_sf_list causes the issues.  
# Dropping it results in a loop that is finished after < 1min on my computer. 
redd_sf_list[[1]] <- redd_sf_list[[1]][1:3,]
###########################
#Convert polygon to multipolygon 
#For REDD+ project
redd_sf <- function(redd_sf_list) {
  if ("POLYGON" %in% st_geometry_type(redd_sf_list)) {
    sf_object <- st_cast(redd_sf_list, "MULTIPOLYGON")
  }
  return(redd_sf_list)
}

# Apply the conversion to all sf objects
redd_sf_list<- lapply(redd_sf_list, redd_sf)

# Function to convert POLYGON to MULTIPOLYGON
cookstove_sf <- function(cookstove_sf_list) {
  # Only convert POLYGONs to MULTIPOLYGONs, leave other geometry types as they are
  if ("POLYGON" %in% st_geometry_type(cookstove_sf_list)) {
    sf_object <- st_cast(cookstove_sf_list, "MULTIPOLYGON")
  }
  return(cookstove_sf_list)
}

# Apply the conversion to all sf objects
cookstove_sf_list <- lapply(cookstove_sf_list, cookstove_sf)
###################################################

# Initialize an empty list to store indices of REDD geometries that overlap with cookstove geometries
overlap_indices_redd <- list()

# Loop through each REDD sf object
for (j in seq_along(redd_sf_list)) {
  
  # Initialize a logical vector for overlap status of the current REDD geometry
  overlaps_with_cookstove <- rep(FALSE, nrow(redd_sf_list[[j]]))
  
  # Loop through each cookstove sf object
  for (i in seq_along(cookstove_sf_list)) {
    
    # Perform the overlap analysis
    overlaps <- st_intersects(redd_sf_list[[j]], cookstove_sf_list[[i]], sparse = FALSE)
    
    # Check which REDD geometries have an overlap with any cookstove geometry
    overlaps_with_cookstove <- overlaps_with_cookstove | apply(overlaps, 1, any)
  }
  
  # Store the indices of REDD geometries that have an overlap with cookstove geometries
  overlap_indices_redd[[j]] <- which(overlaps_with_cookstove)
}


# Extract the overlapping REDD+ geometries into a new list of sf objects
overlapping_geometries_redd_list <- lapply(seq_along(overlap_indices_redd), function(j) {
  if (length(overlap_indices_redd[[j]]) > 0) {
    redd_sf_list[[j]][overlap_indices_redd[[j]], ]
  }
})

# Remove NULL elements if any exist due to no overlaps
overlapping_geometries_redd_list <- Filter(Negate(is.null), overlapping_geometries_redd_list)

overlap_sf <- do.call(rbind, overlapping_geometries_redd_list)

# Combine all cookstove sf objects into one sf object
all_cookstove_sf <- do.call(rbind, cookstove_sf_list)

# Combine all REDD+ sf objects into one sf object
all_redd_sf <- do.call(rbind, redd_sf_list)

#2. Overlapping analysis using st_intersection

# Combining all cookstove sf objects into one sf object
all_cookstove_sf <- do.call(rbind, cookstove_sf_list)

# Combining all REDD+ sf objects into one sf object
all_redd_sf <- do.call(rbind, redd_sf_list)

#Performing the overlap analysis using st_intersection

tryCatch({
  overlap_sf <- st_intersection(all_cookstove_sf, all_redd_sf)
}, error = function(e) {
  message("Error during intersection: ", e$message)
  # Use st_intersection again after making geometries valid and simplified
  all_cookstove_sf <- st_make_valid(all_cookstove_sf) %>% st_simplify(preserveTopology = TRUE)
  all_redd_sf <- st_make_valid(all_redd_sf) %>% st_simplify(preserveTopology = TRUE)
  overlap_sf <- st_intersection(all_cookstove_sf, all_redd_sf)
})

##################################################
# Plotting the overlap using leaflet
leaflet() %>%
  addProviderTiles(providers$OpenStreetMap) %>%
  addPolygons(data = all_cookstove_sf, fillColor = "blue", fillOpacity = 0.5, color = "white", weight = 1) %>%
  addPolygons(data = all_redd_sf, fillColor = "grey", fillOpacity = 0.5, color = "white", weight = 1, dashArray = "5, 5") %>%
  addPolygons(data = overlap_sf, fillColor = "red", fillOpacity = 0.7, color = "black", weight = 1) %>%
  addLegend("bottomright", colors = c("blue", "grey", "red"), labels = c("Cookstove", "REDD+", "Overlap"), opacity = 0.5)

#Plotting only the overlap using leaflet 
leaflet() %>%
  addProviderTiles(providers$OpenStreetMap) %>%  # Add default OpenStreetMap tiles
  addPolygons(data = overlap_sf, color = "#FF0000", weight = 1, smoothFactor = 0.5, opacity = 1.0, fillOpacity = 0.5) %>%
  addLayersControl(
    overlayGroups = c("Overlap"),
    options = layersControlOptions(collapsed = FALSE)
  )
#Interactive plotting using leaflet
leaflet() %>%
  addTiles() %>%  # Adds a base map
  addPolygons(data = all_cookstove_sf, fillColor = "blue", color = "black", fillOpacity = 0.5, weight = 1, 
              group = "Cookstove Projects") %>%
  addPolygons(data = all_redd_sf, fillColor = "green", color = "black", fillOpacity = 0.5, weight = 1, 
              group = "REDD+ Projects") %>%
  addPolygons(data = overlap_sf, fillColor = "red", color = "black", fillOpacity = 0.7, weight = 1, 
              group = "Overlapping Areas") %>%
  addLayersControl(
    overlayGroups = c("Cookstove Projects", "REDD+ Projects", "Overlapping Areas"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  addLegend(position = "bottomright", 
            colors = c("blue", "green", "red"), 
            labels = c("Cookstove Projects", "REDD+ Projects", "Overlapping Areas"), 
            opacity = 0.5)


##################################################
##################################################
#Saving file in geopackage format 
# Specify the path to your folder and the filename
overlap_Africa_VCS <- "D:/Thesis/Version/25.02.2024/New folder/Overlap_Africa_VCS.gpkg"

# Save the sf object as a GeoPackage
st_write(overlap_sf, overlap_Africa_VCS, delete_layer = TRUE)
