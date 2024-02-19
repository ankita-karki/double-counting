#Installing packages
install.packages("sf")
install.packages("dplyr")
install.packages("leaflet")
install.packages("ggplot2")
install.packages("mapview")

#Loading the library 
library(sf)
library(dplyr)
library(leaflet)
library(ggplot2)
library(mapview)
library(sp)

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
cookstove_files <- list.files(path = "./KML file/GS_COOKSTOVE_NEW/Asia", pattern = "\\.kml$", full.names = TRUE)
redd_files <- list.files(path = "./KML file/VCS_REDD/Asia", pattern = "\\.kml$", full.names = TRUE)

#Creating list of sf object for cookstove and REDD
cookstove_sf_list <- lapply(cookstove_files, read_and_make_valid)
redd_sf_list <- lapply(redd_files, read_and_make_valid)

###########################
#Transforming all geometries to a common CRS (WGS 84)
cookstove_sf_list <- lapply(cookstove_sf_list, function(sf) st_transform(sf, 4326))
redd_sf_list <- lapply(redd_sf_list, function(sf) st_transform(sf, 4326))


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

#For cookstove project 
cookstove_sf_list <- lapply(cookstove_files, st_read)

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

###########################################

##1. Overlapping analysis using loop 
# Initialize an empty list to store indices of cookstove geometries that overlap with REDD geometries
overlap_indices <- list()

# Loop through each cookstove sf object
for (i in seq_along(cookstove_sf_list)) {
  
  # Initialize a logical vector for overlap status of the current cookstove geometry
  # Initially set all to FALSE (no overlap)
  # Using nrow() to get the number of features in the sf object
  overlaps_with_redd <- rep(FALSE, nrow(cookstove_sf_list[[i]]))
  
  # Loop through each REDD sf object
  for (j in seq_along(redd_sf_list)) {
    
    # Perform the overlap analysis
    # TRUE if geometries overlap, FALSE otherwise
    overlaps <- st_intersects(cookstove_sf_list[[i]], redd_sf_list[[j]], sparse = FALSE)
    
    # Check which cookstove geometries have an overlap with any REDD geometry
    overlaps_with_redd <- overlaps_with_redd | apply (overlaps, 1, any)
  }
  
  # Store the indices of cookstove geometries that have an overlap with REDD geometries
  overlap_indices[[i]] <- which(overlaps_with_redd)
}

##############################
#2. Overlapping analysis using st_intersection

# Combine all cookstove sf objects into one sf object
all_cookstove_sf <- do.call(rbind, cookstove_sf_list)

# Combine all REDD+ sf objects into one sf object
all_redd_sf <- do.call(rbind, redd_sf_list)

#Perform the overlap analysis using st_intersection

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


