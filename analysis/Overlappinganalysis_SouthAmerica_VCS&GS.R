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
library(mapview)

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
  
  # Add the filename as a property of the sf object
  sf_obj$filename <- basename(file)
  return(sf_obj)
}

#Reading the KML files
cookstove_files <- list.files(path = "D:/Thesis/KML file/GS_COOKSTOVE_NEW/South America", pattern = "\\.kml$", full.names = TRUE)
redd_files <- list.files(path = "D:/Thesis/KML file/VCS_REDD/America/South America", pattern = "\\.kml$", full.names = TRUE)

#Creating list of sf object for cookstove and REDD
cookstove_sf_list <- lapply(cookstove_files, read_and_make_valid)
redd_sf_list <- lapply(redd_files, read_and_make_valid)

####################################

# Identify indices of sf objects with GEOMETRYCOLLECTION geometries in redd_sf_list
geometry_collection_indices <- which(sapply(redd_sf_list, function(x) {
  # Check if any geometry within each sf object is a GEOMETRYCOLLECTION
  any(st_geometry_type(x) == "GEOMETRYCOLLECTION")
}))

# Extract those sf objects that contain GEOMETRYCOLLECTION geometries
geometry_collection_projects <- redd_sf_list[geometry_collection_indices]

# Optionally, print or use the information about which sf objects contain GEOMETRYCOLLECTIONs
if(length(geometry_collection_indices) > 0) {
  message("Files with GEOMETRYCOLLECTION geometries: ", paste(geometry_collection_indices, collapse = ", "))
} else {
  message("No files with GEOMETRYCOLLECTION geometries found.")
}

# Assuming redd_sf_list is your list and you have identified specific indices but want to drop the first element of the original list
redd_sf_list[[31]]<- redd_sf_list[[31]][-1,]

elements_to_remove <- c(8,16,21,23)
redd_sf_list[[40]]<- redd_sf_list[[40]][-elements_to_remove,]


####################
#Loop analysis
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

#Combine all overlap sf object into sf object
overlap_sf <- do.call(rbind, overlapping_geometries_redd_list)

# Combine all cookstove sf objects into one sf object
all_cookstove_sf <- do.call(rbind, cookstove_sf_list)

# Combine all REDD+ sf objects into one sf object
all_redd_sf <- do.call(rbind, redd_sf_list)

###########################################

############################
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
  addPolygons(data = all_cookstove_sf, color = "#FF0000", weight = 1, smoothFactor = 0.5, opacity = 1.0, fillOpacity = 0.5) %>%
  addLayersControl(
    overlayGroups = c("Overlap"),
    options = layersControlOptions(collapsed = FALSE)
  )

leaflet() %>%
  addProviderTiles(providers$OpenStreetMap) %>%  # Add default OpenStreetMap tiles
  addPolygons(data = all_redd_sf, color = "#FF0000", weight = 1, smoothFactor = 0.5, opacity = 1.0, fillOpacity = 0.5) %>%
  addLayersControl(
    overlayGroups = c("Overlap"),
    options = layersControlOptions(collapsed = FALSE)
  )


leaflet() %>%
  addTiles() %>%  # Adds a base map
  addPolygons(data = all_cookstove_sf, fillColor = "blue", color = "black", fillOpacity = 0.5, weight = 1, 
              group = "Cookstove Projects", label = ~filename) %>%
  addPolygons(data = all_redd_sf, fillColor = "black", color = "black", fillOpacity = 0.5, weight = 1, 
              group = "REDD+ Projects", label = ~filename) %>%
  addPolygons(data = overlap_sf, fillColor = "red", color = "black", fillOpacity = 0.7, weight = 1, 
              group = "Overlapping Areas", label = ~filename) %>%
  addLayersControl(
    overlayGroups = c("Cookstove Projects", "REDD+ Projects", "Overlapping Areas"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  addLegend(position = "bottomright", 
            colors = c("blue", "black", "red"), 
            labels = c("Cookstove Projects", "REDD+ Projects", "Overlapping Areas"), 
            opacity = 0.5)

#Saving file in geopackage format 
# Specify the path to your folder and the filename
overlap_southamerica_VCS_GS <- "D:/Thesis/Version/25.02.2024/New folder/Overlap_southamerica_vcs_gs.gpkg"

# Save the sf object as a GeoPackage
st_write(overlap_sf, overlap_southamerica_VCS_GS, delete_layer = TRUE)

