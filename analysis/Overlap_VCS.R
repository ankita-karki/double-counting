install.packages(c("rgdal", "sf", "leaflet", "dplyr"))
library(rgdal)
library(sf)
library(leaflet)
library(dplyr)


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
  #sf_obj$filename <- basename(file)
  return(sf_obj)
}

#Reading the KML files
cookstove_files <- list.files(path = "D:/Thesis/KML file/ALLPROJECT/VCS_cookstove", pattern = "\\.kml$", full.names = TRUE)
redd_files <- list.files(path = "D:/Thesis/KML file/ALLPROJECT/VCS_Redd", pattern = "\\.kml$", full.names = TRUE)

#Creating list of sf object for cookstove and REDD
cookstove_sf_list <- lapply(cookstove_files, read_and_make_valid)
redd_sf_list <- lapply(redd_files, read_and_make_valid)

redd_sf_list[[1]] <- redd_sf_list[[1]][1:3,]
###############################################
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
redd_sf_list[[57]]<- redd_sf_list[[57]][-1,]

elements_to_remove1 <- c(8,16,21,23)
redd_sf_list[[68]]<- redd_sf_list[[68]][-elements_to_remove1,]

###########################################
# Identify indices of sf objects with GEOMETRYCOLLECTION geometries in redd_sf_list
geometry_collection_indices <- which(sapply(cookstove_sf_list, function(x) {
  # Check if any geometry within each sf object is a GEOMETRYCOLLECTION
  any(st_geometry_type(x) == "POINT")
}))

# Extract those sf objects that contain GEOMETRYCOLLECTION geometries
geometry_collection_projects <- cookstove_sf_list[geometry_collection_indices]

# Optionally, print or use the information about which sf objects contain GEOMETRYCOLLECTIONs
if(length(geometry_collection_indices) > 0) {
  message("Files with GEOMETRYCOLLECTION geometries: ", paste(geometry_collection_indices, collapse = ", "))
} else {
  message("No files with GEOMETRYCOLLECTION geometries found.")
}


# Combining all cookstove sf objects into one sf object
all_cookstove_sf <- do.call(rbind, cookstove_sf_list)

# Combining all REDD+ sf objects into one sf object
all_redd_sf <- do.call(rbind, redd_sf_list)



# Load each GeoPackage file
overlap_Africa <- st_read("D:/Thesis/Version/25.02.2024/New folder/Overlap_Africa_VCS.gpkg")
overlap_Asia <- st_read("D:/Thesis/Version/25.02.2024/New folder/Overlap_Asia_VCS.gpkg")
overlap_centralamerica<- st_read("D:/Thesis/Version/25.02.2024/New folder/overlap_centralamerica_vcs.gpkg")

library(dplyr)

# Select only the columns you want to keep
#overlap_Africa <- select(overlap_Africa, column1, column2, column3)
overlap_Asia <- select(overlap_Asia, Name, Description, geom)
overlap_centralamerica <- select(overlap_centralamerica, Name, Description, geom)



# Combine the sf objects into one
combined_overlap_data <- rbind(overlap_Africa, overlap_Asia, overlap_centralamerica)


# Initialize the Leaflet 
leaflet() %>%
  addProviderTiles(providers$OpenStreetMap) %>%
  addTiles() %>%
  
  # Add combined overlap data
  addPolygons(data = combined_overlap_data, fillColor = ~pal(combined_overlap_data$variable), color = "#444444", weight = 1, opacity = 1, fillOpacity = 0.7) %>%
  
  # Assuming you have these as sf objects named cookstove_data_sf and redd_project_data_sf
  # Add cookstove project locations
  addCircleMarkers(data = all_cookstove_sf, radius = 6, color = "#FF0000", fillColor = "#FF0000", fillOpacity = 0.8, popup = ~paste("Cookstove Project:", Name)) %>%
  
  # Add REDD project locations
  addCircleMarkers(data = all_redd_sf, radius = 6, color = "#0000FF", fillColor = "#0000FF", fillOpacity = 0.8, popup = ~paste("REDD Project:", Name))

  library(sf)

# Example for one dataset
# Replace 'your_dataset_sf' with your actual dataset variable names one by one
# This code filters out GEOMETRYCOLLECTIONS and retains only polygons
all_cookstove_sf <- st_collection_extract(all_cookstove_sf, "POLYGON")

#Interactive plotting using leaflet
leaflet() %>%
  addTiles() %>%  # Adds a base map
  addPolygons(data = all_cookstove_sf, fillColor = "blue", color = "black", fillOpacity = 0.5, weight = 1, 
              group = "Cookstove Projects") %>%
  addPolygons(data = all_redd_sf, fillColor = "green", color = "black", fillOpacity = 0.5, weight = 1, 
              group = "REDD+ Projects") %>%
  addPolygons(data = combined_overlap_data, fillColor = "red", color = "black", fillOpacity = 0.7, weight = 1, 
              group = "Overlapping Areas") %>%
  addLayersControl(
    overlayGroups = c("Cookstove Projects", "REDD+ Projects", "Overlapping Areas"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  addLegend(position = "bottomright", 
            colors = c("blue", "green", "red"), 
            labels = c("Cookstove Projects", "REDD+ Projects", "Overlapping Areas"), 
            opacity = 0.5)



#Plotting only the overlap using leaflet 
leaflet() %>%
  addProviderTiles(providers$OpenStreetMap) %>%  # Add default OpenStreetMap tiles
  addPolygons(data = all_redd_sf, color = "#FF0000", weight = 1, smoothFactor = 0.5, opacity = 1.0, fillOpacity = 0.5) %>%
  addLayersControl(
    overlayGroups = c("Overlap"),
    options = layersControlOptions(collapsed = FALSE)
  )

#Plotting only the overlap using leaflet 
leaflet() %>%
  addProviderTiles(providers$OpenStreetMap) %>%  # Add default OpenStreetMap tiles
  addPolygons(data = all_cookstove_sf, color = "#FF0000", weight = 1, smoothFactor = 0.5, opacity = 1.0, fillOpacity = 0.5) %>%
  addLayersControl(
    overlayGroups = c("Overlap"),
    options = layersControlOptions(collapsed = FALSE)
  )

#Plotting only the overlap using leaflet 
leaflet() %>%
  addProviderTiles(providers$OpenStreetMap) %>%  # Add default OpenStreetMap tiles
  addPolygons(data = combined_overlap_data, color = "#FF0000", weight = 1, smoothFactor = 0.5, opacity = 1.0, fillOpacity = 0.5) %>%
  addLayersControl(
    overlayGroups = c("Overlap"),
    options = layersControlOptions(collapsed = FALSE)
  )
