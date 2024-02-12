############# 
#############
#Installing packages
install.packages("sf")
install.packages("PROJ")
install.packages("dplyr")
install.packages("leaflet")
install.packages("ggplot2")
install.packages("mapview")

#Loading the library 
library(sf)
library(PROJ)
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
  return(sf_obj)
}

#Reading the KML files
cookstove_files <- list.files(path = "D:/Thesis/KML file/VCS_COOKSTOVE/Asia", pattern = "\\.kml$", full.names = TRUE)
redd_files <- list.files(path = "D:/Thesis/KML file/VCS_REDD/Asia", pattern = "\\.kml$", full.names = TRUE)

#Creating list of sf object for cookstove and REDD
cookstove_sf_list <- lapply(cookstove_files, read_and_make_valid)
redd_sf_list <- lapply(redd_files, read_and_make_valid)


#Transforming all geometries to a common CRS (WGS 84)
cookstove_sf_list <- lapply(cookstove_sf_list, function(sf) st_transform(sf, 4326))
redd_sf_list <- lapply(redd_sf_list, function(sf) st_transform(sf, 4326))

############################################
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

####################################################################

#Overlapping analysis(1)

# Initialize an empty list to store indices of cookstove geometries that overlap with REDD geometries
overlap_indices <- list()

# Loop through each cookstove sf object
for (i in seq_along(cookstove_sf_list)) {
  
  # Initialize a logical vector for overlap status of the current cookstove geometry
  # Initially set all to FALSE (no overlap)
  # Using nrow() instead of st_length() to get the number of features in the sf object
  overlaps_with_redd <- rep(FALSE, nrow(cookstove_sf_list[[i]]))
  
  # Loop through each REDD sf object
  for (j in seq_along(redd_sf_list)) {
    
    # Perform the overlap analysis
    # This creates a list of logical matrices: TRUE if geometries overlap, FALSE otherwise
    overlaps <- st_intersects(cookstove_sf_list[[i]], redd_sf_list[[j]], sparse = FALSE)
    
    # Check which cookstove geometries have an overlap with any REDD geometry
    # We update the logical vector with TRUE for any cookstove geometry that overlaps
    overlaps_with_redd <- overlaps_with_redd | apply (overlaps, 1, any)
  }
  
  # Store the indices of cookstove geometries that have an overlap with REDD geometries
  overlap_indices[[i]] <- which(overlaps_with_redd)
}
##############################

#Overlapping or union analysis(2)
# Combine all cookstove sf objects into one sf object
all_cookstove_sf <- do.call(rbind, cookstove_sf_list)

# Combine all REDD+ sf objects into one sf object
all_redd_sf <- do.call(rbind, redd_sf_list)

#Perform the overlap analysis using st_intersection
# If st_intersection throws an error, try st_union or other functions to find common areas
tryCatch({
  overlap_sf <- st_intersection(all_cookstove_sf, all_redd_sf)
}, error = function(e) {
  # If an error occurs, print the message
  print(e)
  # Then try another method to find overlaps
  # For example, st_union can sometimes be used to find common areas if geometries are complex
  overlap_sf <- st_union(all_cookstove_sf, all_redd_sf)
  # Note that st_union does not necessarily give you the intersection and might need further processing
})

##################################################
#Plotting the overlap 

# Perform the overlap analysis using st_intersection
tryCatch({
  overlap_sf <- st_intersection(all_cookstove_sf, all_redd_sf)
}, error = function(e) {
  message("Error during intersection: ", e$message)
  # Instead of st_union, use st_intersection again after making geometries valid and simplified
  all_cookstove_sf <- st_make_valid(all_cookstove_sf) %>% st_simplify(preserveTopology = TRUE)
  all_redd_sf <- st_make_valid(all_redd_sf) %>% st_simplify(preserveTopology = TRUE)
  overlap_sf <- st_intersection(all_cookstove_sf, all_redd_sf)
})

# Plotting the overlap using leaflet
leaflet() %>%
  addProviderTiles(providers$OpenStreetMap) %>%
  addPolygons(data = all_cookstove_sf, fillColor = "blue", fillOpacity = 0.5, color = "white", weight = 1) %>%
  addPolygons(data = all_redd_sf, fillColor = "red", fillOpacity = 0.5, color = "white", weight = 1) %>%
  addPolygons(data = overlap_sf, fillColor = "purple", fillOpacity = 0.5, color = "white", weight = 1) %>%
  addLegend("bottomright", colors = c("blue", "red", "purple"), labels = c("Cookstove", "REDD+", "Overlap"), opacity = 0.5)


# Plotting the overlap using leaflet
leaflet() %>%
  addProviderTiles(providers$OpenStreetMap) %>%
  addPolygons(data = all_cookstove_sf, fillColor = "blue", fillOpacity = 0.5, color = "white", weight = 1) %>%
  addPolygons(data = all_redd_sf, fillColor = "grey", fillOpacity = 0.5, color = "white", weight = 1, dashArray = "5, 5") %>%
  addPolygons(data = overlap_sf, fillColor = "red", fillOpacity = 0.7, color = "black", weight = 1) %>%
  addLegend("bottomright", colors = c("blue", "grey", "red"), labels = c("Cookstove", "REDD+", "Overlap"), opacity = 0.5)

leaflet() %>%
  addProviderTiles(providers$OpenStreetMap) %>%  # Add default OpenStreetMap tiles
  addPolygons(data = overlap_sf, color = "#FF0000", weight = 1, smoothFactor = 0.5, opacity = 1.0, fillOpacity = 0.5) %>%
  addLayersControl(
    overlayGroups = c("Overlap"),
    options = layersControlOptions(collapsed = FALSE)
  )


##################################################


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
cookstove_files <- list.files(path = "D:/Thesis/KML file/VCS_COOKSTOVE/LatinAmerica", pattern = "\\.kml$", full.names = TRUE)
redd_files <- list.files(path = "D:/Thesis/KML file/VCS_REDD/LatinAmerica", pattern = "\\.kml$", full.names = TRUE)

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
# Initialize an empty list to store indices of cookstove geometries that overlap with REDD geometries
overlap_indices <- list()

# Assuming cookstove_sf_list and redd_sf_list are lists of sf objects
for (i in seq_along(cookstove_sf_list)) {
  # Initialize overlaps_with_redd as FALSE for each cookstove geometry
  overlaps_with_redd <- rep(FALSE, nrow(cookstove_sf_list[[i]]))
  
  for (j in seq_along(redd_sf_list)) {
    # Perform the overlap analysis
    overlaps <- st_intersects(cookstove_sf_list[[i]], redd_sf_list[[j]], sparse = FALSE)
    
    # Check for any overlap for each cookstove geometry
    if (!is.null(dim(overlaps))) {  # Ensure overlaps is not NULL and has dimensions
      overlaps_with_redd <- overlaps_with_redd | apply(overlaps, 1, any)
    }
  }
  
  # Store the indices of cookstove geometries that have an overlap
  overlap_indices[[i]] <- which(overlaps_with_redd)


}

# Combine all cookstove sf objects into one sf object
all_cookstove_sf <- do.call(rbind, cookstove_sf_list)

# Combine all REDD+ sf objects into one sf object
all_redd_sf <- do.call(rbind, redd_sf_list)

#Perform the overlap analysis using st_intersection
# If st_intersection throws an error, try st_union or other functions to find common areas
tryCatch({
  overlap_sf <- st_intersection(all_cookstove_sf, all_redd_sf)
}, error = function(e) {
  # If an error occurs, print the message
  print(e)
  # Then try another method to find overlaps
  # For example, st_union can sometimes be used to find common areas if geometries are complex
  overlap_sf <- st_union(all_cookstove_sf, all_redd_sf)
  # Note that st_union does not necessarily give you the intersection and might need further processing
})


#Plotting the overlap 

# Perform the overlap analysis using st_intersection
tryCatch({
  overlap_sf <- st_intersection(all_cookstove_sf, all_redd_sf)
}, error = function(e) {
  message("Error during intersection: ", e$message)
  # Instead of st_union, use st_intersection again after making geometries valid and simplified
  all_cookstove_sf <- st_make_valid(all_cookstove_sf) %>% st_simplify(preserveTopology = TRUE)
  all_redd_sf <- st_make_valid(all_redd_sf) %>% st_simplify(preserveTopology = TRUE)
  overlap_sf <- st_intersection(all_cookstove_sf, all_redd_sf)
})

# Plotting the overlap using leaflet
leaflet() %>%
  addProviderTiles(providers$OpenStreetMap) %>%
  addPolygons(data = all_cookstove_sf, fillColor = "blue", fillOpacity = 0.5, color = "white", weight = 1) %>%
  addPolygons(data = all_redd_sf, fillColor = "red", fillOpacity = 0.5, color = "white", weight = 1) %>%
  addPolygons(data = overlap_sf, fillColor = "purple", fillOpacity = 0.5, color = "white", weight = 1) %>%
  addLegend("bottomright", colors = c("blue", "red", "purple"), labels = c("Cookstove", "REDD+", "Overlap"), opacity = 0.5)


# Plotting the overlap using leaflet
leaflet() %>%
  addProviderTiles(providers$OpenStreetMap) %>%
  addPolygons(data = all_cookstove_sf, fillColor = "blue", fillOpacity = 0.5, color = "white", weight = 1) %>%
  addPolygons(data = all_redd_sf, fillColor = "grey", fillOpacity = 0.5, color = "white", weight = 1, dashArray = "5, 5") %>%
  addPolygons(data = overlap_sf, fillColor = "red", fillOpacity = 0.7, color = "black", weight = 1) %>%
  addLegend("bottomright", colors = c("blue", "grey", "red"), labels = c("Cookstove", "REDD+", "Overlap"), opacity = 0.5)


# Plotting the overlap using leaflet
library(leaflet)

# Assuming 'overlap_sf' is an sf object that contains the overlapping areas
# The leaflet map will be created with this sf object

leaflet() %>%
  addProviderTiles(providers$OpenStreetMap) %>%  # Add default OpenStreetMap tiles
  addPolygons(data = all_redd_sf, color = "#FF0000", weight = 1, smoothFactor = 0.5, opacity = 1.0, fillOpacity = 0.5) %>%
  addLayersControl(
    overlayGroups = c("Overlap"),
    options = layersControlOptions(collapsed = FALSE)
  )


