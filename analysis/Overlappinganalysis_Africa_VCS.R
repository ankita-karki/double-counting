#Installing packages
install.packages("sf")
install.packages("dplyr")
install.packages("leaflet")
install.packages("raster")
install.packages("rgdal")
install.packages("PROJ", dependencies = TRUE)
devtools::install_github("CRAN/rgdal")

#Loading the library 
library(sf)
library(sp)
library(dplyr)
library(leaflet)
library(raster)
library(PROJ)
library(rgdal)

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
cookstove_files <- list.files(path = "KML file/VCS_COOKSTOVE/Africa", pattern = "\\.kml$", full.names = TRUE)
avoided_def_files <- list.files(path = "KML file/VCS_REDD/Africa", pattern = "\\.kml$", full.names = TRUE)

#Creating list of sf object for cookstove and Avoided deforestation
cookstove_sf_list <- lapply(cookstove_files, read_and_make_valid)
avoided_def_list <- lapply(avoided_def_files, read_and_make_valid)

#Transforming all geometries to a common CRS (WGS 84)
# Define WGS84 (EPSG:4326) CRS object
#wgs84_crs <- CRS("+init=epsg:4326")
#cookstove_sf_list <- lapply(cookstove_sf_list, function(sf) st_transform(sf, 4326))
#avoided_def_list <- lapply(avoided_def_list, function(sf) st_transform(sf, 4326))

cookstove_sf_list[[7]]  <- st_make_valid(cookstove_sf_list[[7]])
cookstove_sf_list[[12]] <- st_make_valid(cookstove_sf_list[[12]])

## Drop trouble maker ##
# the fourth row of the eleventh element of redd_sf_list causes the issues.  
# Dropping it results in a loop that is finished after < 1min on my computer. 
#redd_sf_list[[1]] <- redd_sf_list[[1]][1:3,]
####################################################################
##1. Overlapping analysis using loop 

###Loop analysis
# Initialize an empty list to store indices of Avoided Deforestation geometries that overlap with cookstove geometries
overlap_indices_ad <- list()

# Loop through each Avoided deforestation sf object
for (j in seq_along(avoided_def_list)) {
  
  # Initialize a logical vector for overlap status of the current REDD geometry
  overlaps_with_cookstove <- rep(FALSE, nrow(avoided_def_list[[j]]))
  
  # Loop through each cookstove sf object
  for (i in seq_along(cookstove_sf_list)) {
    
    # Perform the overlap analysis
    overlaps <- st_intersects(avoided_def_list[[j]], cookstove_sf_list[[i]], sparse = FALSE)
    
    # Check which deforestation geometries have an overlap with any cookstove geometry
    overlaps_with_cookstove <- overlaps_with_cookstove | apply(overlaps, 1, any)
  }
  
  # Store the indices of avoided deforestation geometries that have an overlap with cookstove geometries
  overlap_indices_ad[[j]] <- which(overlaps_with_cookstove)
}

# Extract the overlapping avoided deforestation geometries into a new list of sf objects
overlapping_geometries_ad_list <- lapply(seq_along(overlap_indices_ad), function(j) {
  if (length(overlap_indices_ad[[j]]) > 0) {
    avoided_def_list[[j]][overlap_indices_ad[[j]], ]
  }
})

# Remove NULL elements if any exist due to no overlaps
overlapping_geometries_ad_list <- Filter(Negate(is.null), overlapping_geometries_ad_list)

#Combine all overlap sf object into sf object
overlap_sf <- do.call(rbind, overlapping_geometries_ad_list)

##############################
#2.Overlapping analysis using st_intersection

# Combine all cookstove sf objects into one sf object
all_cookstove_sf <- do.call(rbind, cookstove_sf_list)

# Combine all REDD+ sf objects into one sf object
all_avoided_def_sf <- do.call(rbind, avoided_def_list)

#Perform the overlap analysis using st_intersection
tryCatch({
  overlap_sf <- st_intersection(all_cookstove_sf, all_avoided_def_sf)
}, error = function(e) {
  message("Error during intersection: ", e$message)
  # Use st_intersection again after making geometries valid and simplified
  all_cookstove_sf <- st_make_valid(all_cookstove_sf) %>% st_simplify(preserveTopology = TRUE)
  all_avoided_def_sf <- st_make_valid(all_avoided_def_sf) %>% st_simplify(preserveTopology = TRUE)
  overlap_sf <- st_intersection(all_cookstove_sf, all_avoided_def_sf)
})

##################################################
# Plotting the overlap using leaflet
leaflet() %>%
  addProviderTiles(providers$OpenStreetMap) %>%
  addPolygons(data = all_cookstove_sf, fillColor = "blue", fillOpacity = 0.5, color = "white", weight = 1) %>%
  addPolygons(data = all_avoided_def_sf, fillColor = "grey", fillOpacity = 0.5, color = "white", weight = 1, dashArray = "5, 5") %>%
  addPolygons(data = overlap_sf, fillColor = "red", fillOpacity = 0.7, color = "black", weight = 1) %>%
  addLegend("bottomright", colors = c("blue", "grey", "red"), labels = c("Cookstove", "Avoided Deforestation", "Overlap"), opacity = 0.5)

#Plotting only the overlap using leaflet 
leaflet() %>%
  addProviderTiles(providers$OpenStreetMap) %>%  # Add default OpenStreetMap tiles
  addPolygons(data = overlap_sf, color = "#FF0000", weight = 1, smoothFactor = 0.5, opacity = 1.0, fillOpacity = 0.5) %>%
  addLayersControl(
    overlayGroups = c("Overlap"),
    options = layersControlOptions(collapsed = FALSE)
  )

# Plotting using leaflet
leaflet() %>%
  addProviderTiles(providers$OpenStreetMap) %>%
  addPolygons(data = all_cookstove_sf, fillColor = "blue", fillOpacity = 0.5, color = "white", weight = 1, 
              group = "Cookstove Projects") %>%
  addPolygons(data = all_avoided_def_sf, fillColor = "green", color = "black", fillOpacity = 0.5, weight = 1, 
              group = "Avoided Deforestation Projects") %>%
  addPolygons(data = overlap_sf, fillColor = "red", color = "black", fillOpacity = 0.7, weight = 1, 
              group = "Overlapping Areas") %>%
  addLayersControl(overlayGroups = c("Cookstove Projects", "Avoided Deforestation Projects", "Overlapping Areas"),
                   options = layersControlOptions(collapsed = FALSE)) %>%
  addLegend(position = "bottomright", colors = c("blue", "green", "red"), 
            labels = c("Cookstove Projects", "Avoided Deforestation Projects", "Overlapping Areas"), opacity = 0.5)

##################################################
#Saving file in geopackage format 
#Specify the path to your folder and the filename
overlap_Asia_VCS <- "D:/Thesis/Version/25.02.2024/New folder/Overlap_Asia_VCS.gpkg"

# Save the sf object as a GeoPackage
st_write(overlap_sf, overlap_Asia_VCS, delete_layer = TRUE)


########################
#Proximity analysis 

# Assume all_cookstove_sf and all_avoided_def_sf are already defined sf objects

buffer_distances <- c(5000, 10000, 15000) # Distances in meters
buffer_colors <- c("#FF0000", "#00FF00", "#0000FF") # Colors for each buffer distance

# Initialize the leaflet map with OpenStreetMap tiles
m <- leaflet() %>%
  addProviderTiles(providers$OpenStreetMap)

# Add forest cover areas as polygons to the map
m <- m %>%
  addPolygons(data = forest_cover_sf,
              fillColor = "transparent", # Forest green
              color = "#006400", # Dark green border
              weight = 1,
              fillOpacity = 0.5,
              group = "Forest Cover")
              
m

# Loop through each buffer distance to create and add buffer polygons to the map
for (i in seq_along(buffer_distances)) {
  # Dynamically create buffer for each distance
  current_buffer <- st_buffer(all_cookstove_sf, dist = buffer_distances[i])
  
  # Add the buffer as a polygon layer to the map with a unique color and group
  m <- m %>%
    addPolygons(data = current_buffer, 
                fillColor = "transparent",
                color = buffer_colors[i],
                fillOpacity = 0.3,
                weight = 2,
                opacity = 0.8,
                popup = ~paste(buffer_distances[i] / 1000, "km buffer"),
                group = paste0(buffer_distances[i], " m Buffer"))
}

# Add avoided deforestation project areas as polygons to the map
m <- m %>%
  addPolygons(data = all_avoided_def_sf,
              fillColor = "#000000", # Fill color for avoided deforestation projects
              color = "#000000", # Border color for avoided deforestation projects
              weight = 2,
              fillOpacity = 0.7, # Adjusted for visibility
              popup = ~as.character(filename), # Adjust 'filename' to your specific column name if different
              group = "Avoided Deforestation")




# Add layers control to toggle visibility of each buffer layer and the avoided deforestation layer
m <- m %>%
  addLayersControl(overlayGroups = c(paste0(buffer_distances, " m Buffer"), "Avoided Deforestation"),
                   options = layersControlOptions(collapsed = FALSE))

# Print the map
m

##################################
# Your existing code for initializing the map and adding buffer polygons...

# Load the forest cover raster
forest_cover <- raster("D:/Thesis/Forestcover/forestcover/AfricaLandCover2020.tif")

plot(forest_cover)
# Aggregate the raster to lower the resolution
# Factor 2 means that each cell in the new raster represents a 2x2 cell area of the original
simplified_raster <- aggregate(forest_cover, fact=5, fun=mean)

# Now try converting the simplified raster to polygons
simplified_polygons <- rasterToPolygons(simplified_raster, fun=function(x) {x==1}, dissolve=TRUE)


# Convert forest cover raster to polygons
# Convert raster to polygons
polygon_layer <- rasterToPolygons(forest_cover, fun=function(x) {x==1}, dissolve=TRUE)

# Convert to sf object
polygon_sf <- st_as_sf(polygon_layer)
# Calculate distance from each cookstove project to the nearest forest cover
all_cookstove_sf$nearest_forest_distance <- st_distance(all_cookstove_sf, forest_cover_sf, by_element=TRUE) %>% apply(1, min)

# Calculate distance from each avoided deforestation project to the nearest forest cover
all_avoided_def_sf$nearest_forest_distance <- st_distance(all_avoided_def_sf, forest_cover_sf, by_element=TRUE) %>% apply(1, min)



# Add layers control to include the Forest Cover layer
m <- m %>%
  addLayersControl(overlayGroups = c(paste0(buffer_distances, " m Buffer"), "Avoided Deforestation", "Forest Cover"),
                   options = layersControlOptions(collapsed = FALSE))

# Print the map
m


#######################
#Forest land cover overlay 
#############################################
library(sf)
library(raster)
library(leaflet)
library(rgdal)

# Your existing code for initializing the map and adding buffer polygons...

# Load the forest cover raster
forest_cover <- raster("D:/Thesis/Forestcover/forestcover/123.tif")

# Reclassify the raster
# Set values of 1 to 1, and everything else to NA
reclass <- c(-Inf, 0.9999, NA, 
             0.9999, 1.0001, 1, 
             1.0001, Inf, NA)
rclmat <- matrix(reclass, ncol=3, byrow=TRUE)

forest_raster_reclass <- reclassify(forest_cover, rclmat)


# Assuming your reclassified forest raster is 'forest_raster_reclass'
# Reclassify the raster: we want to create a binary raster with 1 for forest and NA for non-forest
forest_raster_reclass <- reclassify(forest_cover, cbind(1, 1), right=NA)

# Convert reclassified raster to a SpatialPolygonsDataFrame for plotting
forest_polygons <- rasterToPolygons(forest_raster_reclass, na.rm=TRUE, dissolve=TRUE)

# Plot with leaflet
leaflet() %>%
  addProviderTiles(providers$Esri.WorldImagery) %>%  # Adding a nice basemap
  addPolygons(data = forest_polygons,
              fillColor = "#008000",
              fillOpacity = 0.5,
              color = "#004000",
              weight = 1,
              smoothFactor = 0.5) %>%
  addLegend("bottomright", 
            colors = "#008000", 
            labels = "Forest cover",
            title = "Legend")









# Convert forest cover raster to polygons
polygon_layer <- rasterToPolygons(forest_raster_reclass, fun=function(x) {x==1}, dissolve=TRUE)

# Convert to sf object
polygon_sf <- st_as_sf(polygon_layer)
# Calculate distance from each cookstove project to the nearest forest cover
all_cookstove_sf$nearest_forest_distance <- st_distance(all_cookstove_sf, forest_cover_sf, by_element=TRUE) %>% apply(1, min)

# Calculate distance from each avoided deforestation project to the nearest forest cover
all_avoided_def_sf$nearest_forest_distance <- st_distance(all_avoided_def_sf, forest_cover_sf, by_element=TRUE) %>% apply(1, min)

#######################

library(leaflet)

# Assuming your reclassified forest raster is 'forest_raster_reclass'
# Reclassify the raster: we want to create a binary raster with 1 for forest and NA for non-forest
forest_raster_reclass <- reclassify(forest_cover, cbind(1, 1), right=NA)

# Convert reclassified raster to a SpatialPolygonsDataFrame for plotting
forest_polygons <- rasterToPolygons(forest_raster_reclass, na.rm=TRUE, dissolve=TRUE)

# Plot with leaflet
leaflet() %>%
  addProviderTiles(providers$Esri.WorldImagery) %>%  # Adding a nice basemap
  addPolygons(data = forest_polygons,
              fillColor = "#008000",
              fillOpacity = 0.5,
              color = "#004000",
              weight = 1,
              smoothFactor = 0.5) %>%
  addLegend("bottomright", 
            colors = "#008000", 
            labels = "Forest cover",
            title = "Legend")


##############
#raster to tiles 
# Install packages if they're not already installed
if (!requireNamespace("leaflet", quietly = TRUE)) install.packages("leaflet")
if (!requireNamespace("terra", quietly = TRUE)) install.packages("terra")
if (!requireNamespace("htmlwidgets", quietly = TRUE)) install.packages("htmlwidgets")

# Load the necessary libraries
library(leaflet)
library(terra)
library(htmlwidgets)
library(raster)

# Load the forest cover raster
raster <- raster("D:/Thesis/Forestcover/forestcover/AfricaLandCover2020.tif")

# Factor 2 means that each cell in the new raster represents a 2x2 cell area of the original
raster <- aggregate(rasterLayer, fact=1, fun=mean)

# Create a map object using leaflet
m <- leaflet() %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>% 
  setView(lng = mean(ext(raster)[1:2]), lat = mean(ext(raster)[3:4]), zoom = 10)

# Add the raster layer to the map



# Create a color palette where forest areas are #4d9221 and others are transparent
colors <- ifelse(values(raster) == 1, "#4d9221", "#FFFFFF00")
# Note: For larger rasters, consider using addRasterImage for direct display might not be efficient
m <- m %>% addRasterImage(raster, colors = colors, opacity = 0.8)

# Print the map
m

# Create a Leaflet map
m <- leaflet() %>%
  addProviderTiles(providers$OpenStreetMap) %>%
  setView(lng = mean(ext(raster)[1:2]), lat = mean(ext(raster)[3:4]), zoom = 10) %>%
  addRasterImage(raster, colors = colors, opacity = 1, project = FALSE)

# Display the map
m


#############################
forest_cover_sf <- st_read("D:/Thesis/Forestcover/forestcover/New folder/AfricaForestCoverVectors.shp")

forest_cover_sf <- st_set_crs(forest_cover_sf, 4326)






# Proximity analysis setup
buffer_distances <- c(5000, 10000, 15000) # Distances in meters
buffer_colors <- c("#FF0000", "#00FF00", "#0000FF") # Colors for each buffer distance

# Initialize the leaflet map with OpenStreetMap tiles
m <- leaflet() %>%
  addProviderTiles(providers$OpenStreetMap)

# Loop through each buffer distance to create buffers and add to the map
for (i in seq_along(buffer_distances)) {
  # Create buffer for each cookstove location
  current_buffer <- st_buffer(all_cookstove_sf, dist = buffer_distances[i])
  
  # Intersection between current buffer and forest cover
  forest_in_buffer <- st_intersection(current_buffer, forest_cover_sf)
  
  # Add the intersected forest cover as a layer (adjust fillColor as needed)
  m <- m %>%
    addPolygons(data = forest_in_buffer, 
                fillColor = buffer_colors[i],
                fillOpacity = 0.5,
                color = buffer_colors[i],
                weight = 1,
                opacity = 1,
                group = paste0(buffer_distances[i], "m Forest Cover"),
                popup = ~paste(buffer_distances[i] / 1000, "km buffer - Forest Cover"))
}

# Add avoided deforestation project areas as polygons to the map
m <- m %>%
  addPolygons(data = all_avoided_def_sf,
              fillColor = "#76C0EF", # Custom color for visibility
              color = "#000000", # Border color
              weight = 1,
              fillOpacity = 0.5,
              group = "Avoided Deforestation Projects",
              popup = ~as.character(filename)) # Assuming 'name' column exists for project names

# Add cookstove location polygons to the map
m <- m %>%
  addPolygons(data = all_cookstove_sf, 
              fillColor = "#FFFF00", # Yellow color for cookstove locations
              color = "#000000",
              weight = 1,
              fillOpacity = 0.7,
              group = "Cookstove Locations",
              popup = ~as.character(filename)) # Assuming 'location_name' column exists


# Add layers control
m <- m %>%
  addLayersControl(overlayGroups = c(paste0(buffer_distances, "m Forest Cover"), "Avoided Deforestation Projects", "Cookstove Locations"),
                   options = layersControlOptions(collapsed = FALSE))

# Display the map
m


# Example of intersecting forest cover with a specific buffer (e.g., the first buffer)
# Ensure all spatial objects are in the same CRS before this step
forest_in_first_buffer <- st_intersection(st_buffer(all_cookstove_sf, dist = buffer_distances[1]), forest_cover_sf)



# Add forest cover areas as polygons to the map
m <- m %>%
  addPolygons(data = forest_cover_sf,
              fillColor = "#228B22", # Forest green
              color = "#006400", # Dark green border
              weight = 1,
              fillOpacity = 0.5,
              group = "Forest Cover",
              popup = ~as.character(name)) # Assuming 'name' column exists for forest names



