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
cookstove_files <- list.files(path = "KML file/VCS_COOKSTOVE/Asia", pattern = "\\.kml$", full.names = TRUE)
avoided_def_files <- list.files(path = "KML file/VCS_REDD/Asia", pattern = "\\.kml$", full.names = TRUE)

#Creating list of sf object for cookstove and Avoided deforestation
cookstove_sf_list <- lapply(cookstove_files, read_and_make_valid)
avoided_def_list <- lapply(avoided_def_files, read_and_make_valid)

#Transforming all geometries to a common CRS (WGS 84)
# Define WGS84 (EPSG:4326) CRS object
#wgs84_crs <- CRS("+init=epsg:4326")
#cookstove_sf_list <- lapply(cookstove_sf_list, function(sf) st_transform(sf, 4326))
#avoided_def_list <- lapply(avoided_def_list, function(sf) st_transform(sf, 4326))

############################################
#Convert polygon to multipolygon 
#For Avoided Deforestation 
avoided_def_sf <- function(sf_list) {
  lapply(sf_list, function(sf_obj) {
    if ("POLYGON" %in% st_geometry_type(sf_obj)) {
      sf_obj <- st_cast(sf_obj, "MULTIPOLYGON")
    }
    return(sf_obj)
  })
}

# Apply the conversion to all sf objects
avoided_def_list <- avoided_def_sf(avoided_def_list)

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

#######################
#Forest land cover overlay 
#############################################
library(sf)
library(raster)
library(leaflet)
library(rgdal)

# Your existing code for initializing the map and adding buffer polygons...

# Load the forest cover raster
forest_cover <- raster("D:/Thesis/Forestcover/forestcover/AfricaLandCover2020.tif")

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


























#need to work on #
################################################

library(sf)
library(leaflet)
library(dplyr)

# Assuming all_cookstove_sf and all_avoided_def_sf are already defined sf objects
buffer_distances <- c(5000, 10000, 15000) # Distances in meters
buffer_colors <- c("#FF0000", "#00FF00", "#0000FF") # Colors for each buffer distance

# Initialize the leaflet map with OpenStreetMap tiles
m <- leaflet() %>%
  addProviderTiles(providers$OpenStreetMap)

overlapping_projects <- list()

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
  
  # Check for overlaps between current buffer and avoided deforestation projects
  overlaps <- st_intersects(current_buffer, all_avoided_def_sf, sparse = FALSE)
  
  # Find the avoided deforestation projects that overlap with the current buffer
  if(any(overlaps)) {
    overlapping_filenames <- all_avoided_def_sf$filename[apply(overlaps, 2, any)]
    overlapping_projects[[i]] <- unique(overlapping_filenames)
  }
}

# Flatten the list and remove duplicates
overlapping_projects_flat <- unique(unlist(overlapping_projects))

# Print filenames of overlapping projects
if(length(overlapping_projects_flat) > 0) {
  cat("Filenames of overlapping REDD projects:\n")
  print(overlapping_projects_flat)
} else {
  cat("No overlapping REDD projects found.\n")
}

# Add avoided deforestation project areas as polygons to the map
m <- m %>%
  addPolygons(data = all_avoided_def_sf,
              fillColor = "#000000", # Fill color for avoided deforestation projects
              color = "#000000", # Border color for avoided deforestation projects
              weight = 2,
              fillOpacity = 0.7, # Adjusted for visibility
              popup = ~as.character(filename), # Ensure 'filename' matches your column name
              group = "Avoided Deforestation")

# Add layers control to toggle visibility of each buffer layer and the avoided deforestation layer
m <- m %>%
  addLayersControl(overlayGroups = c(paste0(buffer_distances, " m Buffer"), "Avoided Deforestation"),
                   options = layersControlOptions(collapsed = FALSE))

# Print the map
m


##########################

# Assuming all_cookstove_sf and all_avoided_def_sf are predefined sf objects
buffer_distances <- c(5000, 10000, 15000) # Distances in meters

# Prepare a list to hold results
results <- vector("list", length(buffer_distances))
names(results) <- paste(buffer_distances / 1000, "km")

# Loop through each buffer distance
for (distance in buffer_distances) {
  # Apply buffer around each REDD project
  redd_buffered <- st_buffer(all_avoided_def_sf, dist = distance)
  
  # Perform intersection test
  intersection_test <- st_intersects(redd_buffered, all_cookstove_sf, sparse = FALSE)
  
  # Collect REDD project filenames that have any intersection
  intersecting_projects <- all_avoided_def_sf$filename[apply(intersection_test, 1, any)]
  
  # Store results
  results[[paste(distance / 1000, "km")]] <- intersecting_projects
}

# Print results
for (distance in names(results)) {
  cat(paste("REDD projects intersecting with any cookstove project within", distance, "buffer:\n"))
  intersecting_projects <- results[[distance]]
  if (length(intersecting_projects) > 0) {
    print(intersecting_projects)
  } else {
    cat("No intersections found.\n")
  }
  cat("\n") # For better readability
}
















# Calculate distances
min_distances <- sapply(1:nrow(all_avoided_def_sf), function(i) {
  min(st_distance(all_avoided_def_sf[i, ], all_cookstove_sf))
})


# Summary statistics for minimum distances
summary(min_distances)

# Classify based on arbitrary distance thresholds (in meters)
distance_categories <- cut(min_distances,
                           breaks = c(0, 1000, 5000, 10000, Inf),
                           labels = c("Very Close (<1km)",
                                      "Close (1km - 5km)",
                                      "Nearby (5km - 10km)",
                                      "Far (>10km)"),
                           include.lowest = TRUE)

table(distance_categories)

# Add minimum distance and category to the sf object
#all_avoided_def_sf$min_distance_to_cookstove <- min_distances
all_avoided_def_sf$distance_category <- distance_categories


library(leaflet)

#coords <- st_coordinates(st_centroid(all_avoided_def_sf))
# Initialize the leaflet map with OpenStreetMap tiles
m <- leaflet() %>%
  addProviderTiles(providers$OpenStreetMap)


# Define colors for each distance category
category_colors <- c("Very Close (<1km)" = "#FF0000", 
                     "Close (1km - 5km)" = "#FFA500",
                     "Nearby (5km - 10km)" = "#FFFF00",
                     "Far (>10km)" = "#008000")

# Map function for category to color
getColor <- function(category) {
  category_colors[category]
}

# Assuming all_avoided_def_sf already has 'distance_category'
# Update fillColor in addPolygons to use getColor for each feature's distance category
m <- leaflet() %>%
  addProviderTiles(providers$OpenStreetMap) %>%
  addPolygons(data = all_cookstove_sf, fillColor = "transparent", color = "#1E90FF", fillOpacity = 0.5, weight = 1, group = "Cookstove Projects") %>%
  addPolygons(data = all_avoided_def_sf, 
              fillColor = ~getColor(as.character(distance_category)),
              color = "#000000", weight = 1, fillOpacity = 0.7, 
              popup = ~paste("Distance Category: ", distance_category, "<br>Filename: ", filename),
              group = "Avoided Deforestation")

# Correctly add the legend
m <- m %>%
  addLegend(position = "bottomright", colors = category_colors, labels = names(category_colors), title = "Proximity to Cookstove")


m

############################################
# Calculate the distances from each avoided deforestation project to all cookstove projects
distances <- st_distance(all_avoided_def_sf, all_cookstove_sf)

# Find the minimum distance for each avoided deforestation project
all_avoided_def_sf$min_distance_to_cookstove <- apply(distances, 1, min)

# Define the threshold for close proximity
proximity_threshold <- 15000 # 15 km ??


# Filter avoided deforestation that are within close proximity to any cookstove project
redd_close_proximity <- all_avoided_def_sf[all_avoided_def_sf$min_distance_to_cookstove <= proximity_threshold, ]

# Print the names of the avoided deforestation projects in close proximity
print(redd_close_proximity$filename)

# Calculate buffered cookstove project
#buffered_cookstove <- st_buffer(all_cookstove_sf, dist = 1500)  # 500 meters buffer, adjust as needed


# Initialize the leaflet map
m <- leaflet() %>%
  addProviderTiles(providers$OpenStreetMap) # Add base map tiles

# Add buffered cookstove project to the map
m <- m %>%
  addPolygons(data = cookstove_buffers,
              fillColor = "#FF0000", # Fill color
              color = "#000000", # Outline color
              fillOpacity = 0.3, # Fill opacity
              weight = 2, # Outline weight
              popup = "Buffered Cookstove Project") # Popup content

# Add avoided deforestation projects in close proximity
m <- m %>%
  addPolygons(data = redd_close_proximity,
              fillColor = "#00FF00", # Fill color
              color = "#000000", # Outline color
              fillOpacity = 0.8, # Fill opacity
              weight = 2, # Outline weight
              popup = ~as.character(filename)) # Popup content

# Print the map with only close proximity projects
print(m)

###################

# Filter avoided deforestation that are within close proximity to any cookstove project
redd_close_proximity <- all_avoided_def_sf[all_avoided_def_sf$min_distance_to_cookstove <= proximity_threshold, ]

# Get the row indices of the cookstove projects with nearby avoided deforestation projects
nearby_cookstove_indices <- unique(which(distances <= proximity_threshold, arr.ind = TRUE)[,"row"])

# Extract the IDs of nearby cookstove projects
nearby_cookstove_ids <- all_cookstove_sf$id[nearby_cookstove_indices]

# Filter cookstove projects to include only those with nearby avoided deforestation projects
cookstove_near_ad <- all_cookstove_sf[all_cookstove_sf$id %in% nearby_cookstove_ids, ]

# Calculate the distances from each cookstove project to all avoided deforestation projects
distances <- st_distance(all_cookstove_sf, all_avoided_def_sf)




# Find the minimum distance for each cookstove project
all_cookstove_sf$min_distance_to_avoided_def <- apply(distances, 1, min)

# Identify cookstove projects with at least one avoided deforestation project nearby
cookstove_near_avoided_def <- all_cookstove_sf[all_cookstove_sf$min_distance_to_avoided_def <= proximity_threshold, ]

# Get the unique IDs of the cookstove projects with nearby avoided deforestation projects
nearby_cookstove_ids <- unique(cookstove_near_avoided_def$filename)

# Filter avoided deforestation projects that are within close proximity to identified cookstove projects
redd_close_proximity <- all_avoided_def_sf[all_avoided_def_sf$filename %in% nearby_cookstove_ids, ]

# Print the names of the avoided deforestation projects in close proximity
print(redd_close_proximity$filename)

# Assuming 'm' is your Leaflet map object already initialized
m <- m %>%
  addPolygons(data = redd_close_proximity,
              fillColor = "#00FF00", # Fill color
              color = "#000000", # Outline color
              fillOpacity = 0.8, # Fill opacity
              weight = 2, # Outline weight
              popup = ~as.character(filename)) # Popup content
m

############################
library(terra)
library(leaflet)


# Load the global forest cover raster
forest_cover <- rast("D:/Thesis/Forestcover/Africa_treecover.tif")

# Check and match the CRS of the forest cover raster to the cookstove and REDD+ projects
# Assuming that all_cookstove_sf and all_avoided_def_sf are in the WGS 84 CRS (EPSG:4326)
crs(forest_cover) <- crs(all_cookstove_sf)

# Initialize the leaflet map with base tiles
m <- leaflet() %>%
  addProviderTiles(providers$OpenStreetMap)

# Add the forest cover as a raster layer
m <- m %>% addRasterImage(forest_cover, colors = c("#228B22"), opacity = 0.7, group = "Forest Cover")

# Loop through the buffer distances and add them to the map
buffer_colors <-  c("#FF0000", "#00FF00", "#0000FF") # Distinct colors for each buffer distance
for (i in seq_along(buffer_distances)) {
  current_buffer <- cookstove_buffers_list[[i]]
  m <- m %>% addPolygons(data = current_buffer, 
                         fillColor = buffer_colors[i], 
                         fillOpacity = 0.3, 
                         color = "#000000",
                         weight = 2,
                         opacity = 1,
                         group = paste(buffer_distances[i] / 1000, "km Buffer"))
}

# Add cookstove projects to the map
m <- m %>%
  addPolygons(data = all_cookstove_sf,
              fillColor = "#0000FF",
              color = "#FFFFFF",
              fillOpacity = 0.5,
              weight = 1,
              group = "Cookstove Projects")

# Add REDD+ (Avoided Deforestation) projects to the map
m <- m %>%
  addPolygons(data = all_avoided_def_sf,
              fillColor = "#FFA500",
              color = "#FFFFFF",
              fillOpacity = 0.5,
              weight = 1,
              group = "REDD+ Projects")

# Optional: If you want to highlight the REDD+ projects within the proximity threshold
m <- m %>%
  addPolygons(data = redd_close_proximity,
              fillColor = "#FF0000",
              color = "#FFFFFF",
              fillOpacity = 0.8,
              weight = 2,
              group = "Close Proximity REDD+")

# Add layer control to switch between the layers
m <- m %>%
  addLayersControl(overlayGroups = c("Forest Cover", "Cookstove Projects", "REDD+ Projects", "Close Proximity REDD+"),
                   options = layersControlOptions(collapsed = FALSE))

# Render the map
m

#############################
library(terra)
library(sf)

# Assuming that the forest cover raster and REDD+ sf object are already loaded
forest_cover <- rast("D:/Thesis/Forestcover/SHAPEFILE.tif")
# all_redd_sf <- st_read("path/to/your/redd_data.shp")
plot(forest_cover)
# Step 1: Create a binary raster of forested areas (assuming forested areas are already represented by specific values)
# Let's assume that forested areas in your dataset are represented by the value 1
forest_binary <- ifel(forest_cover == 1, 1, 0)

# Step 2: Convert the binary raster of forested areas to polygons (this might take some time for large datasets)
forest_polygons <- as.polygons(forest_binary, values = TRUE)

# Convert SpatVector to data frame with geometry information
forest_polygons_df <- as.data.frame(forest_polygons, WHAT = "geometry")

# Read the data frame as an sf object (assuming geometry column is named "geometry")
forest_polygons_sf <- st_read(forest_polygons_df, geom = "geometry", crs = crs(all_avoided_def_sf))  # Ensure CRS consistency

# Recalculate distances with the sf object
distances_to_forest <- st_distance(all_avoided_def_sf, forest_polygons_df)



library(sf)  # Ensure sf package is loaded

# Convert SpatVector to sf object
forest_polygons_sf <- st_as_sf(forest_polygons, crs = crs(all_avoided_def_sf))  # Ensure CRS consistency

# Recalculate distances with the sf object
distances_to_forest <- st_distance(all_avoided_def_sf, forest_polygons_sf)

# If the raster is too large and the above step is not feasible, consider raster to points
# forest_points <- as.points(forest_binary, values = TRUE)
forest_polygons <- st_polygon(forest_cover)
# Step 3: Calculate the distance from each REDD+ project to the nearest forest area
# If you have polygons
distances_to_forest <- st_distance(all_avoided_def_sf, forest_polygons)
# If you used points, uncomment the following line
# distances_to_forest <- st_distance(all_redd_sf, forest_points)

# Find the minimum distance for each REDD+ project
all_redd_sf$min_distance_to_forest <- apply(distances_to_forest, 1, min)

# Add this distance information as a column in your REDD+ sf object
all_redd_sf <- all_redd_sf %>%
  mutate(min_distance_to_forest = apply(distances_to_forest, 1, min))

# Optionally, save the updated REDD+ sf object with distance information
st_write(all_redd_sf, "updated_redd_with_distance.gpkg", layer = "REDD_Distance_to_Forest", driver = "GPKG")


########################
install.packages("xml2")
library(rgdal) # For handling geospatial data
library(xml2) # For parsing XML files
library(sf)

xml_file <- read_xml("D:/Thesis/Forestcover/JRC_GFC2020_V1.xml")

# Find all polygon elements in the XML file
polygons <- xml_find_all(xml_file, ".//polygon")

# Initialize an empty list to store polygon coordinates
polygon_coords <- list()

# Loop through each polygon element
for (polygon in polygons) {
  # Extract coordinates from the polygon element
  coords <- xml_attr(xml_find_all(polygon, ".//coordinates"), "points")
  # Split coordinates into individual points
  points <- strsplit(coords, " ")
  # Convert points to a matrix
  points_matrix <- matrix(unlist(points), ncol = 2, byrow = TRUE)
  # Convert matrix to a Polygon object
  polygon_obj <- st_polygon(list(points_matrix))
  # Add polygon object to the list
  polygon_coords <- c(polygon_coords, polygon_obj)
}

# Convert the list of polygon objects to an sf object
polygons_sf <- st_sf(geometry = st_sfc(polygon_coords))

# Plot the extracted polygons
plot(polygons_sf)


