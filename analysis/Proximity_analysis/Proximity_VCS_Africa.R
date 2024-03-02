#####################
################################
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
  sf_obj$filename <- basename(file)
  return(sf_obj)
}

#Reading the KML files
cookstove_files <- list.files(path = "KML file/VCS_COOKSTOVE/Africa", pattern = "\\.kml$", full.names = TRUE)
redd_files <- list.files(path = "KML file/VCS_REDD/Africa", pattern = "\\.kml$", full.names = TRUE)

#Creating list of sf object for cookstove and REDD
cookstove_sf_list <- lapply(cookstove_files, read_and_make_valid)
redd_sf_list <- lapply(redd_files, read_and_make_valid)

# Dropping it results in a loop that is finished after < 1min on my computer. 
redd_sf_list[[1]] <- redd_sf_list[[1]][1:3,]

redd_sf <- do.call(rbind, redd_sf_list)
cookstove_sf <- do.call(rbind, cookstove_sf_list)


buffer_distances <- c(5000, 10000, 15000) # Distances in meters

# Create a list to hold buffer sf objects for each distance
cookstove_buffers_list <- lapply(buffer_distances, function(distance) {
  st_buffer(cookstove_sf, dist = distance)
})

# Combine into a single sf object, setting a new column to identify the buffer distance
cookstove_buffers <- do.call(rbind, lapply(1:length(cookstove_buffers_list), function(i) {
  cbind(cookstove_buffers_list[[i]], buffer_distance = buffer_distances[i])
}))


# Initialize the leaflet map
m <- leaflet() %>%
  addProviderTiles(providers$OpenStreetMap) # Add base map tiles

# Add cookstove buffers to the map with different colors for each buffer distance
colors <- c("#FF0000", "#00FF00", "#0000FF") # Colors for 5km, 10km, 15km
names(colors) <- as.character(buffer_distances)

m <- m %>% addPolygons(data = cookstove_buffers, fillColor = ~colors[as.character(buffer_distance)],
                       color = "white", weight = 1, fillOpacity = 0.5,
                       popup = ~paste(buffer_distance/1000, "km buffer"))

# Add REDD+ project areas as polygons to the map
m <- m %>%
  addPolygons(data = redd_sf,
              fillColor = "#FFFF00",
              color = "green", # Outline color
              fillOpacity = 0.5,
              weight = 1, # Outline thickness
              popup = ~as.character(filename)) # Ensure 'Name' matches your column name for REDD+ project names


# Print the map
m

######
# Calculate distances from each REDD+ project to all cookstove buffers
# This returns a matrix of distances where each row corresponds to a REDD+ project
# and each column to a cookstove buffer
distances_matrix <- st_distance(redd_sf, cookstove_buffers)

# Find the minimum distance for each REDD+ project to the nearest cookstove buffer
nearest_distances <- apply(distances_matrix, 1, min)

# If you want to add these distances back to the REDD+ sf object
redd_sf$nearest_distance <- nearest_distances

##############
library(sf)
library(leaflet)

# Assuming cookstove_sf and redd_sf are already loaded and prepared
buffer_distances <- c(15000, 10000, 5000) # Start with the largest buffer
buffer_colors <-  c("#FF0000", "#00FF00", "#0000FF") # Assign distinct colors

# Initialize the leaflet map
m <- leaflet() %>%
  addProviderTiles(providers$OpenStreetMap) # Add base map tiles

# Loop through the buffer distances in descending order
for (i in seq_along(buffer_distances)) {
  # Create buffer for the current distance
  current_buffer <- st_buffer(cookstove_sf, dist = buffer_distances[i])
  
  # Add the buffer as a polygon layer to the map with the specified color
  m <- m %>% addPolygons(data = current_buffer, 
                         fillColor = "#FFFDD0",
                         color = buffer_colors[i], 
                         fillOpacity = 0.3, 
                         weight = 4,
                         opacity = 1,
                         popup = ~paste(buffer_distances[i] / 1000, "km buffer"))
}

# Add REDD+ project areas as polygons to the map
m <- m %>%
  addPolygons(data = redd_sf,
              fillColor = "red", # No fill for REDD+ projects
              color = "#FF0000", # RED outline color for REDD+ projects
              weight = 2,
              fillOpacity = 1,
              popup = ~as.character(filename)) # Ensure 'Name' matches your column name for REDD+ project names

# Print the map
m




# Calculate nearest distance from each REDD+ project to cookstove buffers
nearest_distances <- st_distance(redd_sf, cookstove_buffers, by_element = TRUE)

# Add this distance information to the REDD+ sf object (assuming it's in meters)
redd_sf$nearest_distance_to_cookstove <- apply(nearest_distances, 1, min) / 1000 # Convert to kilometers

# View the updated REDD+ sf object
head(redd_sf)
