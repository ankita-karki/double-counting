# Load necessary libraries
library(sf)
library(dplyr)
library(leaflet)
library(ggplot2)
library(mapview)

# Paths to specific KML files for cookstove and REDD
cookstove_file <- "D:/Thesis/KML file/VCS_COOKSTOVE/Asia/2409.kml"
redd_file <- "D:/Thesis/KML file/VCS_REDD/Asia/1650.kml"

# Read and validate a single KML file
read_and_make_valid <- function(file) {
  sf_obj <- st_read(file, quiet = TRUE)
  sf_obj <- st_make_valid(sf_obj)
  
  # Drop Z and M dimensions if present
  sf_obj <- st_zm(sf_obj, what = "ZM")
  
  return(sf_obj)
}

# Read the specific KML files
cookstove_sf <- read_and_make_valid(cookstove_file)
redd_sf <- read_and_make_valid(redd_file)

# Transform geometries to a common CRS (WGS 84)
cookstove_sf <- st_transform(cookstove_sf, 4326)
redd_sf <- st_transform(redd_sf, 4326)

# Convert POLYGON to MULTIPOLYGON if necessary
cookstove_sf <- if ("POLYGON" %in% st_geometry_type(cookstove_sf)) {
  st_cast(cookstove_sf, "MULTIPOLYGON")
} else {
  cookstove_sf
}

redd_sf <- if ("POLYGON" %in% st_geometry_type(redd_sf)) {
  st_cast(redd_sf, "MULTIPOLYGON")
} else {
  redd_sf
}

# Perform the overlap analysis
tryCatch({
  overlap_sf <- st_intersection(cookstove_sf, redd_sf)
}, error = function(e) {
  print(e)
  overlap_sf <- st_union(cookstove_sf, redd_sf)
  # Note: st_union does not necessarily give you the intersection
})

# The rest of the code for visualization or further analysis goes here
# ... (Assuming the previous code for reading and processing the KML files)

library(leaflet)

# Assuming 'all_cookstove_sf' and 'all_redd_sf' are the combined 'sf' objects for all cookstove and REDD+ projects, respectively
# And 'overlap_sf' is the 'sf' object for the overlapping regions

# Create a palette that will be used to color the polygons
palette <- colorFactor(c("blue", "red", "purple"), domain = NULL)

leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addPolygons(data = cookstove_sf, fillColor = ~palette("cookstove"), fillOpacity = 0.5, weight = 1, color = "#FFFFFF") %>%
  addPolygons(data = redd_sf, fillColor = ~palette("redd"), fillOpacity = 0.5, weight = 1, color = "#00FF00") %>%
  addPolygons(data = overlap_sf, fillColor = ~palette("overlap"), fillOpacity = 0.5, weight = 1, color = "#000000") %>%
  addLegend(pal = palette, values = c("cookstove", "redd", "overlap"), opacity = 0.5, title = "Legend", position = "bottomright")
