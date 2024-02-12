# Load required libraries
library(sf)
library(dplyr)
library(leaflet)
library(ggplot2)

# Function to read KML files and convert to sf objects
read_kml <- function(file) {
  sf_obj <- st_read(file, quiet = TRUE) %>%
    st_transform(crs = 4326) %>% # Transform to WGS 84 CRS
    st_make_valid() # Fix invalid geometries
  return(sf_obj)
}

# Function to perform overlap analysis between two lists of sf objects
perform_overlap_analysis <- function(sf_list1, sf_list2) {
  overlaps <- vector("list", length(sf_list1))
  for (i in seq_along(sf_list1)) {
    for (j in seq_along(sf_list2)) {
      overlaps[[i]] <- st_intersection(sf_list1[[i]], sf_list2[[j]])
    }
  }
  return(do.call(rbind, overlaps))
}

# Function to plot the results using leaflet
plot_results <- function(base_sf, overlay_sf, overlap_sf) {
  leaflet() %>%
    addProviderTiles(providers$Stamen.TonerLite) %>%
    addPolygons(data = base_sf, fillColor = "blue", fillOpacity = 0.5, weight = 1) %>%
    addPolygons(data = overlay_sf, fillColor = "red", fillOpacity = 0.5, weight = 1) %>%
    addPolygons(data = overlap_sf, fillColor = "purple", fillOpacity = 0.7, weight = 2) %>%
    addLegend("bottomright", pal = colorNumeric(palette = c("blue", "red", "purple"), domain = NULL), values = ~geometryType, title = "Legend")
}

# Paths to directories containing KML files (update these to your directories)
cookstove_dir <- "D:/Thesis/KML file/VCS_COOKSTOVE/Asia"
redd_dir <- "D:/Thesis/KML file/VCS_REDD/Asia"

# Read all KML files from the directories
cookstove_files <- list.files(cookstove_dir, pattern = "\\.kml$", full.names = TRUE)
redd_files <- list.files(redd_dir, pattern = "\\.kml$", full.names = TRUE)

# Load and validate geometries
cookstove_sf_list <- lapply(cookstove_files, read_kml)
redd_sf_list <- lapply(redd_files, read_kml)

# Perform overlap analysis
overlap_sf <- perform_overlap_analysis(cookstove_sf_list, redd_sf_list)

# Plot the results
plot_results(do.call(rbind, cookstove_sf_list), do.call(rbind, redd_sf_list), overlap_sf)


#Reading the KML files
cookstove_files <- list.files(path = "D:/Thesis/KML file/VCS_COOKSTOVE/Asia/", pattern = "\\.kml$", full.names = TRUE)
redd_files <- list.files(path = "D:/Thesis/KML file/VCS_REDD/Asia", pattern = "\\.kml$", full.names = TRUE)
