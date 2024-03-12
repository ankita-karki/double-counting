#Loading library
library(sf)

# Function to check the validity of a KML file
check_kml_validity <- function(kml_path) {
  tryCatch({
    sf_obj <- st_read(kml_path, quiet = TRUE) %>% 
      st_make_valid()  #correct any invalid geometries 
    # Attempt to identify any invalid geometries
    if (any(!st_is_valid(sf_obj))) {
      message("Invalid geometries found in: ", kml_path)
    } else {
      message("Valid geometries in: ", kml_path)
    }
  }, error = function(e) {
    message("Error processing ", kml_path, ": ", e$message)
  })
}
###Validate KML files for different regions 

# Path to the directory containing your KML files (Asia-VCS)
cookstove_asia <- list.files(path = "D:/Thesis/KML file/ALLPROJECT/VCS_cookstove", pattern = "\\.kml$", full.names = TRUE)
redd_asia <- list.files(path = "D:/Thesis/KML file/VCS_REDD/Asia", pattern = "\\.kml$", full.names = TRUE)

# Check the validity of each KML file
lapply(cookstove_asia, check_kml_validity)
lapply(redd_asia, check_kml_validity)

########################
# Path to the directory containing your KML files (Africa-VCS)
cookstove_africa <- list.files(path = "D:/Thesis/KML file/VCS_COOKSTOVE/Africa", pattern = "\\.kml$", full.names = TRUE)
redd_africa <- list.files(path = "D:/Thesis/KML file/VCS_REDD/Africa", pattern = "\\.kml$", full.names = TRUE)

# Check the validity of each KML file
lapply(cookstove_africa, check_kml_validity)
lapply(redd_africa, check_kml_validity)

# Path to the directory containing your KML files (LatinAmerica-VCS)
cookstove_latin<- list.files(path = "./KML file/VCS_COOKSTOVE/LatinAmerica", pattern = "\\.kml$", full.names = TRUE)
redd_latin <- list.files(path = "./KML file/VCS_REDD/LatinAmerica", pattern = "\\.kml$", full.names = TRUE)
# Check the validity of each KML file
lapply(cookstove_latin, check_kml_validity)
lapply(redd_latin, check_kml_validity)

# Path to the directory containing your KML files (GS-Asia/Africa)
cookstovegs_Asia<- list.files(path = "D:/Thesis/KML file/GS_COOKSTOVE_NEW/Asia", pattern = "\\.kml$", full.names = TRUE)
cookstovegs_Africa<- list.files(path = "D:/Thesis/KML file/GS_COOKSTOVE_NEW/Africa", pattern = "\\.kml$", full.names = TRUE)
# Check the validity of each KML file
lapply(cookstovegs_Africa, check_kml_validity)
lapply(cookstovegs_Asia, check_kml_validity)


################################
# Function to check for specific geometry types within a KML file
check_kml_geometry_type <- function(kml_path) {
  tryCatch({
    sf_obj <- st_read(kml_path, quiet = TRUE)
    # Identify geometry types present in the sf object
    geom_types <- unique(st_geometry_type(sf_obj))
    
    # Check for presence of specific geometry types
    has_line <- "LINESTRING" %in% geom_types
    has_point <- "POINT" %in% geom_types
    has_geom_collection <- "GEOMETRYCOLLECTION" %in% geom_types
    
    # Report findings
    message("Geometry types in ", kml_path, ": ", paste(geom_types, collapse = ", "))
    if (has_line) {
      message("LineString geometries found in: ", kml_path)
    }
    if (has_point) {
      message("Point geometries found in: ", kml_path)
    }
    if (has_geom_collection) {
      message("GeometryCollection geometries found in: ", kml_path)
    }
    
  }, error = function(e) {
    message("Error processing ", kml_path, ": ", e$message)
  })
}

### Check for specific geometry types in KML files for different regions

# Assuming cookstove_asia and redd_asia are already defined as in your previous code

# Check geometry types of each KML file
lapply(redd_files, check_kml_geometry_type)

####

# Assuming 'sf_list' is your list of sf objects
point_projects_indices <- which(sapply(redd_sf_list, function(x) st_geometry_type(x)[1] == "POINT"))
line_projects_indices <- which(sapply(redd_sf_list, function(x) st_geometry_type(x)[1] == "LINESTRING"))

# Extract the projects that are points or lines by their indices
point_projects <- redd_sf_list[point_projects_indices]
line_projects <- redd_sf_list[line_projects_indices]

# Optionally, print or examine the projects
print(point_projects)
print(line_projects)
# Assuming redd_sf_list is your list and it has more than one element
redd_sf_list <- tail(point_projects, -1)


point_projects_indices <- which(sapply(redd_sf_list, function(x) st_geometry_type(x)[1] == "GEOMETRYCOLLECTION"))
point_projects <- redd_sf_list[point_projects_indices]
redd_sf_list <- tail(point_projects, -1)
