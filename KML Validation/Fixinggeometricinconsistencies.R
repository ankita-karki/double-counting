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
cookstove_asia <- list.files(path = "D:/Thesis/KML file/VCS_COOKSTOVE/Asia", pattern = "\\.kml$", full.names = TRUE)
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
cookstove_latin<- list.files(path = "D:/Thesis/KML file/VCS_COOKSTOVE/LatinAmerica", pattern = "\\.kml$", full.names = TRUE)
redd_latin <- list.files(path = "D:/Thesis/KML file/VCS_REDD/LatinAmerica", pattern = "\\.kml$", full.names = TRUE)
# Check the validity of each KML file
lapply(cookstove_latin, check_kml_validity)
lapply(redd_latin, check_kml_validity)

# Path to the directory containing your KML files (GS-Asia/Africa)
cookstovegs_Asia<- list.files(path = "D:/Thesis/KML file/GS_COOKSTOVE_NEW/Asia", pattern = "\\.kml$", full.names = TRUE)
cookstovegs_Africa<- list.files(path = "D:/Thesis/KML file/GS_COOKSTOVE_NEW/Africa", pattern = "\\.kml$", full.names = TRUE)
# Check the validity of each KML file
lapply(cookstovegs_Africa, check_kml_validity)
lapply(cookstovegs_Asia, check_kml_validity)

