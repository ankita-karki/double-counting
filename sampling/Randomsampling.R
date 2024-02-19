#Installing packages
install.packages("readxl")
install.packages("openxlxs")

# Loading necessary libraries
library(readxl)
library(dplyr)
library(openxlxs)

# Specifying the path to the Excel file
excel_path <- "D:/Thesis/Excel/Cookstoves_VMR0006.xlsx" # Update this with the actual path to your Excel file

sheets_to_read <- c("Africa", "Asia", "LatinAmerica")

# Initialize an empty list to store data frames
list_of_dataframes <- list()

for (sheet_name in sheets_to_read) {
  sheet_data <- read_excel(excel_path, sheet = sheet_name)
  
  if("Sampling survey sheet" %in% colnames(sheet_data)) {
    filtered_data <- sheet_data %>%
      filter(`Sampling survey sheet` == 'Yes') %>%
      # Ensuring all 'Comments' columns are of the same type, converting to character
      mutate(Comments = as.character(Comments))
    
    list_of_dataframes[[sheet_name]] <- filtered_data
  }
}
# Combining all data frames in the list into a single data frame
combined_data <- bind_rows(list_of_dataframes)

# We perform random sampling within each region samples
# 3 projects from each region is sampled
random_sampled_projects <- combined_data %>%
  group_by(Region) %>%
  sample_n(size = 3, replace = TRUE) %>%
  ungroup()

# Structure of the randomly sampled dataframe
print(random_sampled_projects)

# Write the randomly sampled projects to a CSV file
write.csv(random_sampled_projects, "Randomly_Sampled_Projects_With_Survey.csv", row.names = FALSE)

