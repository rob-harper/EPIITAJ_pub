# Find all the .rds files in your folder
file_paths <- list.files("combined_data", pattern = "\\.rds$", full.names = TRUE)

# Read all files into a single list
all_years_list <- map(file_paths, readRDS)

# Extract and bind all the sample_data and stratum_data into massive data frames
Florida_AllYears <- list(
  sample_data = map_dfr(all_years_list, "sample_data"),
  stratum_data = map_dfr(all_years_list, "stratum_data"),
  taxonomic_data = all_years_list[[1]]$taxonomic_data # Taxonomy is universal
)

# Reapply the class
class(Florida_AllYears) <- "rvcData"
