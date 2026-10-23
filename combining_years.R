
## Matching years

# Find all the .rds files in your folder
file_paths <- list.files("combined_data", pattern = "\\.rds$", full.names = TRUE)

# Read all files into a single list
all_years_list <- map(file_paths, readRDS)

# Extract, clean types, and bind into massive data frames
Florida_99_24_matching <- list(

  sample_data = map_dfr(all_years_list, function(rvc_obj) {
    rvc_obj$sample_data %>%
      # Force PRIMARY_SAMPLE_UNIT and STATION_NR to character
      mutate(across(any_of(c("PRIMARY_SAMPLE_UNIT", "STATION_NR")), as.character))
  }),

  stratum_data = map_dfr(all_years_list, function(rvc_obj) {
    rvc_obj$stratum_data %>%
      # Ensure no crashes if the column is missing/different
      mutate(across(any_of("PRIMARY_SAMPLE_UNIT"), as.character))
  }),

  taxonomic_data = all_years_list[[1]]$taxonomic_data
)

# Reapply the class
class(Florida_99_24_matching) <- "rvcData"



# all years
# Find all the .rds files in the new folder
file_paths <- list.files("combined&single", pattern = "\\.rds$", full.names = TRUE)

# Read all files into a single list
all_years_list <- map(file_paths, readRDS)

# Extract, clean types, and bind into massive data frames
Florida_AllYears <- list(

  sample_data = map_dfr(all_years_list, function(rvc_obj) {
    rvc_obj$sample_data %>%
      # Force PRIMARY_SAMPLE_UNIT and STATION_NR to character
      mutate(across(any_of(c("PRIMARY_SAMPLE_UNIT", "STATION_NR")), as.character))
  }),

  stratum_data = map_dfr(all_years_list, function(rvc_obj) {
    rvc_obj$stratum_data %>%
      # Ensure no crashes if the column is missing/different
      mutate(across(any_of("PRIMARY_SAMPLE_UNIT"), as.character))
  }),

  taxonomic_data = all_years_list[[1]]$taxonomic_data
)

# Reapply the class
class(Florida_AllYears) <- "rvcData"
