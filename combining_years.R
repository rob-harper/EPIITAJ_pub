
keys_folder <- "OnlyFlaKeys"
tortugas_folder <- "OnlyDryTort"


# 2. Locate and read all .rds files
keys_files <- list.files(keys_folder, pattern = "\\.rds$", full.names = TRUE)
tortugas_files <- list.files(tortugas_folder, pattern = "\\.rds$", full.names = TRUE)

all_files <- c(keys_files, tortugas_files)
all_rvc_objects <- map(all_files, readRDS)

# 3. Combine and clean the sample_data
combined_sample_data <- map_dfr(all_rvc_objects, function(rvc_obj) {
  rvc_obj$sample_data %>%
    mutate(
      across(any_of(c("PRIMARY_SAMPLE_UNIT", "STATION_NR")), as.character),

      # Step A: Save the original region string (e.g., "FLA KEYS" or "DRY TORT")
      ORIG_REGION = REGION,

      # Step B: Align the off-cycle survey years first
      YEAR = case_when(
        YEAR == 2021 ~ 2020,
        YEAR == 2023 ~ 2022,
        TRUE ~ YEAR
      ),

      # Step C: Combine YEAR, PSU, and ORIG_REGION to guarantee uniqueness
      # Example output: "2006123FLA KEYS"
      PRIMARY_SAMPLE_UNIT = paste0(YEAR, PRIMARY_SAMPLE_UNIT, ORIG_REGION),

      # Step D: Unify the main region column for your Florida_AllYears plots
      REGION = "Florida"
    )
})

# 4. Combine and clean the stratum_data (NO PSU MUTATION HERE)
combined_stratum_data <- map_dfr(all_rvc_objects, function(rvc_obj) {
  rvc_obj$stratum_data %>%
    mutate(
      ORIG_REGION = REGION,

      # Shift the off-cycle survey years to match the sample_data
      YEAR = case_when(
        YEAR == 2021 ~ 2020,
        YEAR == 2023 ~ 2022,
        TRUE ~ YEAR
      ),

      # Unify the main region column
      REGION = "Florida"
    )
})

# 5. Extract universal taxonomic data
universal_taxonomic_data <- all_rvc_objects[[1]]$taxonomic_data

# 6. Construct the final rvcData master object
Florida_AllYears <- list(
  sample_data = combined_sample_data,
  stratum_data = combined_stratum_data,
  taxonomic_data = universal_taxonomic_data
)
class(Florida_AllYears) <- "rvcData"

cat("Successfully built Florida_AllYears with unique PSUs and ORIG_REGION preserved!\n")

saveRDS(Florida_AllYears, "Florida_AllYears.rds")
