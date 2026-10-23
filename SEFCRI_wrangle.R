library(rvc)
library(dplyr)
library(purrr)

# ==========================================
# PART 1: Fetch and Save Individual Years
# ==========================================
target_years <- 2014:2024
folder_name <- "OnlySEFCRI"

if (!dir.exists(folder_name)) {
  dir.create(folder_name)
}

for (yr in target_years) {
  cat("Attempting to fetch SEFCRI for year:", yr, "...\n")

  raw_data <- tryCatch({
    getRvcData(yr, "SEFCRI")
  }, error = function(e) {
    cat("  -> API Error: Data not available on the server for", yr, "\n")
    return(NULL)
  })

  if (!is.null(raw_data)) {
    yr_tag <- sprintf("%02d", yr %% 100)

    # Construct object explicitly
    sefcri_obj <- list(
      sample_data = raw_data$sample_data,
      stratum_data = raw_data$stratum_data,
      taxonomic_data = raw_data$taxonomic_data
    )
    class(sefcri_obj) <- "rvcData"

    # Save the .rds file
    file_name <- paste0(folder_name, "/SEFCRI_", yr_tag, ".rds")
    saveRDS(sefcri_obj, file = file_name)
    cat("  -> Successfully constructed and saved:", file_name, "\n")
  }
}


# ==========================================
# PART 2: Mutate 2021 -> 2020 and Append
# ==========================================
cat("\nRunning 2021 -> 2020 mutation for SEFCRI...\n")

file_21 <- paste0(folder_name, "/SEFCRI_21.rds")
file_20 <- paste0(folder_name, "/SEFCRI_20.rds")

if (file.exists(file_21)) {
  SEFCRI21 <- readRDS(file_21)

  # Mutate 2021 to 2020
  SEFCRI21$sample_data <- SEFCRI21$sample_data %>% mutate(YEAR = 2020)
  SEFCRI21$stratum_data <- SEFCRI21$stratum_data %>% mutate(YEAR = 2020)

  if (file.exists(file_20)) {
    # If 2020 exists, append them
    SEFCRI20 <- readRDS(file_20)

    SEFCRI20_Appended <- list(
      sample_data = bind_rows(
        SEFCRI20$sample_data %>% mutate(across(any_of(c("PRIMARY_SAMPLE_UNIT", "STATION_NR")), as.character)),
        SEFCRI21$sample_data %>% mutate(across(any_of(c("PRIMARY_SAMPLE_UNIT", "STATION_NR")), as.character))
      ),
      stratum_data = bind_rows(
        SEFCRI20$stratum_data %>% mutate(across(any_of("PRIMARY_SAMPLE_UNIT"), as.character)),
        SEFCRI21$stratum_data %>% mutate(across(any_of("PRIMARY_SAMPLE_UNIT"), as.character))
      ),
      taxonomic_data = SEFCRI20$taxonomic_data
    )
    class(SEFCRI20_Appended) <- "rvcData"
    saveRDS(SEFCRI20_Appended, file = file_20) # Overwrite the 2020 file with the appended version
    cat("  -> Appended mutated 2021 data to existing 2020 data.\n")

  } else {
    # If 2020 doesn't exist, 2021 becomes the new 2020 file
    SEFCRI20_Appended <- list(
      sample_data = SEFCRI21$sample_data,
      stratum_data = SEFCRI21$stratum_data,
      taxonomic_data = SEFCRI21$taxonomic_data
    )
    class(SEFCRI20_Appended) <- "rvcData"
    saveRDS(SEFCRI20_Appended, file = file_20)
    cat("  -> No 2020 file found. Saved mutated 2021 data as the new 2020 file.\n")
  }

  # Delete the original 2021 file so it doesn't get double-counted in the final merge
  file.remove(file_21)
  cat("  -> Deleted original 2021 file to prevent duplication.\n")
} else {
  cat("  -> No 2021 file found. Skipping mutation.\n")
}


# ==========================================
# PART 3: Combine All Years into Master Object
# ==========================================
cat("\nCombining all SEFCRI files into a master dataset...\n")

sefcri_paths <- list.files(folder_name, pattern = "\\.rds$", full.names = TRUE)
sefcri_list <- map(sefcri_paths, readRDS)

SEFCRI_AllYears <- list(

  sample_data = map_dfr(sefcri_list, function(rvc_obj) {
    rvc_obj$sample_data %>%
      mutate(across(any_of(c("PRIMARY_SAMPLE_UNIT", "STATION_NR")), as.character))
  }),

  stratum_data = map_dfr(sefcri_list, function(rvc_obj) {
    rvc_obj$stratum_data %>%
      mutate(across(any_of("PRIMARY_SAMPLE_UNIT"), as.character))
  }),

  taxonomic_data = sefcri_list[[1]]$taxonomic_data
)

class(SEFCRI_AllYears) <- "rvcData"
cat("Successfully created SEFCRI_AllYears master object!\n")


## 2013 fix

cat("Attempting to fetch SEFCRI for year 2013...\n")

raw_data_13 <- tryCatch({
  getRvcData(2013, "SEFCRI")
}, error = function(e) {
  cat("  -> API Error: Data not available on the server for 2013\n")
  return(NULL)
})

if (!is.null(raw_data_13)) {
  # Construct object explicitly
  sefcri_13 <- list(
    sample_data = raw_data_13$sample_data,
    stratum_data = raw_data_13$stratum_data,
    taxonomic_data = raw_data_13$taxonomic_data
  )
  class(sefcri_13) <- "rvcData"

  # Save the .rds file
  saveRDS(sefcri_13, file = paste0(folder_name, "/SEFCRI_13.rds"))
  cat("  -> Successfully constructed and saved SEFCRI_13.rds\n")
}

# ==========================================
# 2. Re-combine All Years into Master Object
# ==========================================
cat("\nRebuilding SEFCRI_AllYears master dataset to include 2013...\n")

# Re-read the folder now that 2013 is in there
sefcri_paths <- list.files(folder_name, pattern = "\\.rds$", full.names = TRUE)
sefcri_list <- map(sefcri_paths, readRDS)

SEFCRI_AllYears <- list(

  sample_data = map_dfr(sefcri_list, function(rvc_obj) {
    rvc_obj$sample_data %>%
      mutate(across(any_of(c("PRIMARY_SAMPLE_UNIT", "STATION_NR")), as.character))
  }),

  stratum_data = map_dfr(sefcri_list, function(rvc_obj) {
    rvc_obj$stratum_data %>%
      mutate(across(any_of("PRIMARY_SAMPLE_UNIT"), as.character))
  }),

  taxonomic_data = sefcri_list[[1]]$taxonomic_data
)

class(SEFCRI_AllYears) <- "rvcData"
cat("Successfully updated SEFCRI_AllYears master object!\n")
