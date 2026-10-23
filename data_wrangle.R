library(rvc)
library(tidyverse)

#1999

DRTO99 <- getRvcData(1999, "DRY TORT")


KEYS99 <- getRvcData(1999, "FLA KEYS")


# 1. Mutate the region column in both sample_data frames in separate lines
DRTO99$sample_data <- DRTO99$sample_data %>% mutate(REGION = "Florida")
KEYS99$sample_data <- KEYS99$sample_data %>% mutate(REGION = "Florida")

# 2. Combine (rbind) the stratum_data and mutate the region to "Florida"
combined_stratum <- bind_rows(KEYS99$stratum_data, DRTO99$stratum_data) %>%
  mutate(REGION = "Florida")

# 3. Combine the sample_data into one object
combined_sample <- bind_rows(KEYS99$sample_data, DRTO99$sample_data)

# 4. Construct the final Florida99 combined object
Florida99 <- list(
  sample_data = combined_sample,
  stratum_data = combined_stratum,
  # Taxonomic data is universal across FL regions, so we just carry one over
  taxonomic_data = KEYS99$taxonomic_data
)

# Maintain the 'rvc' class so the package functions still recognize it
class(Florida99) <- "rvcData"

# 5. Create the output folder (if it doesn't already exist) and save the file
if (!dir.exists("combined_data")) {
  dir.create("combined_data")
}

# Save as an .rds file (the best format for saving individual R objects)
saveRDS(Florida99, file = "combined_data/Florida99.rds")

#2000

DRTO00 <- getRvcData(2000, "DRY TORT")


KEYS00 <- getRvcData(2000, "FLA KEYS")


# 1. Mutate the region column in both sample_data frames in separate lines
DRTO00$sample_data <- DRTO00$sample_data %>% mutate(REGION = "Florida")
KEYS00$sample_data <- KEYS00$sample_data %>% mutate(REGION = "Florida")

# 2. Combine (rbind) the stratum_data and mutate the region to "Florida"
combined_stratum <- bind_rows(KEYS00$stratum_data, DRTO00$stratum_data) %>%
  mutate(REGION = "Florida")

# 3. Combine the sample_data into one object
combined_sample <- bind_rows(KEYS00$sample_data, DRTO00$sample_data)

# 4. Construct the final Florida99 combined object
Florida00 <- list(
  sample_data = combined_sample,
  stratum_data = combined_stratum,
  # Taxonomic data is universal across FL regions, so we just carry one over
  taxonomic_data = KEYS00$taxonomic_data
)

# Maintain the 'rvc' class so the package functions still recognize it
class(Florida00) <- "rvcData"

# 5. Create the output folder (if it doesn't already exist) and save the file
if (!dir.exists("combined_data")) {
  dir.create("combined_data")
}

# Save as an .rds file (the best format for saving individual R objects)
saveRDS(Florida00, file = "combined_data/Florida00.rds")


## attempt at automation
#data grab
years <- c(2014, 2016, 2018)
raw_data <- list()

for (yr in years) {
  # Standardize year tag (e.g., "04", "06")
  yr_tag <- sprintf("%02d", yr %% 100)
  cat("Fetching data for year:", yr, "...\n")

  raw_data[[yr_tag]] <- list(
    DRTO = getRvcData(yr, "DRY TORT"),
    KEYS = getRvcData(yr, "FLA KEYS")
  )
}

# actual combine after manual check to make sure strats are different

# Create output directory if needed
if (!dir.exists("combined_data")) {
  dir.create("combined_data")
}

for (yr in years) {
  yr_tag <- sprintf("%02d", yr %% 100)

  # Copy raw objects to avoid modifying the original downloads
  drto_obj <- raw_data[[yr_tag]]$DRTO
  keys_obj <- raw_data[[yr_tag]]$KEYS

  # 1. Mutate region column in sample_data
  drto_obj$sample_data <- drto_obj$sample_data %>% mutate(REGION = "Florida")
  keys_obj$sample_data <- keys_obj$sample_data %>% mutate(REGION = "Florida")

  # 2. Combine stratum_data and mutate region
  combined_stratum <- bind_rows(keys_obj$stratum_data, drto_obj$stratum_data) %>%
    mutate(REGION = "Florida")

  # 3. Combine sample_data
  combined_sample <- bind_rows(keys_obj$sample_data, drto_obj$sample_data)

  # 4. Construct final combined list object
  florida_obj <- list(
    sample_data = combined_sample,
    stratum_data = combined_stratum,
    taxonomic_data = keys_obj$taxonomic_data
  )
  class(florida_obj) <- "rvcData"

  # Assign dynamically to variable name (e.g., Florida04, Florida06)
  var_name <- paste0("Florida", yr_tag)
  assign(var_name, florida_obj)

  # 5. Save .rds file
  saveRDS(florida_obj, file = paste0("combined_data/", var_name, ".rds"))
  cat("Successfully created and saved:", var_name, "\n")
}


## pt 2
#2024

DRTO24 <- getRvcData(2024, "DRY TORT")


KEYS24 <- getRvcData(2024, "FLA KEYS")


# 1. Mutate the region column in both sample_data frames in separate lines
DRTO24$sample_data <- DRTO24$sample_data %>% mutate(REGION = "Florida")
KEYS24$sample_data <- KEYS24$sample_data %>% mutate(REGION = "Florida")

# 2. Combine (rbind) the stratum_data and mutate the region to "Florida"
combined_stratum <- bind_rows(KEYS24$stratum_data, DRTO24$stratum_data) %>%
  mutate(REGION = "Florida")

# 3. Combine the sample_data into one object
combined_sample <- bind_rows(KEYS24$sample_data, DRTO24$sample_data)

# 4. Construct the final Florida99 combined object
Florida24 <- list(
  sample_data = combined_sample,
  stratum_data = combined_stratum,
  # Taxonomic data is universal across FL regions, so we just carry one over
  taxonomic_data = KEYS24$taxonomic_data
)

# Maintain the 'rvc' class so the package functions still recognize it
class(Florida24) <- "rvcData"

# 5. Create the output folder (if it doesn't already exist) and save the file
if (!dir.exists("combined_data")) {
  dir.create("combined_data")
}

# Save as an .rds file (the best format for saving individual R objects)
saveRDS(Florida24, file = "combined_data/Florida24.rds")


#single florida years

years <- c(2001, 2002, 2003, 2005, 2007, 2009, 2011, 2022)
raw_data <- list()

for (yr in years) {
  # Standardize year tag (e.g., "01", "22")
  yr_tag <- sprintf("%02d", yr %% 100)
  cat("Fetching data for year:", yr, "...\n")

  raw_data[[yr_tag]] <- list(
    KEYS = getRvcData(yr, "FLA KEYS")
  )
}

# --- Inspection step can happen here ---

# Create output directory if needed
if (!dir.exists("combined&single")) {
  dir.create("combined&single")
}

for (yr in years) {
  yr_tag <- sprintf("%02d", yr %% 100)

  # Copy raw object to avoid modifying the original downloads
  keys_obj <- raw_data[[yr_tag]]$KEYS

  # 1. Mutate region column in sample_data
  keys_obj$sample_data <- keys_obj$sample_data %>%
    mutate(REGION = "Florida")

  # 2. Mutate region column in stratum_data
  keys_obj$stratum_data <- keys_obj$stratum_data %>%
    mutate(REGION = "Florida")

  # 3. Construct final list object
  florida_obj <- list(
    sample_data = keys_obj$sample_data,
    stratum_data = keys_obj$stratum_data,
    taxonomic_data = keys_obj$taxonomic_data
  )

  # Maintain the 'rvc' class so the package recognizes it
  class(florida_obj) <- "rvcData"

  # 4. Assign dynamically to variable name (e.g., Florida01, Florida22)
  var_name <- paste0("Florida", yr_tag)
  assign(var_name, florida_obj)

  # 5. Save .rds file to the new folder
  saveRDS(florida_obj, file = paste0("combined&single/", var_name, ".rds"))
  cat("Successfully created and saved:", var_name, "\n")
}


## Dry tort only

years <- c(2021, 2023)
raw_data <- list()

for (yr in years) {
  # Standardize year tag (e.g., "21", "23")
  yr_tag <- sprintf("%02d", yr %% 100)
  cat("Fetching data for year:", yr, "...\n")

  raw_data[[yr_tag]] <- list(
    DRTO = getRvcData(yr, "DRY TORT")
  )
}

# --- Inspection step can happen here ---

# Create output directory if needed
if (!dir.exists("combined&single")) {
  dir.create("combined&single")
}

for (yr in years) {
  yr_tag <- sprintf("%02d", yr %% 100)

  # Copy raw object to avoid modifying the original downloads
  drto_obj <- raw_data[[yr_tag]]$DRTO

  # 1. Mutate region column in sample_data
  drto_obj$sample_data <- drto_obj$sample_data %>%
    mutate(REGION = "Florida")

  # 2. Mutate region column in stratum_data
  drto_obj$stratum_data <- drto_obj$stratum_data %>%
    mutate(REGION = "Florida")

  # 3. Construct final list object
  florida_obj <- list(
    sample_data = drto_obj$sample_data,
    stratum_data = drto_obj$stratum_data,
    taxonomic_data = drto_obj$taxonomic_data
  )

  # Maintain the 'rvc' class so the package recognizes it
  class(florida_obj) <- "rvcData"

  # 4. Assign dynamically to variable name (e.g., Florida21, Florida23)
  var_name <- paste0("Florida", yr_tag)
  assign(var_name, florida_obj)

  # 5. Save .rds file to the folder
  saveRDS(florida_obj, file = paste0("combined&single/", var_name, ".rds"))
  cat("Successfully created and saved:", var_name, "\n")
}

## Dry tort only

years <- c(2013:2024)
raw_data <- list()

for (yr in years) {
  # Standardize year tag (e.g., "21", "23")
  yr_tag <- sprintf("%02d", yr %% 100)
  cat("Fetching data for year:", yr, "...\n")

  raw_data[[yr_tag]] <- list(
    DRTO = getRvcData(yr, "SEFCRI")
  )
}

# --- Inspection step can happen here ---

# Create output directory if needed
if (!dir.exists("OnlySEFCRI")) {
  dir.create("OnlySEFCRI")
}

for (yr in years) {
  yr_tag <- sprintf("%02d", yr %% 100)

  # Copy raw object to avoid modifying the original downloads
  SEFCRI_obj <- raw_data[[yr_tag]]$SEFCRI

  # 1. Mutate region column in sample_data
  SEFCRI_obj$sample_data <- drto_obj$sample_data %>%
    mutate(REGION = "Florida")

  # 2. Mutate region column in stratum_data
  drto_obj$stratum_data <- drto_obj$stratum_data %>%
    mutate(REGION = "Florida")

  # 3. Construct final list object
  florida_obj <- list(
    sample_data = drto_obj$sample_data,
    stratum_data = drto_obj$stratum_data,
    taxonomic_data = drto_obj$taxonomic_data
  )

  # Maintain the 'rvc' class so the package recognizes it
  class(florida_obj) <- "rvcData"

  # 4. Assign dynamically to variable name (e.g., Florida21, Florida23)
  var_name <- paste0("Florida", yr_tag)
  assign(var_name, florida_obj)

  # 5. Save .rds file to the folder
  saveRDS(florida_obj, file = paste0("combined&single/", var_name, ".rds"))
  cat("Successfully created and saved:", var_name, "\n")
}


library(dplyr)
library(rvc)

# ==========================================
# PART 1: Mutate 2021 -> 2020 and Append
# ==========================================

Florida21 <- readRDS("combined&single/Florida21.rds")

# Mutate 2021 year to 2020
Florida21$sample_data <- Florida21$sample_data %>% mutate(YEAR = 2020)
Florida21$stratum_data <- Florida21$stratum_data %>% mutate(YEAR = 2020)

# Check if 2020 exists to append to
if (file.exists("combined&single/Florida20.rds")) {
  Florida20 <- readRDS("combined&single/Florida20.rds")

  Florida20_Appended <- list(
    sample_data = bind_rows(
      Florida20$sample_data %>% mutate(across(any_of(c("PRIMARY_SAMPLE_UNIT", "STATION_NR")), as.character)),
      Florida21$sample_data %>% mutate(across(any_of(c("PRIMARY_SAMPLE_UNIT", "STATION_NR")), as.character))
    ),
    stratum_data = bind_rows(
      Florida20$stratum_data %>% mutate(across(any_of("PRIMARY_SAMPLE_UNIT"), as.character)),
      Florida21$stratum_data %>% mutate(across(any_of("PRIMARY_SAMPLE_UNIT"), as.character))
    ),
    taxonomic_data = Florida20$taxonomic_data
  )
  class(Florida20_Appended) <- "rvcData"
  cat("Appended mutated 2021 data to existing Florida20 data.\n")

} else {
  # If 2020 doesn't exist, 2021 just becomes the new 2020 object
  Florida20_Appended <- list(
    sample_data = Florida21$sample_data,
    stratum_data = Florida21$stratum_data,
    taxonomic_data = Florida21$taxonomic_data
  )
  class(Florida20_Appended) <- "rvcData"
  cat("Florida20.rds not found. Relabeled Florida21 to 2020 and saved as Florida20_Appended.\n")
}

saveRDS(Florida20_Appended, file = "combined&single/Florida20_Appended.rds")


# ==========================================
# PART 2: Mutate 2023 -> 2022 and Append
# ==========================================

Florida22 <- readRDS("combined&single/Florida22.rds")
Florida23 <- readRDS("combined&single/Florida23.rds")

# Mutate 2023 year to 2022
Florida23$sample_data <- Florida23$sample_data %>% mutate(YEAR = 2022)
Florida23$stratum_data <- Florida23$stratum_data %>% mutate(YEAR = 2022)

# Bind and construct object
Florida22_Appended <- list(
  sample_data = bind_rows(
    Florida22$sample_data %>% mutate(across(any_of(c("PRIMARY_SAMPLE_UNIT", "STATION_NR")), as.character)),
    Florida23$sample_data %>% mutate(across(any_of(c("PRIMARY_SAMPLE_UNIT", "STATION_NR")), as.character))
  ),
  stratum_data = bind_rows(
    Florida22$stratum_data %>% mutate(across(any_of("PRIMARY_SAMPLE_UNIT"), as.character)),
    Florida23$stratum_data %>% mutate(across(any_of("PRIMARY_SAMPLE_UNIT"), as.character))
  ),
  taxonomic_data = Florida22$taxonomic_data
)

class(Florida22_Appended) <- "rvcData"
saveRDS(Florida22_Appended, file = "combined&single/Florida22_Appended.rds")
cat("Appended mutated 2023 data to Florida22 and saved as Florida22_Appended.\n")
