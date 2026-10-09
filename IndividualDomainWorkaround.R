library(rvc)
library(dplyr)
library(purrr)

# 1. Determine available years for each region to prevent loop crashes
# You can now safely pass a broad sequence of years; the loop will just skip missing ones.
years_to_check <- 1999:2024

# Create the output folders
if (!dir.exists("OnlyFlaKeys")) dir.create("OnlyFlaKeys")
if (!dir.exists("OnlyDryTort")) dir.create("OnlyDryTort")

# ==========================================
# 1. Fetch and save Florida Keys data
# ==========================================
for (yr in years_to_check) {
  cat("Fetching FLA KEYS for year:", yr, "...\n")

  # Try to get the data; if it fails, catch the error and return NULL
  keys_data <- tryCatch({
    getRvcData(yr, "FLA KEYS")
  }, error = function(e) {
    cat("  -> No data available for FLA KEYS in", yr, "- Skipping.\n")
    return(NULL)
  })

  # If keys_data is NULL (because it errored), skip to the next year
  if (is.null(keys_data)) {
    next
  }

  # If successful, save the data
  yr_tag <- sprintf("%02d", yr %% 100)
  saveRDS(keys_data, file = paste0("OnlyFlaKeys/KEYS_", yr_tag, ".rds"))
}


# ==========================================
# 2. Fetch and save Dry Tortugas data
# ==========================================
for (yr in years_to_check) {
  cat("Fetching DRY TORT for year:", yr, "...\n")

  # Try to get the data; if it fails, catch the error and return NULL
  tort_data <- tryCatch({
    getRvcData(yr, "DRY TORT")
  }, error = function(e) {
    cat("  -> No data available for DRY TORT in", yr, "- Skipping.\n")
    return(NULL)
  })

  # If tort_data is NULL, skip to the next year
  if (is.null(tort_data)) {
    next
  }

  # If successful, save the data
  yr_tag <- sprintf("%02d", yr %% 100)
  saveRDS(tort_data, file = paste0("OnlyDryTort/DRTO_", yr_tag, ".rds"))
}

## Creating objects with all years

# ==========================================
# 1. Build the Keys_AllYears Object
# ==========================================

# Find all the .rds files in the Keys folder
keys_paths <- list.files("OnlyFlaKeys", pattern = "\\.rds$", full.names = TRUE)

# Read all files into a single list
keys_list <- map(keys_paths, readRDS)

# Extract, clean types, and bind into massive data frames
Keys_AllYears <- list(

  sample_data = map_dfr(keys_list, function(rvc_obj) {
    rvc_obj$sample_data %>%
      mutate(across(any_of(c("PRIMARY_SAMPLE_UNIT", "STATION_NR")), as.character))
  }),

  stratum_data = map_dfr(keys_list, function(rvc_obj) {
    rvc_obj$stratum_data %>%
      mutate(across(any_of("PRIMARY_SAMPLE_UNIT"), as.character))
  }),

  taxonomic_data = keys_list[[1]]$taxonomic_data
)

# Reapply the rvcData class
class(Keys_AllYears) <- "rvcData"
cat("Successfully created Keys_AllYears with", length(keys_list), "years of data.\n")


# ==========================================
# 2. Build the Tortugas_AllYears Object
# ==========================================

# Find all the .rds files in the Dry Tortugas folder
tort_paths <- list.files("OnlyDryTort", pattern = "\\.rds$", full.names = TRUE)

# Read all files into a single list
tort_list <- map(tort_paths, readRDS)

# Extract, clean types, and bind into massive data frames
Tortugas_AllYears <- list(

  sample_data = map_dfr(tort_list, function(rvc_obj) {
    rvc_obj$sample_data %>%
      mutate(across(any_of(c("PRIMARY_SAMPLE_UNIT", "STATION_NR")), as.character))
  }),

  stratum_data = map_dfr(tort_list, function(rvc_obj) {
    rvc_obj$stratum_data %>%
      mutate(across(any_of("PRIMARY_SAMPLE_UNIT"), as.character))
  }),

  taxonomic_data = tort_list[[1]]$taxonomic_data
)

# Reapply the rvcData class
class(Tortugas_AllYears) <- "rvcData"
cat("Successfully created Tortugas_AllYears with", length(tort_list), "years of data.\n")


#missing drto fix
target_years <- c(2023, 2024)

# Ensure the folder exists
if (!dir.exists("OnlyDryTort")) {
  dir.create("OnlyDryTort")
}

for (yr in target_years) {
  cat("Attempting to fetch DRY TORT for year:", yr, "...\n")

  # Try to pull the raw data from the server
  raw_data <- tryCatch({
    getRvcData(yr, "DRY TORT")
  }, error = function(e) {
    cat("  -> API Error: Data not available on the server for", yr, "\n")
    return(NULL)
  })

  if (!is.null(raw_data)) {
    yr_tag <- sprintf("%02d", yr %% 100)

    # 1. Construct the final list object explicitly
    drto_obj <- list(
      sample_data = raw_data$sample_data,
      stratum_data = raw_data$stratum_data,
      taxonomic_data = raw_data$taxonomic_data
    )

    # 2. Explicitly reapply the class
    class(drto_obj) <- "rvcData"

    # 3. Assign dynamically to a variable name (e.g., DRTO21, DRTO23)
    var_name <- paste0("DRTO", yr_tag)
    assign(var_name, drto_obj)

    # 4. Save the .rds file to the folder
    file_name <- paste0("OnlyDryTort/", var_name, ".rds")
    saveRDS(drto_obj, file = file_name)

    cat("  -> Successfully constructed and saved:", file_name, "\n")
  }
}


## Plotting both

library(ggplot2)
library(dplyr)
library(rvc)

# 1. Bind the untouched Keys and Tortugas objects into a temporary combined object
Combined_Regions <- list(
  sample_data = bind_rows(Keys_AllYears$sample_data, Tortugas_AllYears$sample_data),
  stratum_data = bind_rows(Keys_AllYears$stratum_data, Tortugas_AllYears$stratum_data),
  taxonomic_data = Keys_AllYears$taxonomic_data
)
class(Combined_Regions) <- "rvcData"

# 2. Extract the data and calculate Standard Error (SE)
plot_data <- getDomainOccurrence(Combined_Regions, "EPI ITAJ") %>%
  mutate(
    SE = sqrt(var),
    YEAR = as.factor(YEAR) # Treats years as discrete categories on the X-axis
  )

# 3. Create the single-use plot
ggplot(plot_data, aes(x = YEAR, y = occurrence, group = REGION, color = REGION)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = occurrence - SE, ymax = occurrence + SE),
                width = 0.25, size = 0.5) +
  theme_Publication(base_size = 12) + # simple, clean theme
  labs(
    title = "Goliath Grouper",
    x = "Year",
    y = "Occurrence",
    color = "Region"
  )




plot_data <- getDomainOccurrence(Combined_Regions, "EPI STRI") %>%
  mutate(
    SE = sqrt(var),
    YEAR = as.factor(YEAR)
  )


ggplot(plot_data, aes(x = YEAR, y = occurrence, group = REGION, color = REGION)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = occurrence - SE, ymax = occurrence + SE),
                width = 0.25, size = 0.5) +
  theme_Publication(base_size = 12) +
  labs(
    title = "Nas Grouper",
    x = "Year",
    y = "Occurrence",
    color = "Region"
  )
