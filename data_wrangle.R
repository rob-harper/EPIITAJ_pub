library(rvc)
library(tidyverse)



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
