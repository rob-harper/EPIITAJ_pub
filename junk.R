library(rvc)
library(tidyverse)


# 1. Create a broad sequence of potential survey years
possible_years <- 1990:2026

# 2. Pass that sequence into the years argument
all_strata <- getStratumData(years = possible_years, regions = c("FLA KEYS", "DRY TORT"))

# 3. View the unique years available for the Florida Keys
keys_years <- all_strata %>%
  filter(REGION == "FLA KEYS") %>%
  pull(YEAR) %>%
  unique() %>%
  sort()

print("Available years for Florida Keys:")
print(keys_years)

# 4. View the unique years available for the Dry Tortugas
tort_years <- all_strata %>%
  filter(REGION == "DRY TORT") %>%
  pull(YEAR) %>%
  unique() %>%
  sort()

print("Available years for Dry Tortugas:")
print(tort_years)

source("FishPlots/theme_publication.R")
source("FishPlots/labeler.R")
source("FishPlots/WrapperFunctions.R")
source("FishPlots/QL_binned_LengthFreq.R")
source("FishPlots/QL_SubregionDensityPlot.R")
source("FishPlots/plot_horizontal_bar_shared_axis.R")

plot_domain_occ_by_year(Florida_AllYears, "Epi Itaj")

plot_domain_occ_by_year(Florida_99_24_matching, "Epi Itaj")

plot_domain_occ_by_year()
