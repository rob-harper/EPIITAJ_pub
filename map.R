library(dplyr)

site_summary_with_year <- Florida_AllYears$sample_data %>%
  group_by(YEAR, PRIMARY_SAMPLE_UNIT, STATION_NR, LAT_DEGREES, LON_DEGREES) %>%
  summarise(
    # Sum the sizes together for a total count, rounding up fractions
    GOLIATH_COUNT = ceiling(sum(NUM[SPECIES_CD == "EPI ITAJ"], na.rm = TRUE)),
    .groups = "drop"
  )

# Export to CSV for ArcGIS Pro
write.csv(site_summary_with_year, "Goliath_Grouper_Counts_WithYear.csv", row.names = FALSE)


sefcri_summary <- SEFCRI_AllYears$sample_data %>%
  group_by(YEAR, PRIMARY_SAMPLE_UNIT, STATION_NR, LAT_DEGREES, LON_DEGREES) %>%
  summarise(
    # Sum the sizes together and round up diver fractions
    GOLIATH_COUNT = ceiling(sum(NUM[SPECIES_CD == "EPI ITAJ"], na.rm = TRUE)),
    .groups = "drop"
  )

# Export the SEFCRI-specific CSV for ArcGIS Pro
write.csv(sefcri_summary, "Goliath_Grouper_Counts_SEFCRI.csv", row.names = FALSE)
