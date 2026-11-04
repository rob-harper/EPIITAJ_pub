
combined_summary <- bind_rows(
  SEFCRI_AllYears$sample_data,
  Florida_AllYears$sample_data
) %>%
  group_by(YEAR, PRIMARY_SAMPLE_UNIT) %>%
  summarise(
    LAT = mean(LAT_DEGREES, na.rm = TRUE),
    LON = mean(LON_DEGREES, na.rm = TRUE),
    GOLIATH_COUNT = ceiling(sum(NUM[SPECIES_CD == "EPI ITAJ"], na.rm = TRUE)),
    .groups = "drop"
  )

write.csv(combined_summary, "combined_goliath_summary.csv", row.names = FALSE)

