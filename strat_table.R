library(dplyr)
library(gt)

# 1. Summarize surveys by Year, Sub-region, and Stratum
strata_samples_table_data <- Florida_AllYears$sample_data %>%
  mutate(
    # Classify location based on the DRTO prefix
    Sub_Region = if_else(
      grepl("^DRTO", STRAT, ignore.case = TRUE),
      "Dry Tortugas",
      "Florida Keys"
    ),
    YEAR = as.numeric(as.character(YEAR))
  ) %>%
  group_by(YEAR, Sub_Region, STRAT) %>%
  summarize(
    n_samples = n_distinct(PRIMARY_SAMPLE_UNIT),
    .groups = "drop"
  ) %>%
  arrange(YEAR, Sub_Region, STRAT)

# 2. Build the publication-ready gt table
strata_summary_table <- strata_samples_table_data %>%
  gt() %>%
  tab_header(
    title = md("**Survey Effort by Stratum**"),
    subtitle = "Number of unique primary sampling units (PSUs) surveyed per year and stratum"
  ) %>%
  cols_label(
    YEAR = "Year",
    Sub_Region = "Sub-Region",
    STRAT = "Stratum",
    n_samples = "Number of Samples"
  ) %>%
  cols_align(
    align = "center",
    columns = c(YEAR, Sub_Region, STRAT, n_samples)
  ) %>%
  opt_table_lines("all") %>%
  tab_options(
    table.border.top.color = "black",
    table.border.top.width = px(2),
    table.border.bottom.color = "black",
    table.border.bottom.width = px(2)
  )

# Display table
strata_summary_table
