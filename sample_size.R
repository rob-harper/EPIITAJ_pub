
library(dplyr)
library(tidyr)
library(gt)
# 1. Prepare the data by counting unique PSUs
table_data <- Combined_FL_SEFCRI$sample_data %>%
  # Note: We removed the species filter here so you get the true total
  # number of PSUs surveyed that year, not just the ones with Goliaths.
  group_by(YEAR, REGION) %>%
  summarize(Sample_Size = n_distinct(PRIMARY_SAMPLE_UNIT), .groups = "drop") %>%

  # Pivot the data (we removed values_fill = 0 so missing years become NA)
  pivot_wider(names_from = REGION, values_from = Sample_Size) %>%

  # Ensure years are sorted chronologically
  arrange(as.numeric(as.character(YEAR)))

# 2. Create the table with lines, blanks, and a grand total row
pub_table <- table_data %>%
  gt() %>%

  tab_header(
    title = md("**Table 1.** Survey Effort"),
    subtitle = "Number of unique PSUs sampled by region across all years."
  ) %>%

  cols_label(
    YEAR = "Year",
    Florida = "Florida Keys & Dry Tortugas",
    SEFCRI = "Southeast Florida"
  ) %>%

  cols_align(
    align = "center",
    columns = everything()
  ) %>%

  # -- NEW: Add a total row at the bottom for the regions --
  grand_summary_rows(
    columns = c(Florida, SEFCRI),
    fns = list("Total" = ~sum(., na.rm = TRUE)),
    formatter = fmt_number,
    decimals = 0
  ) %>%

  # Replace NA values with blanks
  sub_missing(
    columns = everything(),
    missing_text = ""
  ) %>%

  # Turn on table lines
  opt_table_lines("all") %>%

  # Keep the bold top/bottom borders for a clean look
  tab_options(
    table.border.top.color = "black",
    table.border.top.width = px(2),
    table.border.bottom.color = "black",
    table.border.bottom.width = px(2),
    grand_summary_row.background.color = "gray95" # Optional: light gray background for the total row
  )

# View the table
pub_table
