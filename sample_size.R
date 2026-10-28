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
  arrange(as.numeric(as.character(YEAR))) %>%

  # Convert YEAR to character so it can hold the word "Total"
  mutate(YEAR = as.character(YEAR))

# -- NEW: Append the Total row directly to the dataframe --
table_data <- table_data %>%
  bind_rows(
    tibble(
      YEAR = "Total",
      Florida = sum(table_data$Florida, na.rm = TRUE),
      SEFCRI = sum(table_data$SEFCRI, na.rm = TRUE)
    )
  )

# 2. Create the table with lines and blanks
pub_table <- table_data %>%
  gt() %>%

  tab_header(
    title = md("**Table 1.** Survey Effort")
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

  # Replace NA values with blanks
  sub_missing(
    columns = everything(),
    missing_text = ""
  ) %>%

  # Turn on table lines
  opt_table_lines("all") %>%

  # -- NEW: Style the manually added "Total" row --
  tab_style(
    style = list(
      cell_text(weight = "bold"),
      cell_fill(color = "gray95") # Optional: Light gray background for the total row
    ),
    locations = cells_body(rows = YEAR == "Total")
  ) %>%

  # Keep the bold top/bottom borders for a clean look
  tab_options(
    table.border.top.color = "black",
    table.border.top.width = px(2),
    table.border.bottom.color = "black",
    table.border.bottom.width = px(2)
  )

# View the table
pub_table
