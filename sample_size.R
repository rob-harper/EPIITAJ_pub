library(dplyr)
library(tidyr)
library(gt)

# 1. Prepare the data by counting unique PSUs for each specific sub-region
table_data_raw <- Combined_FL_SEFCRI$sample_data %>%
  # Fallback to REGION if ORIG_REGION is missing (which is likely true for SEFCRI)
  mutate(
    ORIG = coalesce(ORIG_REGION, REGION),
    # Standardize names so we can pivot cleanly
    ORIG = case_when(
      grepl("KEYS", toupper(ORIG)) ~ "Keys",
      grepl("TORT", toupper(ORIG)) ~ "Tortugas",
      TRUE ~ "SEFCRI"
    )
  ) %>%
  group_by(YEAR, ORIG) %>%
  summarize(Sample_Size = n_distinct(PRIMARY_SAMPLE_UNIT), .groups = "drop") %>%

  # Pivot into separate columns for Keys, Tortugas, and SEFCRI
  pivot_wider(names_from = ORIG, values_from = Sample_Size) %>%
  arrange(as.numeric(as.character(YEAR))) %>%
  mutate(YEAR = as.character(YEAR))

# 2. Append the Total row using the raw numeric columns
table_data_raw <- table_data_raw %>%
  bind_rows(
    tibble(
      YEAR = "Total",
      Keys = sum(table_data_raw$Keys, na.rm = TRUE),
      Tortugas = sum(table_data_raw$Tortugas, na.rm = TRUE),
      SEFCRI = sum(table_data_raw$SEFCRI, na.rm = TRUE)
    )
  )

# 3. Format the Florida column with the "Keys / Tortugas" slash format
table_data_final <- table_data_raw %>%
  mutate(
    # If both are NA (no Florida surveys that year), leave it NA so gt() makes it blank.
    # Otherwise, paste them together, substituting 0 if one region wasn't surveyed but the other was.
    Florida = if_else(
      is.na(Keys) & is.na(Tortugas),
      NA_character_,
      paste0(replace_na(Keys, 0), " / ", replace_na(Tortugas, 0))
    )
  ) %>%
  # Keep only the columns we want to display
  select(YEAR, Florida, SEFCRI)

# 4. Create the formatted table
pub_table <- table_data_final %>%
  gt() %>%

  tab_header(
    title = md("**Table 1.** Survey Effort")
  ) %>%

  cols_label(
    YEAR = "Year",
    Florida = "Florida Keys / Dry Tortugas",
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

  opt_table_lines("all") %>%

  # Style the manually added "Total" row
  tab_style(
    style = list(
      cell_text(weight = "bold"),
      cell_fill(color = "gray95")
    ),
    locations = cells_body(rows = YEAR == "Total")
  ) %>%

  tab_options(
    table.border.top.color = "black",
    table.border.top.width = px(2),
    table.border.bottom.color = "black",
    table.border.bottom.width = px(2)
  )

# View the table
pub_table
