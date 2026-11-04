# Extract all unique years where surveys actually took place
surveyed_years <- unique(Florida_AllYears$sample_data$YEAR)

# 1. Process data
length_data <- Florida_AllYears$sample_data %>%
  filter(trimws(SPECIES_CD) == "EPI ITAJ") %>%
  filter(!is.na(LEN), LEN > 0, !is.na(NUM), NUM > 0) %>%
  mutate(NUM = as.integer(ceiling(NUM))) %>%
  uncount(NUM) %>%
  group_by(YEAR) %>%
  summarize(
    avg_length = mean(LEN, na.rm = TRUE),
    sd_length = sd(LEN, na.rm = TRUE),
    n_fish = n(),
    se_length = sd_length / sqrt(n_fish)
  ) %>%
  complete(YEAR = surveyed_years, fill = list(n_fish = 0)) %>%
  arrange(YEAR) %>%
  mutate(
    YEAR = as.factor(YEAR),

    # -- CHANGED: Use case_when to map white, gray, and black points --
    point_fill = case_when(
      as.character(YEAR) %in% c(2001, 2002, 2003, 2005, 2007, 2009, 2011) ~ "white",
      as.character(YEAR) == "2020" ~ "gray",
      TRUE ~ "black"
    )
  )

# 2. Calculate the scaling coefficient for the secondary axis
max_len <- max(length_data$avg_length, na.rm = TRUE)
max_count <- max(length_data$n_fish, na.rm = TRUE)
coeff <- (max_len / max_count) * 0.4

ggplot(length_data, aes(x = YEAR)) +

  # 1st layer: Bars
  geom_bar(aes(y = n_fish * coeff), stat = "identity",
           fill = "gray75", color = "black", alpha = 0.5, width = 0.6) +

  # 2nd layer: Text
  geom_text(aes(y = n_fish * coeff, label = n_fish),
            vjust = -1, color = "black", size = 3.5) +

  # 3rd layer: Line
  geom_line(aes(y = avg_length, group = 1), linewidth = 1, color = "black") +

  # -- MOVED: 4th layer: Error bars (now drawn BEFORE the dots) --
  geom_errorbar(aes(ymin = avg_length - se_length, ymax = avg_length + se_length),
                width = 0.25, linewidth = 0.5, color = "black") +

  # 5th layer: Dots (now drawn ON TOP of the error bars)
  geom_point(aes(y = avg_length, fill = point_fill), shape = 21, size = 3, color = "black") +

  scale_fill_identity() +

  theme_bw(base_size = 12) +

  scale_y_continuous(
    name = "Mean Size (cm)",
    sec.axis = sec_axis(~ . / coeff, name = "Number of Individuals Observed"),
    expand = expansion(mult = c(0, 0.1)),
    limits = c(0, NA)
  ) +

  theme(
    panel.grid.minor.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_blank()
  )
