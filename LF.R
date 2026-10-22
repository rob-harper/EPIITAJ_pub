length_data <- Florida_AllYears$sample_data %>%
  filter(trimws(SPECIES_CD) == "EPI ITAJ") %>%
  filter(!is.na(LEN), LEN > 0, !is.na(NUM), NUM > 0) %>%
  mutate(NUM = as.integer(ceiling(NUM))) %>%
  uncount(NUM) %>%
  group_by(YEAR) %>%
  summarize(
    avg_length = mean(LEN, na.rm = TRUE),
    sd_length = sd(LEN, na.rm = TRUE),
    n_fish = n(), # This is the count we will plot on the bars
    se_length = sd_length / sqrt(n_fish)
  ) %>%
  mutate(YEAR = as.numeric(as.character(YEAR)))

# 2. Calculate the scaling coefficient for the secondary axis
# This makes the tallest bar take up roughly 60% of the maximum height of the line graph
max_len <- max(length_data$avg_length, na.rm = TRUE)
max_count <- max(length_data$n_fish, na.rm = TRUE)
coeff <- (max_len / max_count) * 0.6

# 3. Create the dual-axis plot
ggplot(length_data, aes(x = YEAR)) +

  # -- NEW: Add the background bar graph for counts --
  # We multiply n_fish by coeff so the bars stretch up into the graph's visual space
  geom_bar(aes(y = n_fish * coeff), stat = "identity",
           fill = "gray75", color = "black", alpha = 0.5, width = 0.6) +

  # -- EXISTING: Line, points, and error bars for average length --
  geom_line(aes(y = avg_length), linewidth = 1, color = "black") +
  geom_point(aes(y = avg_length), size = 3, color = "black") +
  geom_errorbar(aes(ymin = avg_length - se_length, ymax = avg_length + se_length),
                width = 0.25, linewidth = 0.5, color = "black") +

  theme_bw(base_size = 12) +

  scale_x_continuous(breaks = min(length_data$YEAR):max(length_data$YEAR)) +

  # -- NEW: Setup the dual Y-axes --
  scale_y_continuous(
    name = "Average Fork Length (cm)",
    # sec.axis reverses the math by dividing by coeff, showing true counts!
    sec.axis = sec_axis(~ . / coeff, name = "N")
  ) +

  labs(
    title = "Goliath Grouper Mean Size and Observation Counts",
    subtitle = "Florida Keys  Dry Tortugas",
    x = "Year") +
  theme(
    panel.grid.minor.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
