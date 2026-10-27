library(ggplot2)
library(dplyr)
library(tidyr) # For uncount()

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
  # Convert YEAR to a factor so years with no data are skipped entirely on the plot
  mutate(YEAR = as.factor(YEAR))

# 2. Calculate the scaling coefficient for the secondary axis
# CHANGED: 0.6 down to 0.4. The tallest bar now only reaches 40% up the graph.
max_len <- max(length_data$avg_length, na.rm = TRUE)
max_count <- max(length_data$n_fish, na.rm = TRUE)
coeff <- (max_len / max_count) * 0.4

# 3. Create the dual-axis plot
ggplot(length_data, aes(x = YEAR)) +

  # -- NEW: Add the background bar graph for counts --
  geom_bar(aes(y = n_fish * coeff), stat = "identity",
           fill = "gray75", color = "black", alpha = 0.5, width = 0.6) +

  # -- NEW: Add the text labels (N) --
  # CHANGED: vjust = 1.5 pulls the text down INSIDE the top of the bars
  geom_text(aes(y = n_fish * coeff, label = n_fish),
             vjust = -1, color = "black", size = 3.5) +

  # -- EXISTING: Line, points, and error bars for average length --
  geom_line(aes(y = avg_length, group = 1), linewidth = 1, color = "black") +
  geom_point(aes(y = avg_length), size = 3, color = "black") +
  geom_errorbar(aes(ymin = avg_length - se_length, ymax = avg_length + se_length),
                width = 0.25, linewidth = 0.5, color = "black") +

  theme_bw(base_size = 12) +

  # -- NEW: Setup the dual Y-axes with updated titles and zeroed bottom --
  scale_y_continuous(
    name = "Mean Fork Length (cm)",
    sec.axis = sec_axis(~ . / coeff, name = "Number of Individuals Observed"),
    expand = expansion(mult = c(0, 0.1)),
    limits = c(0, NA)
  ) +

  labs(
    title = "Goliath Grouper Mean Size and Observation Counts",
    x = "Year"
  ) +

  theme(
    panel.grid.minor.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
