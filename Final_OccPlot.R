library(ggplot2)
library(dplyr)
library(rvc)

Combined_FL_SEFCRI <- list(
  sample_data = bind_rows(Florida_AllYears$sample_data, SEFCRI_AllYears$sample_data),
  stratum_data = bind_rows(Florida_AllYears$stratum_data, SEFCRI_AllYears$stratum_data),
  taxonomic_data = Florida_AllYears$taxonomic_data
)
class(Combined_FL_SEFCRI) <- "rvcData"

plot_data <- getDomainOccurrence(Combined_FL_SEFCRI, "EPI ITAJ") %>%
  mutate(
    SE = sqrt(var),
    YEAR = as.numeric(as.character(YEAR)),
    Point_Type = case_when(
      REGION == "SEFCRI" ~ "SEFCRI",
      REGION == "Florida" & YEAR %in% c(2001, 2002, 2003, 2020) ~ "Florida (Partial)",
      TRUE ~ "Florida (Combined)"
    )
  )

# Define a "dodge" width so the lines, points, and error bars shift together slightly
pd <- position_dodge(width = 0.5)

ggplot(plot_data, aes(x = YEAR, y = occurrence, group = REGION)) +

  # 1. Draw the lines first (in the background)
  geom_line(aes(linetype = REGION), linewidth = 1, color = "black", position = pd) +

  # 2. Draw the error bars second
  geom_errorbar(aes(ymin = occurrence - SE, ymax = occurrence + SE),
                width = 0.25, linewidth = 0.5, color = "black", position = pd) +

  # 3. Draw the points LAST so they sit on top of the error bars
  # Using fill = "white" ensures shape 21 looks open and blocks the lines behind it
  geom_point(aes(shape = Point_Type), size = 3, color = "black", fill = "white", position = pd) +

  theme_bw(base_size = 12) +
  scale_x_continuous(breaks = min(plot_data$YEAR):max(plot_data$YEAR)) +

  scale_linetype_manual(
    values = c("Florida" = "solid", "SEFCRI" = "dashed"),
    labels = c("Florida" = "Florida Keys &  Dry Tortugas", "SEFCRI" = "Southeast Florida")
  ) +

  # Updated Shape Mappings
  scale_shape_manual(values = c(
    "Florida (Combined)" = 16, # Solid circle naturally ignores fill
    "Florida (Partial)" = 21,  # Circle with white fill (looks hollow/open, blocks lines)
    "SEFCRI" = 16              # Solid circle naturally ignores fill
  )) +

  guides(shape = "none") +

  labs(y = "Occurrence",
       x = NULL,
    linetype = NULL
  ) +

  theme(
    panel.grid.minor.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    legend.key.width = unit(2, "cm"),
    legend.background = element_rect(color = "black", linewidth = 0.5)
  )
