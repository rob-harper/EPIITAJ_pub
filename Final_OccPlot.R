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

    # -- NEW: Replicate the fill logic. Applies custom colors to Florida,
    # and leaves SEFCRI (and normal Florida years) as standard black --
    point_fill = case_when(
      REGION == "Florida" & YEAR %in% c(2001, 2002, 2003, 2005, 2007, 2009, 2011) ~ "white",
      REGION == "Florida" & YEAR == 2020 ~ "gray",
      TRUE ~ "black"
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
  # -- CHANGED: Use shape 21 for all points and map fill to point_fill --
  geom_point(aes(fill = point_fill), shape = 21, size = 3, color = "black", position = pd) +

  # -- NEW: Tells ggplot to interpret "white", "gray", and "black" as exact colors --
  scale_fill_identity() +

  theme_bw(base_size = 12) +
  scale_x_continuous(breaks = min(plot_data$YEAR):max(plot_data$YEAR)) +

  scale_linetype_manual(
    values = c("Florida" = "solid", "SEFCRI" = "dashed"),
    labels = c("Florida" = "Florida Keys &  Dry Tortugas", "SEFCRI" = "Southeast Florida")
  ) +

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
