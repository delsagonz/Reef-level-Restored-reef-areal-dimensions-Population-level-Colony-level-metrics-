# === Load Libraries ===
library(tidyverse)    
library(patchwork)

# === Load and Clean Data ===
eco_data <- read.csv("Eco_SiteData.csv", stringsAsFactors = FALSE) %>%
  mutate(
    SURVEY_DATE = as.Date(SURVEY_DATE, format = "%m/%d/%Y"),
    SiteWide_Eco_Footprint_M2 = as.numeric(replace(SiteWide_Eco_Footprint_M2, SiteWide_Eco_Footprint_M2 %in% c("-", "----", "IF TIME ALLOWS"), NA)),
    Differenced_RugosityMean = as.numeric(replace(Differenced_RugosityMean, Differenced_RugosityMean %in% c("-", "----", "IF TIME ALLOWS"), NA)),
    TIMEPOINT = factor(TIMEPOINT, levels = c("Post-Outplant", "Brain/Star Post-OP", "Three month", "One Year"))
  ) %>%
  filter(!is.na(SiteWide_Eco_Footprint_M2))

species_info <- read.csv("Species_Transition_Info.csv", stringsAsFactors = FALSE) %>%
  mutate(Species_Transition = paste(Post_Ouplant_Species, "to", Final_TimePoint_Species))

eco_data <- eco_data %>%
  left_join(species_info, by = c("PLOT_ID" = "SITE_ID"))

# Correct PLOT_ID label for Z2-15-FR to Z1-15-FR
eco_data <- eco_data %>%
  mutate(PLOT_ID = ifelse(PLOT_ID == "Z2-15-FR", "Z1-15-FR", PLOT_ID))


# === Define Site Groups ===
replanted_sites <- c("Z2-07-FR", "Z1-15-FR", "Z2-31-RC")

eco_data <- eco_data %>%
  mutate(SITE_GROUP = ifelse(PLOT_ID %in% replanted_sites, "Re-Outplanted", "Non Re-Outplanted"))


# === Custom Fill Column for Sites ===
eco_data <- eco_data %>%
  mutate(Custom_Fill = case_when(
    TIMEPOINT == "Brain/Star Post-OP" ~ "Brain_Post",
    SITE_GROUP == "Re-Outplanted" & TIMEPOINT == "Post-Outplant" & Post_Ouplant_Species == "APAL" ~ "APAL_Post",
    SITE_GROUP == "Re-Outplanted" & TIMEPOINT == "Post-Outplant" & Post_Ouplant_Species == "ACER" ~ "ACER_Post",
    TRUE ~ as.character(TIMEPOINT)
  ))
    


# === Define Color Palettes ===
replanted_colors <- c(
  "APAL_Post" = "#66c2a5",
  "ACER_Post" = "#f4a582",
  "Brain_Post" = "#cce5ff",
  "Three month" = "#3b8b6f",
  "One Year" = "#a8d5ba"
)

replanted_labels <- c(
  "APAL_Post" = "Elkhorn Post-Outplant",
  "ACER_Post" = "Staghorn Post-Outplant",
  "Brain_Post" = "Brain/Star Post-Outplant",
  "Three month" = "Three Months",
  "One Year" = "One Year"
)

nonre_colors <- c(
  "Post-Outplant" = "#66c2a5",
  "Three month" = "#a8d5ba",
  "One Year" = "#a8d5ba",
"Brain_Post" = "#cce5ff")

nonre_labels <- c(
  "Post-Outplant" = "Elkhorn Post-Outplant",
  "Three month" = "Three Months",
  "One Year" = "Elkhorn One Year",
  "Brain_Post" = "Brain/Star Post-Outplant"
)


# === Re-Outplanted Plot ===
plot_replanted <- ggplot(
  filter(eco_data, SITE_GROUP == "Re-Outplanted"),
  aes(x = TIMEPOINT, y = SiteWide_Eco_Footprint_M2, fill = Custom_Fill)
) +
  geom_col(position = position_dodge(width = 1), width = 1) +
  geom_text(aes(label = round(SiteWide_Eco_Footprint_M2, 0)),
            position = position_dodge(width = 0.8), vjust = -0.5,
            size = 3.5, color = "black", fontface = "italic") +
  scale_fill_manual(values = replanted_colors, labels = replanted_labels, name = NULL) +
  facet_wrap(~ PLOT_ID, nrow = 1) +
  coord_cartesian(ylim = c(0, max(filter(eco_data, SITE_GROUP == "Re-Outplanted")$SiteWide_Eco_Footprint_M2, na.rm = TRUE) * 1.2)) +
  labs(
    title = "Re-Planted Plots",
    subtitle = expression(
      "At these plots etiher species of "*italic("Acropora sp.")*" (Elkhorn or Staghorn) were outplanted initially & after complete mortality Brain & Star sp. were later outplanted."
    ),
    y = "Ecological Footprint (m²)"
  ) +
  theme_minimal(base_size = 14, base_family = "serif") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 11, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 10.5, face = "italic"),
    strip.background = element_rect(fill = "gray90", color = "gray50"),
    panel.border = element_rect(color = "black", fill = NA),
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    legend.position = "right"
  )


# === Non Re-Outplanted Plot ===
#ordering graphs
eco_data <- eco_data %>%
  mutate(PLOT_ID = factor(PLOT_ID, levels = c("Z1-22a-RC", "Z1-22-RC", "Z3-09-RC", "Z4-12-RC")))

plot_non_replanted <- ggplot(
  filter(eco_data, SITE_GROUP == "Non Re-Outplanted"),
  aes(x = TIMEPOINT, y = SiteWide_Eco_Footprint_M2, fill = Custom_Fill)
) +
  geom_col(position = position_dodge(width = 1), width = 1) +
  geom_text(aes(label = round(SiteWide_Eco_Footprint_M2, 0)),
            position = position_dodge(1), vjust = -0.5,
            size = 3.5, color = "black", fontface = "italic") +
  scale_fill_manual(values = nonre_colors, labels = nonre_labels, name = NULL)+
  facet_wrap(~ PLOT_ID, nrow = 1) +
  coord_cartesian(ylim = c(0, max(filter(eco_data, SITE_GROUP == "Non Re-Outplanted")$SiteWide_Eco_Footprint_M2, na.rm = TRUE) * 1.2)) +
  labs(
    title = "Non Re-Planted Plots",
    subtitle = expression("These plots only had "*italic("Acropora palmata")*" (Elkhorn) outplanted and were not replanted at after complete mortality. Site 22a is an exception as this plot is a newly installed plot."),
    y = "Ecological Footprint (m²)"
  ) +
  theme_minimal(base_size = 14, base_family = "serif") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 11, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 10.5, face = "italic"),
    strip.background = element_rect(fill = "gray90", color = "gray50"),
    panel.border = element_rect(color = "black", fill = NA),
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    legend.position = "right"
  )




# === Combine Plots ===
combined_eco_plot <- (
  plot_replanted +
    plot_annotation(
      title = "Re-Planted Plots",
      subtitle = expression(
        "At these plots there was "*italic("Acropora sp.")*"outplanted initially and after complete mortality, Brain & Star sp. were outplanted.\n"*
          "Post-Outplant = Immediate Post of "*italic("ACER/APAL")*" footprint compared to the Brain/Stars Immediate Post-Outplanting footprint."
      ),
      theme = theme(
        plot.title = element_text(hjust = 0.5, size = 11, face = "bold", family = "serif"),
        plot.subtitle = element_text(hjust = 0.5, size = 10.5, face = "italic", family = "serif")
      )
    )
) /
  (
    plot_non_replanted +
      plot_annotation(
        title = "Non Re-Planted Plots",
        subtitle = expression("These plots only had "*italic("Acropora palmata")*" (Elkhorn) outplanted and were not replanted at after mortality."),
        theme = theme(
          plot.title = element_text(hjust = 0.5, size = 11, face = "bold", family = "serif"),
          plot.subtitle = element_text(hjust = 0.5, size = 10.5, face = "italic", family = "serif")
        )
      )
  ) +
  plot_annotation(
    title = "Plot Ecological Footprint Summary",
    theme = theme(
      plot.title = element_text(
        hjust = 0.4,
        size = 18,
        face = "plain",
        family = "serif"
      ),
      plot.title.position = "plot",  # ensures full-width title centering
      plot.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
    )
  )


# === Display Plot ===
print(combined_eco_plot)

# === Save Output ===
ggsave(
  filename = "Plot_Ecological_Footprint_Summary.png",
  plot = combined_eco_plot,
  width = 14,        # adjust as needed for spacing
  height = 9,        # adjust as needed for vertical space
  dpi = 300,
  units = "in",
  bg = "white"
)
