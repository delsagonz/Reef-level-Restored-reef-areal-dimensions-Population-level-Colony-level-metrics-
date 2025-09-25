#Creates Abundance graphs showing each site facet wrapped and has both species with wild and restored
library(tidyverse)
library(grid)
library(lubridate)

# Load data
popdata <- read.csv("nfwf_PopulationData.csv")

# Clean and prepare data
pop_data_cleaned <- popdata %>%
  filter(CORAL_CODE %in% c("APAL", "ACER")) %>%
  filter(!is.na(CORAL_CODE), CORAL_CODE != "") %>%
  mutate(
    TIMEPOINT = factor(TIMEPOINT, levels = c("Pre-Outplant", "Post-Outplant", "Three month", "One Year")),
    Site = PLOT_ID,
    RESTORED_CORAL_DENSITY_PER_M2_APAL = na_if(as.character(RESTORED_CORAL_DENSITY_PER_M2_APAL), "-"),
    RESTORED_CORAL_DENSITY_PER_M2_APAL = as.numeric(RESTORED_CORAL_DENSITY_PER_M2_APAL),
    SURVEY_DATE = mdy(SURVEY_DATE),
    Survey_Label = format(SURVEY_DATE, "%b %Y"),
    site_prefix = str_extract(Site, "^[A-Z]+"),
    site_number = as.numeric(str_extract(Site, "[0-9]+")),
    
# Correct site label for Z2-15-FR to Z1-15-FR
    Site = ifelse(Site == "Z2-15-FR", "Z1-15-FR", Site)
  ) %>%
  filter(Site != "Z1-22a-RC")  # Filter out site 22a

# Order Site factor
pop_data_cleaned$Site <- with(pop_data_cleaned, factor(Site, levels = unique(Site[order(site_prefix, site_number)])))

# Prepare long format data
pop_long <- pop_data_cleaned %>%
  select(PLOT_ID, Site, TIMEPOINT, CORAL_CODE, RESTORED_CORAL_ABUN, WILD_RESTORED_CORAL_ABUN, Survey_Label) %>%
  pivot_longer(
    cols = c(RESTORED_CORAL_ABUN, WILD_RESTORED_CORAL_ABUN),
    names_to = "Type", values_to = "Abundance"
  ) %>%
  mutate(
    Type = case_when(
      Type == "RESTORED_CORAL_ABUN" ~ "Restored",
      Type == "WILD_RESTORED_CORAL_ABUN" ~ "Wild"
    ),
    Abundance = na_if(Abundance, "-"),
    Abundance = as.numeric(Abundance),
    CORAL_CODE = factor(CORAL_CODE, levels = c("APAL", "ACER")),
    Type = factor(Type, levels = c("Restored", "Wild")),
    Site = factor(Site, levels = levels(pop_data_cleaned$Site))
  ) %>%
  filter(!is.na(Abundance))

#Plotting
final_plot <- ggplot(pop_long, aes(x = TIMEPOINT, y = Abundance,
                                   group = interaction(CORAL_CODE, Type),
                                   color = CORAL_CODE, linetype = Type)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  facet_wrap(~ Site, ncol = 2, scales = "free_y") +
  scale_color_manual(
    values = c("APAL" = "#66c2a5", "ACER" = "#f4a582"),
    labels = c("APAL" = "Elkhorn", "ACER" = "Staghorn"),
    name = "Species"   # <-- Change legend title here
  ) +
  scale_x_discrete(labels = c(
    "Pre-Outplant" = "Pre-Outplant",
    "Post-Outplant" = "Post-Outplant",
    "Three month" = "Three Months",
    "One Year" = "One Year"
  )) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10), labels = scales::number_format(accuracy = 1)) +
  theme_bw(base_size = 12, base_family = "serif") +
  labs(
    title = "Acroporid Coral Abundance Over Time by Species",,
    x = "Timepoint",
    y = "Abundance"
  ) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 10, family = "serif"),  # Horizontal x-axis labels
    axis.title = element_text(size = 12, face = "plain", family = "serif"),
    plot.title = element_text(size = 14, face = "plain", hjust = 0.5, family = "serif"),
    plot.subtitle = element_text(size = 12, face = "plain", hjust = 0.5, family = "serif"),
    strip.text = element_text(face = "plain", size = 12, family = "serif"),
    plot.margin = margin(20, 20, 60, 20)
  ) +
  coord_cartesian(clip = "off")


# View
print(final_plot)

# === Save Output ===
ggsave(
  filename = "AcropidAbundance.png",
  plot = final_plot,
  width = 12,        # adjust as needed for spacing
  height = 10,        # adjust as needed for vertical space
  dpi = 300,
  units = "in",
  bg = "white"
)
  