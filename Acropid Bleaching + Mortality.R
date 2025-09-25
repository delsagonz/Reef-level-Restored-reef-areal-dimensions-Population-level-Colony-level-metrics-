#bleaching and mortality for all sites and both species 
# facet wrapped by site  

library(tidyverse)
library(patchwork)

# === LOAD PREPROCESS APAL ===
apal_data <- read.csv("APALFragData.csv") %>%
  mutate(
    Bleaching = ifelse(BLEACHING_P_A == "P", 1, 0),
    RecentMortality = ifelse(RECENT_MORT_P_A == "P", 1, 0),
    TIMEPOINT = factor(TIMEPOINT, levels = c("Post-outplanting (immediate)", "Post-outplanting (3 months)", "Post-outplanting (1 year)"))
  )

apal_summary <- apal_data %>%
  group_by(PLOT_ID, TIMEPOINT) %>%
  summarise(
    Bleaching_Prev = mean(Bleaching, na.rm = TRUE) * 100,
    Mortality_Prev = mean(RecentMortality, na.rm = TRUE) * 100,
    .groups = 'drop'
  ) %>%
  pivot_longer(cols = c(Bleaching_Prev, Mortality_Prev),
               names_to = "Condition", values_to = "Percent") %>%
  mutate(
    Condition = recode(Condition, "Bleaching_Prev" = "Bleaching", "Mortality_Prev" = "Recent Mortality"),
    CORAL_CODE = "APAL",
    Species_Condition = paste0("APAL.", Condition)
  ) %>%
  filter(TIMEPOINT != "Post-outplanting (1 year)")

# === LOAD & PREPROCESS ACER ===
acer_data <- read.csv("ACER_FragData2.csv", skip = 1, header = FALSE)
colnames(acer_data) <- c("SITE_ID", "SURVEY_DATE", "CORAL_SPECIES", "GENOTYPE_ID", "TAG_NUMBER",
                         "NUMBER_LIVE_FRAGS", "TLE_CM2", "PCT_LIVE_TISSUE","BLEACHING_P_A","RECENT_MORT_P_A",
                        "DISEASE_P_A", "ANALYSIS_DATE", "EXTRA1", "EXTRA2")

acer_clean <- acer_data %>%
  select(-EXTRA1, -EXTRA2) %>%
  mutate(
    SURVEY_DATE = as.Date(SURVEY_DATE, format = "%m/%d/%Y"),
    Bleaching = ifelse(BLEACHING_P_A == "P", 1, 0),
    RecentMortality = ifelse(RECENT_MORT_P_A == "P", 1, 0)
  ) %>%
  filter(!(TLE_CM2 == 0 & format(SURVEY_DATE, "%Y") == "2024"))

acer_summary <- acer_clean %>%
  group_by(SITE_ID, SURVEY_DATE) %>%
  summarise(
    Bleaching_Prev = mean(Bleaching, na.rm = TRUE) * 100,
    Mortality_Prev = mean(RecentMortality, na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  pivot_longer(cols = c(Bleaching_Prev, Mortality_Prev),
               names_to = "Condition", values_to = "Percent") %>%
  mutate(
    Condition = recode(Condition, "Bleaching_Prev" = "Bleaching", "Mortality_Prev" = "Recent Mortality"),
    TIMEPOINT = case_when(
      SURVEY_DATE %in% as.Date(c("2023-03-02", "2023-04-11")) ~ "Post-outplanting (immediate)",
      SURVEY_DATE %in% as.Date(c("2023-06-08", "2023-07-13")) ~ "Post-outplanting (3 months)"
    ),
    CORAL_CODE = "ACER",
    PLOT_ID = SITE_ID,
    Species_Condition = paste0("ACER.", Condition)
  ) %>%
  filter(!is.na(Percent))

# === COMBINE DATASETS ===
combined_data <- bind_rows(apal_summary, acer_summary)


# === CUSTOM LABELS & COLORS ===
custom_labels <- c(
  "RC22"  = "Z1-22-RC",
  "RC31" = "Z2-31-RC",
  "RC9" = "Z3-09-RC",
  "RC12" = "Z4-12-RC",
  "FR15" = "Z1-15-FR",
  "FR7"  = "Z2-07-FR"
)

fill_colors <- c(
  "APAL.Bleaching" = "#d0f0e0", "APAL.Recent Mortality" = "#93d4c2",
  "ACER.Bleaching" = "#ffe5cc", "ACER.Recent Mortality" = "#f4a582"
)

# Clean legend display
combined_data <- combined_data %>%
  mutate(
    TIMEPOINT = factor(TIMEPOINT, levels = c("Post-outplanting (immediate)", "Post-outplanting (3 months)")),
    TIMEPOINT_LABEL = recode(TIMEPOINT,
                             "Post-outplanting (immediate)" = "Immediate",
                             "Post-outplanting (3 months)" = "Three Month"
    ),
    Display_Condition = recode(Species_Condition,
                               "APAL.Bleaching" = "Bleaching",
                               "APAL.Recent Mortality" = "Recent Mortality",
                               "ACER.Bleaching" = "Bleaching",
                               "ACER.Recent Mortality" = "Recent Mortality"
    )
  )

# Add color group for each site
combined_data <- combined_data %>%
  mutate(
    Site_Group_Color = case_when(
      PLOT_ID %in% c("RC9", "RC12", "RC22", "RC31") ~ "Mint",  # RC = green
      TRUE ~ "Orange"  # FR = orange
    ),
    FillGroup = interaction(Site_Group_Color, Display_Condition, sep = ".")
  )


# Fix TIMEPOINT factor levels and labels
combined_data <- combined_data %>%
  mutate(
    TIMEPOINT = factor(TIMEPOINT, levels = c("Post-outplanting (immediate)", "Post-outplanting (3 months)")),
    TIMEPOINT_LABEL = recode(TIMEPOINT,
                             "Post-outplanting (immediate)" = "Post-Outplant",
                             "Post-outplanting (3 months)" = "Three Months"
    )
  )

#Plot
facet_by_site_plot <- ggplot(combined_data, aes(x = TIMEPOINT_LABEL, y = Percent, fill = FillGroup)) +
  geom_col(position = position_dodge(width = 0.9)) +
  geom_text(
    aes(label = round(Percent, 1)),
    position = position_dodge(width = 0.9),
    vjust = -0.3, size = 3, family = "serif"
  ) +
  facet_wrap(~ PLOT_ID, labeller = labeller(PLOT_ID = custom_labels)) +
  scale_y_continuous(limits = c(0, 110)) +
  scale_fill_manual(
    values = c(
      "Mint.Bleaching" = "#d0f0e0",
      "Mint.Recent Mortality" = "#93d4c2",
      "Orange.Bleaching" = "#ffe5cc",
      "Orange.Recent Mortality" = "#f4a582"
    ),
    labels = c(
      "Mint.Bleaching" = "Bleaching",
      "Mint.Recent Mortality" = "Mortality",
      "Orange.Bleaching" = "Bleaching",
      "Orange.Recent Mortality" = "Mortality"
    )
  ) +
  labs(
    title = "Prevalence of bleaching and partial mortality in staghorn and elkhorn arrays",
    subtitle = " Orange = Forereef Plots FR7 & FR15 (Staghorn), Mint = Reefcrest Plots (Elkhorn)",
    x = "Timepoint",
    y = "Percent of arrays affected"
  ) +
  theme_bw(base_size = 12, base_family = "serif") +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),  # Horizontal labels
    legend.position = "bottom",
    legend.title = element_blank(),
    plot.title = element_text(face = "plain", hjust = 0.5),  # Title bold and centered
    plot.subtitle = element_text(hjust = 0.5, face = "italic"),
    plot.caption = element_text(hjust = 0.5, size = 8, face = "italic", margin = margin(t = 10))
  )

print(facet_by_site_plot)

# === EXPORT ===
ggsave("Bleaching_Mortality_FacetBySite_FINAL.jpeg", plot = facet_by_site_plot, width = 12, height = 8, dpi = 300, bg = "white")

