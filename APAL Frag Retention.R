#Frag Retention Data
#shows retention values the correct way! done need the top half of this  
#Load libraries
library(tidyverse)
library(cowplot)
# === Step 1: Load and Combine Datasets ===
rc9_31 <- read.csv("FragRentention_APAL_RC9_RC31.csv", stringsAsFactors = FALSE)
rc12_22 <- read.csv("APALFragData_Includes_RC12_RC22.CSV", stringsAsFactors = FALSE)  # Replace with your full dataset name if different

# Add identifiers
rc9_31$Source <- "RC9_RC31"
rc12_22$Source <- "RC12_RC22"

# Combine datasets
combined_data <- bind_rows(rc9_31, rc12_22)

# === Step 2: Clean and Filter ===
frag_data_all <- combined_data %>%
  mutate(
    NUMBER_INITIAL_FRAGS = as.numeric(replace_na(NUMBER_INITIAL_FRAGS, 0)),
    DEAD_FRAGS = as.numeric(replace_na(DEAD_FRAGS, 0)),
    MISSING_FRAGS = as.numeric(replace_na(MISSING_FRAGS, 0)),
    live_frag_counts = pmax(NUMBER_INITIAL_FRAGS - DEAD_FRAGS - MISSING_FRAGS, 0)
  ) %>%
  filter(
    PLOT_ID %in% c("RC9", "RC31", "RC22", "RC12"),
    TIMEPOINT != "Post-outplanting (1 year)",
    !is.na(GenotypeID),
    GenotypeID != "",
    GenotypeID != "#N/A"
  ) %>%
  mutate(PLOT_ID = recode(PLOT_ID,
                          "RC22" = "Z1-22-RC", 
                          "RC9"  = "Z3-09-RC", 
                          "RC12" = "Z4-12-RC", 
                          "RC31" = "Z2-31-RC"))

# === Step 3: Summarize and Reshape ===
frag_long <- frag_data_all %>%
  group_by(PLOT_ID, TIMEPOINT) %>%
  summarise(
    Live = sum(live_frag_counts, na.rm = TRUE),
    Dead = sum(DEAD_FRAGS, na.rm = TRUE),
    Missing = sum(MISSING_FRAGS, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = c(Live, Dead, Missing),
               names_to = "Fragment_Status",
               values_to = "Count") %>%
  mutate(
    Fragment_Status = factor(Fragment_Status, levels = c("Live", "Dead", "Missing")),
    TIMEPOINT = factor(TIMEPOINT, levels = c("Post-outplanting (immediate)", "Post-outplanting (3 months)"))
  )

# Filter for cleaner labels
frag_long_filtered <- frag_long %>% filter(!(TIMEPOINT == "Post-outplanting (immediate)" & Count == 0))

# === Retention % = Not Missing at 3 Months ===
retention_percent_data <- frag_data_all %>%
  filter(TIMEPOINT == "Post-outplanting (3 months)") %>%
  group_by(PLOT_ID) %>%
  summarise(
    Present = sum(live_frag_counts + DEAD_FRAGS, na.rm = TRUE),
    Total   = sum(live_frag_counts + DEAD_FRAGS + MISSING_FRAGS, na.rm = TRUE),
    Retention_Percent = round((Present / Total) * 100, 1),
    .groups = "drop"
  ) %>%
  mutate(Label = paste0(Retention_Percent, "%"))


# === Step 5: Define Palette ===
mint_shades <- c("Live" = "#c5f5e9", "Dead" = "#38a79b", "Missing" = "#b0e0c6")

# === Step 6: Plot ===
combined_plot <- ggplot(frag_long_filtered, aes(x = TIMEPOINT, y = Count, fill = Fragment_Status)) +
  geom_bar(stat = "identity", position = "stack", width = 0.9, color = "black") +
  geom_text(aes(label = ifelse(Count == 0, "", Count)),
            position = position_stack(vjust = 0.5),
            size = 3.2, family = "serif") +
  geom_label(
    data = retention_percent_data,
    aes(x = "Post-outplanting (3 months)", y = Present + 5, label = Label),
    inherit.aes = FALSE,
    fill = "white", color = "black",
    label.size = 0.4, label.r = unit(0.1, "lines"),
    size = 3.3, family = "serif", hjust = -1.5
  ) +
  facet_wrap(~PLOT_ID, ncol = 2) +
  scale_fill_manual(values = mint_shades) +
  scale_x_discrete(labels = c(
    "Post-outplanting (immediate)" = "Post-Outplant",
    "Post-outplanting (3 months)"  = "Three Months"
  ))+
scale_y_sqrt() +
  labs(
    title = expression("Fragment Summary of "*italic("Acropora palmata")  (Elkhorn)),
    subtitle = "Boxed values are the retention % or percent of arrays that retained in the plot at the three month timepoint.",
    caption = "Retention % is defined by = (Live + Dead) ÷ (Live + Dead + Missing) at 3-month timepoint x 100",
    x = "Timepoint",
    y = "Number of Fragments",
    fill = NULL
  ) +
  theme_bw(base_family = "serif") +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 9),
    plot.title = element_text(hjust = 0.5, face = "plain"),       
    plot.subtitle = element_text(hjust = 0.5, size = 10, face = "plain"),
    strip.text = element_text(face = "plain"),                      
    legend.position = "right",
    plot.caption = element_text(
      hjust = 0.5,
      face = "plain",
      size = 8,
      margin = margin(t = 10)
  )
)

# Show Plot
print(combined_plot)

# === Step 7: Save Plot ===
ggsave("Fragment_Retention_All_Sites.jpg", combined_plot, width = 10, height = 8, dpi = 300)

