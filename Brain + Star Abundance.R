#This will show abundance for brain and stars using facet wrap to have them sorted by Site specifically 
#Wild and restored abundances are on the graph 
#USE THIS CODE TO SHOW ABUNDANCE ACROSS ALL SITES WITH THE RUGOSITY INLCUDED as a value in %
library(tidyverse)

# Load your data
pop_long <- read.csv("nfwf_PopulationData.csv")

# Filter out unwanted species
pop_long <- pop_long %>%
  filter(!CORAL_CODE %in% c("APAL", "ACER"))

# Clean and recode TIMEPOINT, set order so Pre-outplant comes first
pop_long <- pop_long %>%
  mutate(
    TIMEPOINT = recode(TIMEPOINT,
                       "Brain/Star Pre-OP" = "Pre-outplant",
                       "Brain/Star Post-OP" = "Post-outplant"),
    TIMEPOINT = factor(TIMEPOINT, levels = c("Pre-outplant", "Post-outplant"))
  )

# Change label of Z2-15-FR to Z1-15-FR
pop_long <- pop_long %>%
  mutate(PLOT_ID = ifelse(PLOT_ID == "Z2-15-FR", "Z1-15-FR", PLOT_ID))

# Replace invalid entries "-" and "none outplanted" with 0 and convert to numeric
pop_long <- pop_long %>%
  mutate(
    RESTORED_CORAL_ABUN = as.numeric(ifelse(RESTORED_CORAL_ABUN %in% c("-", "none outplanted"), 0, RESTORED_CORAL_ABUN)),
    WILD_RESTORED_CORAL_ABUN = as.numeric(ifelse(WILD_RESTORED_CORAL_ABUN %in% c("-", "none outplanted"), 0, WILD_RESTORED_CORAL_ABUN)),
    RESTORED_CORAL_DENSITY_PER_M2_APAL = as.numeric(ifelse(RESTORED_CORAL_DENSITY_PER_M2_APAL %in% c("-", "none outplanted"), NA, RESTORED_CORAL_DENSITY_PER_M2_APAL))
  )

# Pivot longer for abundance by Type
pop_long <- pop_long %>%
  pivot_longer(
    cols = c(WILD_RESTORED_CORAL_ABUN, RESTORED_CORAL_ABUN),
    names_to = "Type", values_to = "Abundance"
  ) %>%
  mutate(
    Type = recode(Type,
                  WILD_RESTORED_CORAL_ABUN = "Wild",
                  RESTORED_CORAL_ABUN = "Restored")
  ) %>%
  filter(!is.na(Abundance))  # Keep rows with abundance (including zeros)

# Prepare density labels for restored abundance points
density_labels <- pop_long %>%
  filter(Type == "Restored", !is.na(RESTORED_CORAL_DENSITY_PER_M2_APAL)) %>%
  group_by(PLOT_ID, TIMEPOINT, CORAL_CODE) %>%
  summarise(
    y_pos = max(Abundance, na.rm = TRUE) * 1.02,

# Use the actual value, not multiplied by 100, and round to 1 decimal
label = paste0(round(mean(RESTORED_CORAL_DENSITY_PER_M2_APAL, na.rm = TRUE), 1), "%"),
.groups = "drop")


# Adjust label positions for density labels (moving them slightly higher and to the right)
density_labels <- density_labels %>% 
  mutate(
    y_pos_adjusted = y_pos + 0.2,  # Lower the label closer to the point (adjust as needed)
    x_pos_adjusted = as.numeric(TIMEPOINT) + 0.2  # Move the label a bit to the right (adjust as needed)
  )

# Plot combined species by site with restored density labels
combined_plot <- ggplot(pop_long, aes(
  x = TIMEPOINT,
  y = Abundance,
  group = interaction(CORAL_CODE, Type),
  color = CORAL_CODE,
  linetype = Type
)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  # geom_text(
  #   data = density_labels,
  #   aes(x = x_pos_adjusted, y = y_pos_adjusted, label = label),
  #   inherit.aes = FALSE,
  #   color = "black",
  #   size = 3.5,
  #   fontface = "italic"
  # ) +
  scale_y_continuous(
    breaks = scales::pretty_breaks(n = 10),
    labels = scales::number_format(accuracy = 1)
  ) +
  scale_x_discrete(
    labels = c(
      "Pre-outplant" = "Pre-Outplant\nJan 2025",
      "Post-outplant" = "Post-Outplant\nFeb/June 2025"
    )
  ) +
  facet_wrap(~ PLOT_ID, scales = "free_y") +
  scale_linetype_manual(values = c("Restored" = "solid", "Wild" = "dashed")) +
  labs(
    title = "Brain & Star Coral Abundance by Plot & Species",
    caption = "Species acronyms: PSTR = Pseudodiploria strigosa, OFAV = Orbicella faveolata, MCAV = Montastraea cavernosa,
    SINT = Siderastrea intersepta,PCLI = Pseudodiploria clivosa, OANN = Orbicella annularis, OrbSp. = Other Orbicella species",
    x = "Timepoint",
    y = "Abundance",
    color = "Species",
    linetype = "Type") +
  theme_bw(base_size = 12, base_family = "serif") +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 8.5),
    plot.title = element_text(hjust = 0.5, face = "plain", size = 14),
    plot.caption = element_text(hjust = 0.5, face = "italic", size = 9, family = "serif"), # smaller and italic
    strip.text = element_text(face = "plain", size = 11),
  )

# Display the plot
print(combined_plot)




# === Save Output ===
ggsave(
  filename = "Brain+Star.png",
  plot = combined_plot,
  width = 10,        # reduced width for better axis spacing
  height = 9,        # reduced height for better vertical space
  dpi = 300,
  units = "in",
  bg = "white"
)
