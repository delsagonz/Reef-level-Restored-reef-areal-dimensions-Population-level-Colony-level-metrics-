
#Creates a Boxplot showowing the restored planar area per Array for APAL
#represents the distribution of fragment tissue areas at one site (array) at each timepoint 
library(tidyverse)
library(ggplot2)
library(cowplot)
library(patchwork)


# Load the data.csv
apal_data <- read.csv("APALFragData.csv", stringsAsFactors = FALSE)

# Clean column names (optional, good for consistency)
colnames(apal_data) <- make.names(colnames(apal_data))

# Calculate mean restored live tissue for each array by timepointtt 
meanliveplanararea_by_tag_timepoint<- apal_data %>%
  group_by(TAG_NUMBER, TIMEPOINT) %>%
  summarise(
    LivePlanarMeanTissue = mean(LIVE_PLANAR_AREA_CM2.OR.TLE_CM, na.rm = TRUE),
    .groups = "drop"
  )

print(meanliveplanararea_by_tag_timepoint)

#groupped by site and tag and timepoint
meanliveplanararea_site_by_tag_timepoint <- apal_data %>%
  group_by(PLOT_ID, TAG_NUMBER, TIMEPOINT) %>%
  summarise(
    Mean_Live_Tissue = mean(LIVE_PLANAR_AREA_CM2.OR.TLE_CM, na.rm = TRUE),
    .groups = "drop"
  )

#This is every array at each time point and it's mean planar area 
ggplot(meanliveplanararea_by_tag_timepoint, aes(x = TIMEPOINT, y = LivePlanarMeanTissue)) +
  geom_col(aes(fill = TIMEPOINT), position = "dodge", width = 0.7) +
  facet_wrap(~ TAG_NUMBER, scales = "free_y") +
  labs(
    title = expression("Mean Restored Area per "*italic("Acropora palmata")*" Tag"),
    x = "Timepoint",
    y = "Mean Restored Planar Area (cm²)"
  ) +
  theme_bw(base_family = "serif") +
  theme(
    strip.text = element_text(face = "bold", size = 10),
    plot.title = element_text(hjust = 0.5, size = 14),
    legend.position = "none"
  )


# Plotting by site for each time point showing the Mean Restored 
ggplot(meanliveplanararea_site_by_tag_timepoint, aes(x = TIMEPOINT, y = Mean_Live_Tissue)) +
  geom_col(aes(fill = TIMEPOINT), position = "dodge", width = 0.7) +
  facet_wrap(~ PLOT_ID, scales = "free_y") +
  labs(
    title = expression("Mean Restored Planar Area per "*italic("Acropora palmata")*" Tag by Site"),
    x = "Timepoint",
    y = "Mean Restored Planar Area (cm²)"
  ) +
  theme_bw(base_family = "serif") +
  theme(
    strip.text = element_text(face = "bold", size = 10),
    plot.title = element_text(hjust = 0.5, size = 14),
    legend.position = "bottom"
  )

# Mean by array (PLOT_ID) and timepoint
mean_by_site <- apal_data %>%
  group_by(PLOT_ID, TIMEPOINT) %>%
  summarise(
    Mean_Live_Tissue = mean(LIVE_PLANAR_AREA_CM2.OR.TLE_CM, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(mean_by_site, aes(x = TIMEPOINT, y = Mean_Live_Tissue)) +
  geom_col(aes(fill = TIMEPOINT), position = "dodge", width = 0.7) +
  facet_wrap(~ PLOT_ID, scales = "free_y") +
  labs(
    title = expression("Mean Restored Planar Area per Array by Timepoint for "*italic("Acropora palmata")),
    x = "Timepoint",
    y = "Mean Planar Area (cm²)"
  ) +
  theme_bw(base_family = "serif") +
  theme(
    strip.text = element_text(face = "bold", size = 10),
    plot.title = element_text(hjust = 0.5, size = 14)
  )


# Convert live tissue column to numeric if needed
apal_data <- apal_data %>%
  mutate(
    LIVE_TISSUE = as.numeric(LIVE_PLANAR_AREA_CM2.OR.TLE_CM),
    TIMEPOINT = factor(TIMEPOINT, levels = c(
      "Post-outplanting (immediate)", 
      "Post-outplanting (3 months)", 
      "Post-outplanting (1 year)"
    ))
  )

# Define custom mint shades
mint_colors <- c(
  "Post-outplanting (immediate)" = "#c5f5e9",
  "Post-outplanting (3 months)"  = "#66d9cc",
  "Post-outplanting (1 year)"    = "#7bdac0"
)

# Create the mint-colored boxplot for restored Planar Area per Array
boxplot <- ggplot(apal_data, aes(x = PLOT_ID, y = LIVE_TISSUE)) +
  geom_boxplot(
    aes(fill = TIMEPOINT),
    outlier.shape = 21,
    outlier.size = 1.5,
    color = "gray30"
  ) +
  # add mean indicators
  stat_summary(
    fun = mean,
    geom = "point",
    shape = 4,       # X shape
    size = 2.5,
    color = "black",
    stroke = 1
  ) +
  facet_wrap(
    ~ TIMEPOINT,
    labeller = as_labeller(c(
      "Post-outplanting (immediate)" = "Post Outplant",
      "Post-outplanting (3 months)"  = "Three Months",
      "Post-outplanting (1 year)"    = "One Year"
    ))
  ) +
  scale_fill_manual(values = mint_colors) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +  # uniform y-axis with nice breaks
  labs(
    title = expression("Distribution of Restored Planar Mean Area of "*italic("Acropora palmata")*" (Elkhorn)"),
    x = "Plots",
    y = "Live Tissue Area (cm²)"
  ) +
  theme_bw(base_family = "serif") +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 1, size = 7),
    strip.text = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5, size = 14),
    legend.position = "none"
  )

print(boxplot)

ggsave(
  filename = "Restored_Planar_Area_Boxplot.jpg",
  plot = boxplot,
  width = 8,
  height = 6,
  dpi = 300
)



#---------------------------------------------------------------------------#
#Reporting Mean Restored Area +SE 
#making a graph mean that shows the each site performed over time
#shows more trends across timepoints 
# Step 1: Mean per array per timepoint to show the site level means shift
mean_by_array <- apal_data %>%
  group_by(PLOT_ID, TIMEPOINT) %>%
  summarise(Mean_Live_Tissue = mean(LIVE_TISSUE, na.rm = TRUE), .groups = "drop")

# Define mint shades (adjust or fine-tune if needed)
mint_colors <- c(
  "Post-outplanting (immediate)" = "#c5f5e9",
  "Post-outplanting (3 months)"  = "#66d9cc",
  "Post-outplanting (1 year)"    = "#7bdac0"
)
# Step 1: Calculate mean and SE per array per timepoint
mean_se_by_array <- apal_data %>%
  group_by(PLOT_ID, TIMEPOINT) %>%
  summarise(
    Mean_Live_Tissue = mean(LIVE_TISSUE, na.rm = TRUE),
    SE_Live_Tissue = sd(LIVE_TISSUE, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )


mean_se_by_array <- mean_se_by_array %>%
  mutate(PLOT_ID = recode(PLOT_ID,
                          "RC22" = "Z1-22-RC", 
                          "RC9" = "Z3-09-RC", 
                          "RC12" = "Z4-12-RC", 
                          "RC31" = "Z2-31-RC" 
  ))

# Step 2: Create plot with mint shades, SE bars, and mean labels

MeanLiveTissue<-
ggplot(mean_se_by_array, aes(x = PLOT_ID, y = Mean_Live_Tissue, fill = TIMEPOINT)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7, color = "gray30") +
  geom_errorbar(
    aes(ymin = Mean_Live_Tissue - SE_Live_Tissue,
        ymax = Mean_Live_Tissue + SE_Live_Tissue),
    position = position_dodge(width = 0.8),
    width = 0.2
  ) +
  geom_text(
    aes(label = round(Mean_Live_Tissue, 1)),
    position = position_dodge(width = 0.8),
    vjust = -0.9,
    hjust = -0.3,
    size = 3,
    family = "serif"
  ) +
  scale_fill_manual(
    name = "Timepoint", 
    values = mint_colors,
    labels = c(
      "Post-Outplant",
      "Three Months",
      "One Year"
    )
  )+
  labs(
  title = expression("Mean Live Tissue ± SE Restored Planar Area for "*italic("Acropora palmata") (Elkhorn)),    
  x = "Site",
    y = "Mean Restored Planar Area (cm²)"
  ) +
  theme_bw(base_family = "serif") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    plot.title = element_text(hjust = 0.5, size = 14),
    plot.caption = element_text(hjust = 0.5, face = "italic", size = 9),
    legend.title = element_blank(),
    strip.background = element_rect(fill = "gray90", color = "black"),
    strip.text = element_text(face = "bold", size = 10, family = "serif")
  )

print(MeanLiveTissue)

#edit graph to facet wrap version with gray background and similar headings 
MeanLiveTissue <- ggplot(mean_se_by_array, aes(x = PLOT_ID, y = Mean_Live_Tissue, fill = TIMEPOINT)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.5, color = "gray30") +
  geom_errorbar(
    aes(ymin = Mean_Live_Tissue - SE_Live_Tissue,
        ymax = Mean_Live_Tissue + SE_Live_Tissue),
    position = position_dodge(width = 0.8),
    width = 0.2
  ) +
  geom_text(
    aes(label = round(Mean_Live_Tissue, 1)),
    position = position_dodge(width = 0.8),
    vjust = -0.9,
    hjust = -0.3,
    size = 3,
    family = "serif"
  ) +
  scale_fill_manual(
    name = "Timepoint", 
    values = mint_colors,
    labels = c(
      "Post-Outplant",
      "Three Months",
      "One Year"
    )
  )+
  labs(
    x = "Plot",
    y = "Mean Restored Planar Area (cm²)"
  ) +
  theme_bw(base_family = "serif") +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 1, size = 8),
    plot.caption = element_text(hjust = 0.5, face = "italic", size = 9),
    legend.title = element_blank(),
    strip.background = element_rect(fill = "gray90", color = "black"),
    strip.text = element_text(face = "bold", size = 10, family = "serif"),
    plot.title = element_blank()
  )

# === Custom Title with Gray Background and Border ===
meanlive_title <- ggdraw() +
  draw_label(
    expression("Mean Restored Planar Area ± SE"),
    fontfamily = "serif",
    fontface = "bold",
    size = 13,
    x = 0.5, hjust = 0.5
  ) +
  theme(
    plot.background = element_rect(fill = "gray90", color = "black", linewidth = 0.8),
    plot.margin = margin(2, 5, 2, 5)
  )

# === Combine Title and Plot ===
final_meanlive_plot <- plot_grid(
  meanlive_title,
  MeanLiveTissue,
  ncol = 1,
  rel_heights = c(0.03, 1)
)

# === Display Final Output ===
print(final_meanlive_plot)

ggsave(
  filename = "MeanRestoredPlanarArea.jpg",
  plot = final_meanlive_plot,
  width = 8,
  height = 6,
  dpi = 300
)

#Exporting SE+Mean 
library(dplyr)
library(gridExtra) # for tableGrob
library(grid)      # for unit
library(gtable)

# Step 1: Add n to mean_se_by_array
mean_se_by_array <- apal_data %>%
  group_by(PLOT_ID, TIMEPOINT) %>%
  summarise(
    Mean_Live_Tissue = mean(LIVE_TISSUE, na.rm = TRUE),
    SE_Live_Tissue   = sd(LIVE_TISSUE, na.rm = TRUE) / sqrt(n()),
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(PLOT_ID = recode(PLOT_ID,
                          "RC22" = "Z1-22-RC", 
                          "RC9"  = "Z3-09-RC", 
                          "RC12" = "Z4-12-RC", 
                          "RC31" = "Z2-31-RC"))

# Step 2: Create a table graphic
table_plot <- tableGrob(
  mean_se_by_array,
  rows = NULL,
  theme = ttheme_default(
    core = list(fg_params = list(cex = 0.8)),
    colhead = list(fg_params = list(cex = 0.9, fontface = "bold"))
  )
)

# Step 3: Export as PDF
pdf("Mean_Live_Tissue_SE_Table.pdf", height = 6, width = 8)
grid.draw(table_plot)
dev.off()

# Step 4: Export as JPG
jpeg("Mean_Live_Tissue_SE_Table.jpg", height = 6, width = 8, units = "in", res = 300)
grid.draw(table_plot)
dev.off()

# === Export mean and SE table ===
write.csv(
  mean_se_by_array,
  file = "Mean_Live_Tissue_SE_Export.csv",
  row.names = FALSE
)



#Display total surface area 
#Total SurfaceArea
total_area_by_array <- apal_data %>%
  mutate(LIVE_TISSUE = as.numeric(LIVE_PLANAR_AREA_CM2.OR.TLE_CM)) %>%
  group_by(PLOT_ID, TIMEPOINT) %>%
  summarise(
    Total_Live_Tissue = sum(LIVE_TISSUE, na.rm = TRUE),
    .groups = "drop"
  )

total_area_by_array <- total_area_by_array %>%
  mutate(PLOT_ID = recode(PLOT_ID,
                          "RC22" = "Z1-22-RC", 
                          "RC9" = "Z3-09-RC", 
                          "RC12" = "Z4-12-RC", 
                          "RC31" = "Z2-31-RC" 
  ))

#Plot
totalareaplot <-ggplot(total_area_by_array, aes(x = PLOT_ID, y = Total_Live_Tissue, fill = TIMEPOINT)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7, color = "gray30") +
  geom_text(
    aes(label = round(Total_Live_Tissue, 1)),
    position = position_dodge(width = 0.8),
    vjust = -0.5,
    size = 3,
    family = "serif"
  ) +
  scale_fill_manual(values = mint_colors) +
  labs(
  x = "Site",
  y = "Total Live Planar Area (cm²)",
)
 +
  theme_bw(base_family = "serif") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    plot.title = element_text(hjust = 0.5, size = 14),
    plot.caption = element_text(hjust = 0.5, face = "italic", size = 9),
    legend.title = element_blank()
  )


# === Base Plot ===
totalareaplot <- ggplot(total_area_by_array, aes(x = PLOT_ID, y = Total_Live_Tissue, fill = TIMEPOINT)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7, color = "gray30") +
  geom_text(
    aes(label = round(Total_Live_Tissue, 1)),
    position = position_dodge(width = 0.8),
    vjust = -0.5,
    size = 3,
    family = "serif"
  ) +
  scale_fill_manual(values = mint_colors) +
  labs(
    x = "Site",
    y = "Total Live Planar Area (cm²)",
  ) +
  theme_bw(base_family = "serif") +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 1, size = 8),
    plot.caption = element_text(hjust = 0.5, face = "italic", size = 9),
    plot.title = element_blank(),
    legend.title = element_blank()
  )

# Remove legend from the totalareaplot
totalareaplot <- totalareaplot +
  theme(legend.position = "none")

# === Compact Title with Gray Background and Border ===
total_title <- ggdraw() +
  draw_label(
    "Total Surface Area",  # <-- pass the title text directly (no 'title =')
    fontfamily = "serif",
    fontface = "plain",
    size = 13,
    x = 0.5, hjust = 0.5
  ) +
  theme(
    plot.background = element_rect(fill = "gray90", color = "black", linewidth = 0.8),
    plot.margin = margin(2, 5, 2, 5)
  )

# === Combine Title and Plot with Less Space ===
final_totalarea_plot <- plot_grid(
  total_title,
  totalareaplot,
  ncol = 1,
  rel_heights = c(0.03, 1)  # less space for title
)

# === Display ===
print(final_totalarea_plot)




#Facet by plot to look have the x-axis be time point instead
# === 1. Prepare data ===
total_live_by_site <- apal_data %>%
  group_by(PLOT_ID, TIMEPOINT) %>%
  summarise(
    Total_Live_Planar = sum(LIVE_TISSUE, na.rm = TRUE),
    .groups = "drop"
  )

mean_se_by_site <- apal_data %>%
  group_by(PLOT_ID, TIMEPOINT) %>%
  summarise(
    Mean_Live_Tissue = mean(LIVE_TISSUE, na.rm = TRUE),
    SE_Live_Tissue   = sd(LIVE_TISSUE, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

# Define custom site labels
site_labels <- c(
  "RC31" = "Z2-31-RC",
  "RC9"  = "23-09-RC",
  "RC22" = "Z1-22-RC",
  "RC12" = "Z4-12-RC"
)

# Reorder PLOT_ID to match Z1 -> Z2 -> Z3 -> Z4
mean_se_by_site <- mean_se_by_site %>%
  mutate(
    PLOT_ID = factor(PLOT_ID, levels = c("RC22", "RC31", "RC9", "RC12"))  # Z1, Z2, Z3, Z4
  )

# Also apply the same order to total_live_by_site if plotting both
total_live_by_site <- total_live_by_site %>%
  mutate(
    PLOT_ID = factor(PLOT_ID, levels = c("RC22", "RC31", "RC9", "RC12"))
  )

# === 2. Custom theme ===
custom_theme <- theme_bw(base_family = "serif") +
  theme(
    strip.background = element_rect(fill = "gray90", color = "black"),
    strip.text = element_text(face = "plain", size = 10),
    plot.title = element_text(hjust = 0.5),                  
    panel.border = element_rect(color = "black", fill = NA),
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 9),
    axis.text.y = element_text(size = 9),
    axis.title = element_text(size = 10),
    legend.position = "bottom",
    legend.title = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

# === 3. Total Live Planar Area Plot with values ===
total_live_plot <- ggplot(total_live_by_site, aes(x = TIMEPOINT, y = Total_Live_Planar, fill = TIMEPOINT)) +
  geom_col(width = 0.7, color = "gray30") +
  geom_text(aes(label = round(Total_Live_Planar, 1)),
            vjust = -0.5, size = 2.8, family = "serif") +
  facet_wrap(~ PLOT_ID, ncol = 4, labeller = as_labeller(site_labels)) +
  scale_fill_manual(
    values = mint_colors,
    labels = c("Post-Outplant", "Three Months", "One Year")
  ) +
  scale_x_discrete(labels = c("Post-Outplant", "Three Months", "One Year")) +
  labs(
    title = expression("Total Live Planar Area of " * italic("Acropora palmata") * " (Elkhorn)"),
    x = "Timepoint",
    y = "Total Live Planar Area (cm²)"
  ) +
  custom_theme

# === 4. Mean Restored Planar Area ± SE Plot with values ===
mean_live_plot <- ggplot(mean_se_by_site, aes(x = TIMEPOINT, y = Mean_Live_Tissue, fill = TIMEPOINT)) +
  geom_col(width = 0.7, color = "gray30") +
  geom_errorbar(aes(ymin = Mean_Live_Tissue - SE_Live_Tissue,
                    ymax = Mean_Live_Tissue + SE_Live_Tissue),
                width = 0.2) +
  geom_text(aes(label = round(Mean_Live_Tissue, 1)),
            vjust = -0.5, size = 2.8, family = "serif",
            position = position_nudge(x = 0.2)) +   # move mean values right
  facet_wrap(~ PLOT_ID, ncol = 4, labeller = as_labeller(site_labels)) +
  scale_fill_manual(
    values = mint_colors,
    labels = c("Post-Outplant", "Three Months", "One Year")
  ) +
  scale_x_discrete(labels = c("Post-Outplant", "Three Months", "One Year")) +
  labs(
    title = expression("Mean Restored Planar Area (cm"^2*") ± SE of " * italic("Acropora palmata") * " (Elkhorn)"),
    x = "Timepoint",
    y = "Area (cm²)"
  ) +
  custom_theme

# === 5. Display separately ===
total_live_plot
mean_live_plot

# Save Total Live Planar Area plot
ggsave(
  filename = "Total_Live_Planar_Area_Apal.png",
  plot = total_live_plot,
  width = 10, height = 6, dpi = 300
)

# Save Mean Restored Planar Area ± SE plot
ggsave(
  filename = "Mean_Restored_Planar_Area_SE_Apal.png",
  plot = mean_live_plot,
  width = 10, height = 6, dpi = 300
)





#Mean percent change in live planar area
#PLotting Mean Percent Change in Live Planar Area for APAL
#Finding % change in planar area for apal
# Step 1: Summarize mean Planar Area and SE per site per timepoint
summary_stats_pct <- apal_data %>%
  group_by(PLOT_ID, TIMEPOINT) %>%
  summarise(
    Mean_PA = mean(LIVE_PLANAR_AREA_CM2.OR.TLE_CM, na.rm = TRUE),
    SE_PA = sd(LIVE_PLANAR_AREA_CM2.OR.TLE_CM, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  ) %>%
  mutate(
    PLOT_LABEL = case_when(
      PLOT_ID == "RC12" ~ "Z4-12-RC",
      PLOT_ID == "RC22" ~ "Z1-22-RC",
      PLOT_ID == "RC31" ~ "Z2-31-RC",
      PLOT_ID == "RC9"  ~ "Z3-09-RC",
      TRUE ~ PLOT_ID
    ),
    TIMEPOINT = recode(TIMEPOINT,
                       "Post-outplanting (immediate)" = "IP",
                       "ThreeMonth" = "ThreeMonth",
                       "OneYear" = "OneYear"
    )
  )
summary_stats_pct <- summary_stats_pct %>%
  mutate(TIMEPOINT = recode(TIMEPOINT,
                            "Post-outplanting (immediate)" = "IP",
                            "Post-outplanting (3 months)" = "ThreeMonth",
                            "Post-outplanting (1 year)" = "OneYear"
  ))


# Step 2A: Pivot wide
wide_stats <- summary_stats_pct %>%
  pivot_wider(
    names_from = TIMEPOINT,
    values_from = c(Mean_PA, SE_PA),
    names_glue = "{.value}_{TIMEPOINT}",
    values_fill = NA
  )

# Step 2B: Then add percent change
wide_stats <- wide_stats %>%
  mutate(
    PercentChange_ThreeMonth = ((Mean_PA_ThreeMonth - Mean_PA_IP) / Mean_PA_IP) * 100,
    SE_ThreeMonth = sqrt((SE_PA_IP / Mean_PA_IP)^2 +
                           (SE_PA_ThreeMonth / Mean_PA_ThreeMonth)^2) * abs(PercentChange_ThreeMonth),
    
    PercentChange_OneYear = ((Mean_PA_OneYear - Mean_PA_IP) / Mean_PA_IP) * 100,
    SE_OneYear = sqrt((SE_PA_IP / Mean_PA_IP)^2 +
                        (SE_PA_OneYear / Mean_PA_OneYear)^2) * abs(PercentChange_OneYear)
  )


# Step 3: Reshape to long format for plotting
long_pct <- wide_stats %>%
  pivot_longer(
    cols = c(PercentChange_ThreeMonth, SE_ThreeMonth, PercentChange_OneYear, SE_OneYear),
    names_to = c(".value", "Timepoint"),
    names_pattern = "(.*)_(ThreeMonth|OneYear)"
  ) %>%
  mutate(
    SITE_LABEL = case_when(
      PLOT_ID == "RC12" ~ "Z4-12-RC",
      PLOT_ID == "RC9"  ~ "Z3-09-RC",
      PLOT_ID == "RC31" ~ "Z2-31-RC",
      PLOT_ID == "RC22" ~ "Z1-22-RC",
      TRUE ~ PLOT_ID
    ),
    Timepoint = factor(Timepoint, levels = c("ThreeMonth", "OneYear"))
  ) %>%
  select(PLOT_ID, SITE_LABEL, Timepoint, PercentChange, SE)


# Step 4: Create dot layer for IP reference
dot_data <- summary_stats_pct %>%
  filter(TIMEPOINT == "IP") %>%
  rename(Mean_IP = Mean_PA) %>%
  mutate(
    SITE_LABEL = case_when(
      PLOT_ID == "RC12" ~ "Z4-12-RC",
      PLOT_ID == "RC9"  ~ "Z3-09-RC",
      PLOT_ID == "RC31" ~ "Z2-31-RC",
      PLOT_ID == "RC22" ~ "Z1-22-RC",
      TRUE ~ PLOT_ID
    )
  )

# Step 5: Plot
tle_pct_plot <- ggplot() +
  geom_col(data = long_pct,
           aes(x = SITE_LABEL, y = PercentChange, fill = Timepoint),
           position = position_dodge(width = 0.6), width = 0.5) +
  geom_errorbar(data = long_pct,
                aes(x = SITE_LABEL, ymin = PercentChange - SE, ymax = PercentChange + SE, group = Timepoint),
                width = 0.15, position = position_dodge(width = 0.6)) +
  geom_point(data = dot_data,
             aes(x = SITE_LABEL, y = 0),
             shape = 21, fill = "black", size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_text(data = long_pct,
            aes(x = SITE_LABEL, y = PercentChange, label = paste0(round(PercentChange, 1), "%"), group = Timepoint),
            position = position_dodge(width = 0.6),
            vjust = 1.3, hjust = -0.3, size = 3, family = "serif") +
  scale_fill_manual(
    values = c("ThreeMonth" = "#66d9cc", "OneYear" = "#b0e0c6"),
    labels = c("Three Month", "One Year")
  ) +
  labs(
    title = "Mean Percent Change in Live Planar Area of APAL",
    x = "Site",
    y = "Percent Change (%)",
    fill = NULL
  ) +
  theme_minimal(base_size = 14, base_family = "serif") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

# Display
print(tle_pct_plot)

#PLotting Mean Percent Change in Live Planar Area for APAL
# === 1. Set timepoint order ===
ordered_levels <- c("IP", "ThreeMonth", "OneYear")

# === 2. Clean and factor Timepoint in both datasets ===
long_pct <- long_pct %>%
  mutate(
    Timepoint = factor(Timepoint, levels = ordered_levels),
    vjust_label = case_when(
      SITE_LABEL %in% c("Z3-09-RC", "Z2-31-RC", "Z4-12-RC") ~ -1.5,
      TRUE ~ 1.3
    )
  )

dot_data <- dot_data %>%
  mutate(
    Timepoint = factor("IP", levels = ordered_levels),
    Label_IP = paste0("Mean Area: ", round(Mean_IP, 1), " cm²")
  )

# === 3. Build the plot ===
live_planar_plot_2 <- ggplot() +
  
  # Bars for percent change
  geom_col(
    data = long_pct,
    aes(x = Timepoint, y = PercentChange, fill = Timepoint),
    position = position_dodge(width = 0.6),
    width = 0.5
  ) +
  
  # Error bars with slight offset for OneYear
  geom_errorbar(
    data = long_pct %>%
      mutate(
        ymin_adj = ifelse(Timepoint == "OneYear", PercentChange - SE - 1, PercentChange - SE),
        ymax_adj = ifelse(Timepoint == "OneYear", PercentChange + SE - 1, PercentChange + SE)
      ),
    aes(x = Timepoint, ymin = ymin_adj, ymax = ymax_adj, group = Timepoint),
    width = 0.15,
    position = position_dodge(width = 0.6)
  ) +
  
  # IP dot
  geom_point(
    data = dot_data,
    aes(x = Timepoint, y = 0, fill = Timepoint),
    shape = 21,
    size = 3
  ) +
  
  # IP label
  geom_text(
    data = dot_data,
    aes(x = Timepoint, y = 0, label = Label_IP),
    vjust = -1.5,
    size = 3,
    family = "serif",
    color = "gray30"
  ) +
  
  # Dashed zero reference line
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  
  # Percent change labels
  geom_text(
    data = long_pct %>%
      mutate(hjust_label = ifelse(Timepoint == "OneYear", -1.6, -0.7)),
    aes(x = Timepoint, y = PercentChange, label = paste0(round(PercentChange, 1), "%"), 
        vjust = vjust_label, hjust = hjust_label),
    position = position_dodge(width = 0.5),
    size = 3,
    family = "serif"
  ) +
  
  # Manual fill colors (legend)
  scale_fill_manual(
    values = c(
      "IP" = "#ffff00",
      "ThreeMonth" = "#66d9cc",
      "OneYear" = "#7bdac0"
    ),
    labels = c(
      "IP" = "Post-Outplant",
      "ThreeMonth" = "Three Month",
      "OneYear" = "One Year"
    )
  ) +
  
  # Custom x-axis labels
  scale_x_discrete(
    drop = FALSE,
    labels = c(
      "IP" = "Post-Outplant",
      "ThreeMonth" = "Three Months",
      "OneYear" = "One Year"
    )
  ) +
  
  # Facet by site
  facet_wrap(~ SITE_LABEL, nrow = 3) +
  
  # Plot title, labels, theme
  labs(
    title = expression(" Percent Change in Mean Live Planar Area of " * italic("Acropora palmata") * " (Elkhorn)"),
    x = "Timepoint",
    y = "Percent Change (%)",
    fill = NULL
  ) +
  theme_minimal(base_size = 14, base_family = "serif") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    strip.background = element_rect(fill = "gray90", color = "black"),
    strip.text = element_text(face = "plain", size = 12),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    legend.position = "bottom"
  )

# Print the final plot
print(live_planar_plot_2)


ggsave("live_planar_plot_2.png",
       plot = live_planar_plot_2,
       width = 12,
       height = 14,
       dpi = 300,
       bg = "white")


# === Step 1: Create SE/SD table for percent change ===
se_pct_change <- wide_stats %>%
  mutate(
    SITE_LABEL = case_when(
      PLOT_ID == "RC12" ~ "Z4-12-RC",
      PLOT_ID == "RC22" ~ "Z1-22-RC",
      PLOT_ID == "RC31" ~ "Z2-31-RC",
      PLOT_ID == "RC9"  ~ "Z3-09-RC",
      TRUE ~ PLOT_ID
    )
  ) %>%
  select(SITE_LABEL, PLOT_ID,
         PercentChange_ThreeMonth, SE_ThreeMonth,
         PercentChange_OneYear, SE_OneYear) %>%
  rename(
    `% Change (3 Month)` = PercentChange_ThreeMonth,
    `SE (3 Month)` = SE_ThreeMonth,
    `% Change (1 Year)` = PercentChange_OneYear,
    `SE (1 Year)` = SE_OneYear
  ) %>%
  arrange(SITE_LABEL)

# === Step 2: View in console ===
print(se_pct_change, n = Inf)

# === Step 3: export ===
write.csv(se_pct_change, "APAL_PercentChange_SE_Table.csv", row.names = FALSE)

