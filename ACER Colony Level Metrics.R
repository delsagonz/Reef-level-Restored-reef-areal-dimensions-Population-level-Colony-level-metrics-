#Finding the mean total linear extension & creates an exportable table with SE values
library(gridExtra)
library(ggplot2)
library(tidyverse)
library(patchwork)

#Lines 7 - 200 are for finding the Mean Linear Extension + SE of ACER 
# Load the data
data <- read.csv("ACER_FragData.csv", skip = 1, header = FALSE)
colnames(data) <- c("SITE_ID", "SURVEY_DATE", "CORAL_SPECIES", "GENOTYPE_ID", "TAG_NUMBER", 
                    "NUMBER_LIVE_FRAGS", "TLE_CM2", "%_LIVE_TISSUE", "RECENT_MORT_P_A", 
                    "BLEACHING_P_A", "DISEASE_P_A", "ANALYSIS_DATE", "ANALYSIS_BY", "NOTES")

# Clean and prepare data
data <- data %>%
  mutate(
    SURVEY_DATE = as.Date(trimws(SURVEY_DATE), format = "%m/%d/%Y"),
    SITE_ID = trimws(SITE_ID),
    TLE_CM2 = as.numeric(TLE_CM2),
    NUMBER_LIVE_FRAGS = as.numeric(NUMBER_LIVE_FRAGS),
    TLE_CM2 = replace_na(TLE_CM2, 0),
    NUMBER_LIVE_FRAGS = replace_na(NUMBER_LIVE_FRAGS, 1),  # assume at least 1
    TAG_NUMBER = gsub("[^0-9]", "", TAG_NUMBER),
    TAG_NUMBER = sprintf("%03d", as.numeric(factor(TAG_NUMBER))),
    SE_TLE = ifelse(NUMBER_LIVE_FRAGS > 1, TLE_CM2 / sqrt(NUMBER_LIVE_FRAGS), NA)
  ) %>%
  filter(!SURVEY_DATE %in% as.Date(c("2024-02-07", "2024-04-05")))

# Generate tables per site
fr7_table <- data %>%
  filter(SITE_ID == "FR7") %>%
  select(SURVEY_DATE, TAG_NUMBER, TLE_CM2, NUMBER_LIVE_FRAGS, SE_TLE) %>%
  arrange(SURVEY_DATE, TAG_NUMBER)

fr15_table <- data %>%
  filter(SITE_ID == "FR15") %>%
  select(SURVEY_DATE, TAG_NUMBER, TLE_CM2, NUMBER_LIVE_FRAGS, SE_TLE) %>%
  arrange(SURVEY_DATE, TAG_NUMBER)

# View the result
print(fr7_table)
print(fr15_table)


# Export FR15 table as JPG
table_plot <- tableGrob(fr15_table)
pdf("fr15_table.pdf", width = 10, height = nrow(fr7_table) * 0.4 + 1)
grid.table(fr15_table)
dev.off()

# Export FR7 table as PDF
pdf("FR7_TLE_SE_Table.pdf", width = 10, height = nrow(fr7_table) * 0.4 + 1)
grid.table(fr7_table)
dev.off()


# Load and clean data
data <- read.csv("ACER_FragData.csv", skip = 1, header = FALSE)
colnames(data) <- c("SITE_ID", "SURVEY_DATE", "CORAL_SPECIES", "GENOTYPE_ID", "TAG_NUMBER", 
                    "NUMBER_LIVE_FRAGS", "TLE_CM2", "%_LIVE_TISSUE", "RECENT_MORT_P_A", 
                    "BLEACHING_P_A", "DISEASE_P_A", "ANALYSIS_DATE", "ANALYSIS_BY", "NOTES")

data <- data %>%
  mutate(
    SITE_ID = trimws(SITE_ID),
    SURVEY_DATE = as.Date(trimws(SURVEY_DATE), format = "%m/%d/%Y"),
    TLE_CM2 = as.numeric(TLE_CM2),
    TLE_CM2 = replace_na(TLE_CM2, 0)
  )

# Filter relevant data
filtered_data <- data %>%
  filter(
    (SITE_ID == "FR7" & SURVEY_DATE %in% as.Date(c("2023-04-11", "2023-07-13"))) |
      (SITE_ID == "FR15" & SURVEY_DATE %in% as.Date(c("2023-03-02", "2023-06-08")))
  )

# Step 1: 
# Summarize mean TLE and SE
summary_stats <- filtered_data %>%
  group_by(SITE_ID, SURVEY_DATE) %>%
  summarise(
    Mean_TLE = mean(TLE_CM2, na.rm = TRUE),
    SE_TLE = sd(TLE_CM2, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  ) %>%
  mutate(
    Timepoint = case_when(
      SURVEY_DATE %in% as.Date(c("2023-03-02", "2023-04-11")) ~ "Post-Outplant",
      SURVEY_DATE %in% as.Date(c("2023-06-08", "2023-07-13")) ~ "Three Month"
    ),
    # Factor to ensure plot order: Pre-Outplant first, then Three Month
    Timepoint = factor(Timepoint, levels = c("Post-Outplant", "Three Month"))
  )


#Ensure Timepoint is labeled and ordered correctly
summary_stats <- summary_stats %>%
  mutate(
    Timepoint = case_when(
      SURVEY_DATE %in% as.Date(c("2023-03-02", "2023-04-11")) ~ "Immediate Post",
      SURVEY_DATE %in% as.Date(c("2023-06-08", "2023-07-13")) ~ "Three Month"
    ),
    Timepoint = factor(Timepoint, levels = c("Immediate Post", "Three Month")),
    SITE_LABEL = case_when(
      SITE_ID == "FR7" ~ "Z2-07-FR",
      SITE_ID == "FR15" ~ "Z1-15-FR",
      TRUE ~ SITE_ID
    )
  )

# Step 2: Print to check factor levels
print(unique(summary_stats$Timepoint))

# Step 3: Plot with manual fill that MATCHES these exact factor levels
acermeantleplot <- ggplot(summary_stats, aes(x = SITE_LABEL, y = Mean_TLE, fill = Timepoint)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.5), width = 0.4) +
  geom_errorbar(
    aes(ymin = Mean_TLE - SE_TLE, ymax = Mean_TLE + SE_TLE),
    position = position_dodge(width = 0.5),
    width = 0.15
  ) +
  geom_text(
    aes(label = round(Mean_TLE, 1)),
    position = position_dodge(width = 0.5),
    vjust = -0.3,
    hjust = -0.7,
    size = 3,
    family = "serif"
  ) +
  scale_fill_manual(
    values = c(
      "Immediate Post" = "#ffc9a9",   # warm coral
      "Three Month" = "#ff9966"      # cool blue
    )
  ) +
  labs(
    title = expression("Mean TLE (cm"^2*") of "*italic("Acropora cervicornis")),
    subtitle = "Value presented in Mean Extension is the mean not the SE value",
    x = "Site",
    y = expression("Mean Total Linear Extension (cm"^2*")"),
    fill = NULL
  ) +
  theme_minimal(base_size = 14, base_family = "serif") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    legend.position = "bottom",
    legend.margin = margin(t = -10),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.y = element_text(margin = margin(r = 10))
  )

# Step 4: Display
print(acermeantleplot)

#give me the standard errors 
# --- Summary: mean, sd, n, SE ---
summary_stats <- filtered_data %>%
  group_by(SITE_ID, SURVEY_DATE) %>%
  summarise(
    n       = sum(!is.na(TLE_CM2)),
    Mean_TLE = mean(TLE_CM2, na.rm = TRUE),
    SD_TLE   = sd(TLE_CM2, na.rm = TRUE),
    SE_TLE   = SD_TLE / sqrt(n),
    .groups = "drop"
  ) %>%
  mutate(
    Timepoint = case_when(
      SURVEY_DATE %in% as.Date(c("2023-03-02", "2023-04-11")) ~ "Immediate Post",
      SURVEY_DATE %in% as.Date(c("2023-06-08", "2023-07-13")) ~ "Three Month",
      TRUE ~ NA_character_
    ),
    Timepoint = factor(Timepoint, levels = c("Immediate Post", "Three Month")),
    SITE_LABEL = case_when(
      SITE_ID == "FR7"  ~ "Z2-07-FR",
      SITE_ID == "FR15" ~ "Z1-15-FR",
      TRUE ~ SITE_ID
    )
  ) %>%
  arrange(SITE_ID, SURVEY_DATE)

# --- Nicely formatted export table (rounded) ---
export_table <- summary_stats %>%
  transmute(
    Plot = SITE_LABEL,
    Plot_Code = SITE_ID,
    Date = SURVEY_DATE,
    Timepoint = as.character(Timepoint), 
    n = n,
    Mean_TLE_cm2 = round(Mean_TLE, 2),
    SD_TLE_cm2   = round(SD_TLE, 2),
    SE_TLE_cm2   = round(SE_TLE, 2)
  )

grid.table(export_table)

ggsave("MeanLinearExtenstion_SD_SE.png", export_table, width = 8, height = 6, dpi = 300, bg = "white")

# Save plot 
ggsave("Mean_TLE_Acervicornis.png", acermeantleplot, width = 8, height = 6, dpi = 300, bg = "white")





#Lines 206-269 are for finding Total Live Tissue Area (TLE) 
#Finding TOTAL TLE 
library(tidyverse)
library(lubridate)
# Step 1: Read and prepare data
ACER_with1year <- read.csv("ACER_FragData.csv") %>%
  mutate(
    SURVEY_DATE = mdy(SURVEY_DATE),
    Timepoint = case_when(
      SITE_ID == "FR7" & SURVEY_DATE == as.Date("2023-04-11") ~ "IP",
      SITE_ID == "FR7" & SURVEY_DATE == as.Date("2023-07-13") ~ "ThreeMonth",
      SITE_ID == "FR15" & SURVEY_DATE == as.Date("2023-03-02") ~ "IP",
      SITE_ID == "FR15" & SURVEY_DATE == as.Date("2023-06-08") ~ "ThreeMonth",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Timepoint))

# Step 2: Calculate total TLE per site and timepoint
total_tle_summary <- ACER_with1year %>%
  group_by(SITE_ID, Timepoint) %>%
  summarise(
    Total_TLE = sum(TLE_CM2, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    SITE_LABEL = case_when(
      SITE_ID == "FR7" ~ "Z2-07-FR",
      SITE_ID == "FR15" ~ "Z1-15-FR",
      TRUE ~ SITE_ID
    )
  )

# Step 3: Plot
total_tle_plot <- ggplot(total_tle_summary, aes(x = SITE_LABEL, y = Total_TLE, fill = Timepoint)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.6), width = 0.5) +
  geom_text(
    aes(label = round(Total_TLE, 1)),
    position = position_dodge(width = 0.6),
    vjust = -0.3,
    size = 3,
    family = "serif"
  ) +
  scale_fill_manual(
    values = c("IP" = "#ffc9a9", "ThreeMonth" = "#f4a582"),
    labels = c("Immediate Post", "Three Month")
  ) +
  labs(
    title = "Total Live Tissue Area (TLE)",
    x = "Site",
    y = "Total TLE (cm²)",
    fill = NULL
  ) +
  theme_minimal(base_size = 14, base_family = "serif") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.y = element_text(margin = margin(r = 10)),
    legend.position = "bottom"
  )

# Step 4: Display plot
print(total_tle_plot)
ggsave("total_tle_plot.png", total_tle_plot, width = 8, height = 6, dpi = 300, bg = "white")






#Line 276 - 372 is showing frag survival/survivorship which shows survival rate % 
#This is found by looking at the # of live frags at initial outplant and uses how many are live at 3 month
#It does not account for any frags in the plot that are present **even though they are dead

# Step 1: Read and prepare data 
library(tidyverse)
library(lubridate)

# Step 1: Read and prepare data
ACER_with1year <- read.csv("ACER_FragData.csv") %>%
  mutate(
    SURVEY_DATE = mdy(SURVEY_DATE),
    Timepoint = case_when(
      SITE_ID == "FR7" & SURVEY_DATE == as.Date("2023-04-11") ~ "IP",
      SITE_ID == "FR7" & SURVEY_DATE == as.Date("2023-07-13") ~ "ThreeMonth",
      SITE_ID == "FR15" & SURVEY_DATE == as.Date("2023-03-02") ~ "IP",
      SITE_ID == "FR15" & SURVEY_DATE == as.Date("2023-06-08") ~ "ThreeMonth",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Timepoint))


# Step 2: Calculate total live fragment count (retention)
frag_retention_summary <- ACER_with1year %>%
  group_by(SITE_ID, Timepoint) %>%
  summarise(
    Live_Frags = sum(NUMBER_LIVE_FRAGS, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    SITE_LABEL = case_when(
      SITE_ID == "FR7" ~ "Z2-07-FR",
      SITE_ID == "FR15" ~ "Z1-15-FR",
      TRUE ~ SITE_ID
    )
  )

# Step 3: Calculate percent loss from IP to ThreeMonth
frag_loss_pct <- frag_retention_summary %>%
  select(SITE_LABEL, Timepoint, Live_Frags) %>%
  pivot_wider(names_from = Timepoint, values_from = Live_Frags) %>%
  mutate(
    Percent_Loss = round((IP - ThreeMonth) / IP * 100, 1)
  )

# Step 4: Join percent loss back to fragment retention data
frag_retention_with_loss <- frag_retention_summary %>%
  left_join(frag_loss_pct %>% select(SITE_LABEL, Percent_Loss), by = "SITE_LABEL")

# Step 5: Plot
frag_retention_plot <- ggplot(frag_retention_with_loss, aes(x = SITE_LABEL, y = Live_Frags, fill = Timepoint)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.6), width = 0.5) +
  
  # Live frag count labels
  geom_text(
    aes(label = Live_Frags),
    position = position_dodge(width = 0.6),
    vjust = -0.3,
    size = 3,
    family = "serif"
  ) +
  
  # Percent loss labels only for ThreeMonth bars
  geom_label(
    data = frag_retention_with_loss %>% filter(Timepoint == "ThreeMonth"),
    aes(label = paste0("-", Percent_Loss, "%")),
    position = position_dodge(width = 0.6),
    vjust = -1.5,
    hjust = 0.5,
    size = 3,
    family = "serif",
    fontface = "bold",
    fill = "white",
    label.size = 0.2
  ) +
  
  scale_fill_manual(
    values = c("IP" = "#ffc9a9", "ThreeMonth" = "#f4a582"),
    labels = c("Immediate Post", "Three Month")
  ) +
  labs(
    title = "Fragment Retention and % Percent Loss",
    x = "Site",
    y = "Total Number of Live Fragments",
    fill = NULL
  ) +
  theme_minimal(base_size = 14, base_family = "serif") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.y = element_text(margin = margin(r = 10)),
    legend.position = "bottom"
  )

# Step 6: Print plot
print(frag_retention_plot)






#Line 379 - 467 is for Fragment Retention Rates
#It uses the initial number of frags and compares it to how many are present in the plot at 3months
#**EVEN if they are dead it accounts for presence of frags

# === Load CSV without using header row ===
frag_retention_Acer <- read.csv("FragRetention_ACER.csv", skip = 1, header = FALSE)

# === Assign column names ===
colnames(frag_retention_Acer) <- c("SITE_ID", "Timepoint", "CORAL_SP", "FragCount", "Retention", "ANALYSIS_DATE", "ANALYSIS_BY")

# === Clean and convert columns ===
retention_clean <- frag_retention_Acer %>%
  mutate(
    Timepoint = factor(Timepoint, levels = c("Post-Outplant", "Three Months")),
    FragCount = as.numeric(FragCount),
    Retention = gsub("%", "", Retention),             # Remove % symbol
    Retention = as.numeric(Retention)                 # Convert to numeric
  )

# Small vertical offset so labels clear the bar tops
offset <- max(retention_clean$FragCount, na.rm = TRUE) * 0.04

# Create new column with custom site labels
retention_clean <- retention_clean %>%
  mutate(
    SITE_LABEL = recode(SITE_ID,
                        "FR07" = "Z2-07-FR",
                        "FR15" = "Z1-15-FR")
  )

#Graphing
fragretention_cleaned <- ggplot(retention_clean, aes(x = Timepoint, y = FragCount, fill = Timepoint)) +
  geom_col(width = 1) +
# Count value on top of every bar
  geom_text(
    aes(label = FragCount),
    vjust = -0.4,
    size = 3,
    family = "serif"
  ) +
# Boxed % retention label for Three Month bars only (lower right inside)
  geom_label(
    data = dplyr::filter(retention_clean, Timepoint == "Three Months"),
    aes(label = paste0(Retention, "%")),
    hjust = -1.9,
    vjust = 1.2,
    label.size = 0.25,
    fill = "white",
    color = "black",
    size = 3,
    family = "serif",
    label.padding = unit(0.15, "lines")
  ) +
  facet_wrap(~ SITE_LABEL) +
  scale_x_discrete(drop = FALSE) +
  scale_fill_manual(
    values = c(
      "Post-Outplant" = "#ffc9a9",  
      "Three Months"   = "#f4a582"   
    ),
    labels = c("Post-Outplant", "Three Months")
  ) +
  labs(
    title = "Fragment Retention",
    #subtitle = "Boxed values are the percent (%) of arrays at the three month timepoint that retained in the plot",
    x = "Timepoint",
    y = "Number of Fragments",
    fill = NULL
  ) +
  theme_minimal(base_size = 14, base_family = "serif") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12, face = "plain", family="serif"),
    plot.subtitle = element_text(hjust = 0.5 , face = "plain", size = 7),
    plot.caption = element_text(hjust = 0.5, face = "plain", size = 10, margin = margin(t = 10)),
    axis.text.x = element_text(angle = 0, hjust = 0.5, face = "plain"),
    axis.title.x = element_text(face = "plain", margin = margin(t = 10), size = 10,family="serif"),
    axis.title.y = element_text(face = "plain", margin = margin(t = 10), size = 10,family="serif"),
    axis.ticks.y = element_line(color = "black", size = 0.5),
    axis.title.y.right = element_text(family = "serif", size = 11),
    axis.text.y.right = element_text(size = 8.5, family = "serif"),
    panel.border = element_rect(color = "gray70", fill = NA, size = 0.2),
    strip.background = element_rect(fill = "gray90", color = "black", size =0.4),
    strip.text = element_text(color = "black", face = "plain"),
    legend.position = "none"
  ) +
  expand_limits(y = max(retention_clean$FragCount, na.rm = TRUE) * 1.15) +
  coord_cartesian(clip = "off")

print(fragretention_cleaned)



#Lines 471 - 583 shows mean percent change of live tissue area at the 3month and one year 
#Here we find Mean % Change of Live Tissue Area (does not have the isolated initial area dot on the graph)
#Showing % change at each time point includeing the 1 year  
# Load required libraries
library(tidyverse)

# Step 0: Load data and parse dates
data <- read.csv("ACER_FragData.csv")

ACER_with1year <- data %>%
  mutate(SURVEY_DATE = as.Date(SURVEY_DATE, format = "%m/%d/%Y"))

# Step 1: Summarize mean TLE and SE per site per date
summary_stats_pct <- ACER_with1year %>%
  group_by(SITE_ID, SURVEY_DATE) %>%
  summarise(
    Mean_TLE = mean(TLE_CM2, na.rm = TRUE),
    SE_TLE = sd(TLE_CM2, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  ) %>%
  # Assign exact timepoints based on site-specific dates
  mutate(
    Timepoint = case_when(
      SITE_ID == "FR7" & SURVEY_DATE == as.Date("2023-04-11") ~ "IP",
      SITE_ID == "FR7" & SURVEY_DATE == as.Date("2023-07-13") ~ "ThreeMonth",
      SITE_ID == "FR7" & SURVEY_DATE == as.Date("2024-04-05") ~ "OneYear",
      
      SITE_ID == "FR15" & SURVEY_DATE == as.Date("2023-03-02") ~ "IP",
      SITE_ID == "FR15" & SURVEY_DATE == as.Date("2023-06-08") ~ "ThreeMonth",
      SITE_ID == "FR15" & SURVEY_DATE == as.Date("2024-02-07") ~ "OneYear",
      
      TRUE ~ NA_character_
    ),
    Timepoint = factor(Timepoint, levels = c("IP", "ThreeMonth", "OneYear"))
  )

# Step 2: Pivot wider with proper column naming
tle_pct_change <- summary_stats_pct %>%
  filter(!is.na(Timepoint)) %>%
  pivot_wider(
    id_cols = SITE_ID,
    names_from = Timepoint,
    values_from = c(Mean_TLE, SE_TLE),
    names_glue = "{.value}_{Timepoint}",
    values_fn = mean,
    values_fill = NA
  ) %>%
  mutate(
    PercentChange_ThreeMonth = ((Mean_TLE_ThreeMonth - Mean_TLE_IP) / Mean_TLE_IP) * 100,
    SE_ThreeMonth = sqrt((SE_TLE_IP / Mean_TLE_IP)^2 + (SE_TLE_ThreeMonth / Mean_TLE_ThreeMonth)^2) * abs(PercentChange_ThreeMonth),
    
    PercentChange_OneYear = ((Mean_TLE_OneYear - Mean_TLE_IP) / Mean_TLE_IP) * 100,
    SE_OneYear = sqrt((SE_TLE_IP / Mean_TLE_IP)^2 + (SE_TLE_OneYear / Mean_TLE_OneYear)^2) * abs(PercentChange_OneYear),
    
    SITE_LABEL = SITE_ID
  )

# Step 3: Reshape to long format for plotting
tle_pct_long <- tle_pct_change %>%
  pivot_longer(
    cols = c(PercentChange_ThreeMonth, SE_ThreeMonth, PercentChange_OneYear, SE_OneYear),
    names_to = c(".value", "Timepoint"),
    names_pattern = "(.*)_(ThreeMonth|OneYear)"
  )
tle_pct_long <- tle_pct_long %>%
  mutate(
    SITE_LABEL = case_when(
      SITE_ID == "FR7" ~ "Z2-07-FR",
      SITE_ID == "FR15" ~ "Z1-15-FR",
      TRUE ~ SITE_ID  # fallback
    )
  )


# Step 4: Plot
tle_pct_plot <- ggplot(tle_pct_long, aes(x = SITE_LABEL, y = PercentChange, fill = Timepoint)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.6), width = 0.5) +
  geom_errorbar(
    aes(ymin = PercentChange - SE, ymax = PercentChange + SE),
    width = 0.15,
    position = position_dodge(width = 0.6)
  ) + geom_hline(yintercept = 0, linetype = "dashed", color = "gray50")+
  geom_text(
    aes(label = paste0(round(PercentChange, 1), "%")),
    position = position_dodge(width = 0.6),
    vjust = 1,
    hjust = -0.8,
    size = 3,
    family = "serif"
  ) +
  scale_fill_manual(
    values = c(
      "ThreeMonth" = "#f4a582",  # light orange
      "OneYear" = "#ffc9a9"      # peach
    ),
    labels = c("Three Month", "One Year")
  ) +
  labs(
    title = "Mean Percent Change in Live Tissue Are over All Timepoints",
    x = "Site",
    y = "Percent Change (%)",
    fill = NULL  # This removes the legend title
  ) +
  theme_minimal(base_size = 14, base_family = "serif") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.y = element_text(margin = margin(r = 10)),
    legend.position = "bottom"
  )

# Step 5: Display plot
print(tle_pct_plot)


#Line 586 - 717 shows mean % change the same as the section above but -> THIS HERE ADDS THE DOT FOR THE INTIAL MEAN
#The current tle_pct_long dataframe calculates:PercentChange_ThreeMonth = (ThreeMonth - IP) / IP * 100
#and PercentChange_OneYear = (OneYear - IP) / IP * 100 So you get two values per site:Change from IP → ThreeMonth and Change from IP → OneYear
#cannot calculate percent change from IP → IP — that would always be 0%, so it’s excluded by design.


library(tidyverse)

# === Step 1: Format long data ===
tle_pct_long <- tle_pct_change %>%
  mutate(
    SITE_LABEL = case_when(
      SITE_ID == "FR7" ~ "Z2-07-FR",
      SITE_ID == "FR15" ~ "Z1-15-FR",
      TRUE ~ SITE_ID
    )
  ) %>%
  pivot_longer(
    cols = c(PercentChange_ThreeMonth, PercentChange_OneYear,
             SE_ThreeMonth, SE_OneYear),
    names_to = c(".value", "Timepoint"),
    names_pattern = "(.*)_(ThreeMonth|OneYear)"
  ) %>%
  mutate(Mean_TLE_IP = NA)

# === Step 2: Add PostOutplant rows as dot ===
post_outplant_rows <- tle_pct_change %>%
  transmute(
    SITE_ID,
    SITE_LABEL = case_when(
      SITE_ID == "FR7" ~ "Z2-07-FR",
      SITE_ID == "FR15" ~ "Z1-15-FR",
      TRUE ~ SITE_ID
    ),
    Timepoint = "PostOutplant",
    PercentChange = 0,
    SE = 0,
    Mean_TLE_IP = Mean_TLE_IP
  )

# === Step 3: Combine datasets and enforce factor order ===
tle_plot_data <- bind_rows(tle_pct_long, post_outplant_rows) %>%
  mutate(Timepoint = factor(Timepoint, levels = c("PostOutplant", "ThreeMonth", "OneYear")))

# === Step 4: Plot ===
facet_tle_pct_plot <- ggplot(tle_plot_data, aes(x = Timepoint)) +
  # Bars for Three Month and One Year
  geom_bar(
    data = filter(tle_plot_data, Timepoint %in% c("ThreeMonth", "OneYear")),
    aes(y = PercentChange, fill = Timepoint),
    stat = "identity", width = 0.7
  ) +
  # Error bars
  geom_errorbar(
    data = filter(tle_plot_data, Timepoint %in% c("ThreeMonth", "OneYear")),
    aes(ymin = PercentChange - SE, ymax = PercentChange + SE),
    width = 0.2
  ) +
  # Post-Outplant dot (centered)
  geom_point(
    data = filter(tle_plot_data, Timepoint == "PostOutplant"),
    aes(x = Timepoint, y = PercentChange, fill = Timepoint),
    shape = 21, color = "black", size = 3,
    inherit.aes = FALSE
  ) +
  # Post-Outplant label (centered)
  geom_text(
    data = filter(tle_plot_data, Timepoint == "PostOutplant"),
    aes(x = Timepoint,
        y = PercentChange + 5,
        label = paste0("Mean Size: ", round(Mean_TLE_IP, 1), " cm²")),
    family = "serif", size = 3, color = "gray30",
    inherit.aes = FALSE
  ) +
  # Percent change labels on bars
  geom_text(
    data = filter(tle_plot_data, Timepoint %in% c("ThreeMonth", "OneYear")),
    aes(y = PercentChange - 5,
        label = paste0(round(PercentChange, 1), "%")),
    family = "serif", size = 3, hjust = 1.2
  ) +
  facet_wrap(~ SITE_LABEL, shrink = TRUE) +
  scale_fill_manual(
    values = c(
      "PostOutplant" = "#FFD700",
      "ThreeMonth" = "#ffc9a9",
      "OneYear" = "#f4a582"
    ),
    labels = c(
      "PostOutplant" = "Post-Outplant",
      "ThreeMonth" = "Three Months",
      "OneYear" = "One Year"
    ),
    name = NULL
  ) +
  scale_x_discrete(
    limits = c("PostOutplant", "ThreeMonth", "OneYear"),
    labels = c(
      "PostOutplant" = "Post-Outplant",
      "ThreeMonth" = "Three Months",
      "OneYear" = "One Year"
    )
  ) +
  labs(
    title = expression("Percent Change (%) in Mean Live Tissue Area of " * italic("Acropora cervicornis") * " (Staghorn)"),
    subtitle = "Immediate Post-Outplant Mean Size (cm2) is shown as circle in order to help visualize the change in live tissue",
    x = "Timepoint",
    y = "Percent Change (%)"
  ) +
  theme_bw(base_size = 12, base_family = "serif") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "plain"),
    plot.subtitle = element_text(hjust = 0.5, size = 11, face = "italic"),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    strip.background = element_rect(fill = "gray90", color = "black"),
    strip.text = element_text(face = "plain", family = "serif", size = 11),
    legend.position = "bottom",
    legend.text = element_text(family = "serif", size = 10),
    panel.border = element_rect(color = "gray70", fill = NA, size = 0.4),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white", color = NA)
  )

#Shows the SE in the Envirnment for easy exporting 
standard_errors_meanpercent <- tle_plot_data %>%
  select(SITE_LABEL, Timepoint, SE) %>%
  arrange(SITE_LABEL, Timepoint)

# === Display plot ===
print(facet_tle_pct_plot)
ggsave("facet_tle_pct_plot.png", facet_tle_pct_plot, width = 10, height = 6, dpi = 300)







#Line 725 - 943 combines all 3 graphs (Total TLE / Mean LE / Frag retention)
#Combining all these graphs for one easy export in 1 graph!
#DOES NOT include mean % change in area with initial  size dot = separate graph

# === Load the data ===
acer_data <- read.csv("ACER_FragData.csv", skip = 1, header = FALSE)
colnames(acer_data) <- c("SITE_ID", "SURVEY_DATE", "CORAL_SPECIES", "GENOTYPE_ID", "TAG_NUMBER", 
                         "NUMBER_LIVE_FRAGS", "TLE_CM2", "%_LIVE_TISSUE", "RECENT_MORT_P_A", 
                         "BLEACHING_P_A", "DISEASE_P_A", "ANALYSIS_DATE", "ANALYSIS_BY", "NOTES")

# === Standardize SITE_IDs ===
acer_data <- acer_data %>%
  mutate(
    SITE_ID = trimws(SITE_ID),
    SITE_ID = recode(SITE_ID, "FR7" = "Z2-07-FR", "FR15" = "Z1-15-FR"),
    SURVEY_DATE = as.Date(SURVEY_DATE, format = "%m/%d/%Y"),
    TIMEPOINT = case_when(
      SURVEY_DATE == as.Date("2023-04-11") ~ "IP",
      SURVEY_DATE == as.Date("2023-07-13") ~ "ThreeMonth",
      SURVEY_DATE == as.Date("2024-04-05") ~ "OneYear",
      SURVEY_DATE == as.Date("2023-03-02") ~ "IP",
      SURVEY_DATE == as.Date("2023-06-08") ~ "ThreeMonth",
      SURVEY_DATE == as.Date("2024-02-07") ~ "OneYear",
      TRUE ~ NA_character_
    )
  )

# === Updated timepoint labels ===
timepoint_colors <- c("IP" = "#ffc9a9", "ThreeMonth" = "#f4a582")
timepoint_labels <- c(
  "IP" = "Post-Outplant",
  "ThreeMonth" = "Three Months"
)

# === Shared theme with horizontal legend and plain strip titles ===
shared_theme <- theme_bw(base_size = 12, base_family = "serif") +
  theme(
    strip.background = element_rect(fill = "gray90", color = "black"),
    strip.text = element_text(face = "plain", size = 11, family = "serif"),
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 8.5),
    panel.border = element_rect(color = "gray70", fill = NA, size = 0.2),
    plot.title = element_text(hjust = 0.5, face = "plain", size = 12, family = "serif"),
    legend.position = "bottom",
    legend.direction = "horizontal",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )



# === Clean and standardize TIMEPOINT in total_tle_summary ===
total_tle_summary <- total_tle_summary %>%
  rename(TIMEPOINT = Timepoint) %>%
  mutate(
    SITE_LABEL = recode(SITE_LABEL, "FR7" = "Z2-07-FR", "FR15" = "Z1-15-FR"),
    TIMEPOINT = recode(trimws(TIMEPOINT),
                       "IP" = "IP",
                       "Immediate Post" = "IP",
                       "ThreeMonth" = "ThreeMonth",
                       "Three Month" = "ThreeMonth")
  )


# === Summarize Mean TLE and SE ===
acer_summary <- acer_data %>%
  group_by(SITE_ID, TIMEPOINT) %>%
  summarize(
    Mean_TLE = mean(TLE_CM2, na.rm = TRUE),
    SE_TLE = sd(TLE_CM2, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  ) %>%
  filter(Mean_TLE > 0)

# === Retention data + percent retained ===
retention_df <- tibble(
  TIMEPOINT = c("IP", "ThreeMonth", "OneYear", "IP", "ThreeMonth", "OneYear"),
  SURVEY_DATE = as.Date(c("2023-04-11", "2023-07-13", "2024-04-05", "2023-03-02", "2023-06-08", "2024-02-07")),
  SITE_LABEL = c("Z2-07-FR", "Z2-07-FR", "Z2-07-FR", "Z1-15-FR", "Z1-15-FR", "Z1-15-FR"),
  Live_Fragments = c(112, 67, 0, 120, 89, 0)
) %>%
  filter(Live_Fragments > 0)

initial_counts <- retention_df %>%
  filter(TIMEPOINT == "IP") %>%
  select(SITE_LABEL, Initial_Fragments = Live_Fragments)

retention_df <- retention_df %>%
  left_join(initial_counts, by = "SITE_LABEL") %>%
  mutate(Percent_Retained = round(100 * Live_Fragments / Initial_Fragments, 1))

# === Total TLE Plot with tighter facets ===
total_tle_plot <- ggplot(total_tle_summary, aes(x = TIMEPOINT, y = Total_TLE, fill = TIMEPOINT)) +
  geom_bar(stat = "identity", width = 1) +
  geom_text(aes(label = round(Total_TLE, 1)), position = position_dodge(0.4), vjust = -0.3, size = 3, family = "serif") +
  facet_wrap(~ SITE_LABEL, nrow=1) +
  theme(panel.spacing = unit(0, "lines")) +
  scale_fill_manual(values = timepoint_colors, labels = timepoint_labels) +
  labs(title = "Total Linear Extension (TLE)", y = "Total TLE (cm)", fill = NULL) +
  shared_theme +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.border = element_rect(color = "gray70", fill = NA, size = 0.5),
    plot.background = element_rect(fill = "white", color = NA)
  )

# === Mean TLE Plot with tighter facets ===
acermeantle_clean <- ggplot(acer_summary, aes(x = TIMEPOINT, y = Mean_TLE, fill = TIMEPOINT)) +
  geom_bar(stat = "identity", width = 1) +
  geom_errorbar(aes(ymin = Mean_TLE - SE_TLE, ymax = Mean_TLE + SE_TLE),
                position = position_dodge(0.4), width = 0.2, color = "black") +
  geom_text(aes(label = round(Mean_TLE, 1)),
            position = position_dodge(0.4), vjust = -2.5, hjust = -0.05, size = 3, family = "serif") +
  facet_wrap(~ SITE_ID, nrow = 1) +
  theme(panel.spacing = unit(0, "lines")) +
  scale_fill_manual(values = timepoint_colors, labels = timepoint_labels) +
  scale_y_continuous(
    name = NULL,                                # hide left title
    sec.axis = dup_axis(name = "Mean TLE (cm)") # <-- right-side title you want
  ) +
  labs(title = "Mean Linear Extension", x = NULL, fill = NULL) +
  shared_theme +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    # hide LEFT axis text/ticks, keep RIGHT visible
    axis.text.y  = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),
    axis.title.y.right = element_text(family = "serif", size = 11),
    axis.text.y.right  = element_text(size = 8.5, family = "serif"),
    axis.ticks.y.right = element_line(),
    panel.border = element_rect(color = "gray70", fill = NA, size = 0.5),
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  coord_cartesian(ylim = c(0, max(acer_summary$Mean_TLE + acer_summary$SE_TLE, na.rm = TRUE) * 1.2))

# === Fragment Retention Plot (ThreeMonth % only) ===
max_fragments <- max(retention_df$Live_Fragments, na.rm = TRUE)

# === Fragment Retention Plot with tighter facets ===
frag_retention_clean <- ggplot(retention_df, aes(x = TIMEPOINT, y = Live_Fragments, fill = TIMEPOINT)) +
  geom_bar(stat = "identity", width = 1, show.legend = FALSE) +
  geom_text(aes(label = round(Live_Fragments, 1)), vjust = -0.5, size = 3, family = "serif") +
  geom_label(
    data = retention_df %>% filter(TIMEPOINT == "ThreeMonth"),
    aes(label = paste0(Percent_Retained, "%")),
    vjust = 1.8, hjust = 0, nudge_x = 0.15,
    fill = "white", color = "black", size = 3, label.size = 0.3, family = "serif"
  ) +
  facet_wrap(~ SITE_LABEL, nrow=1) +
  theme(panel.spacing = unit(0, "lines")) +
  scale_fill_manual(values = timepoint_colors, labels = timepoint_labels) +
  scale_x_discrete(labels = c("IP" = "Post-Outplant", "ThreeMonth" = "Three Months")) +
  labs(
    title = "Fragment Retention",
    x = "Timepoint", y = "Number of Live Fragments"
  ) +
  shared_theme +
  theme(
    strip.background = element_rect(fill = "gray90", color = "black", size = 0.6),
    strip.text = element_text(face = "plain", family = "serif", size = 11),
    panel.background = element_rect(fill = "white"),
    panel.border = element_rect(color = "gray70", fill = NA, size = 0.5),
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  coord_cartesian(ylim = c(0, max_fragments * 1.2))

# === Match Y-axis ===
#shared_y_limits <- c(0, max(
  #layer_data(total_tle_plot, 1)$y,
  #layer_data(acermeantle_clean, 1)$y,
  #na.rm = TRUE
#) * 1.2)  # 20% buffer to prevent overlap

total_tle_plot <- total_tle_plot + coord_cartesian(ylim = shared_y_limits)
acermeantle_clean <- acermeantle_clean <- acermeantle_clean + coord_cartesian(ylim = c(0, max(acer_summary$Mean_TLE + acer_summary$SE_TLE) * 1.2))


# === Combine Layout ===
top_row <- total_tle_plot + acermeantle_clean + plot_layout(ncol = 2)

bottom_row <- plot_spacer() + fragretention_cleaned + plot_spacer() +
  plot_layout(ncol = 3, widths = c(0.15, 1, 0.15))



combined_final <- (top_row / bottom_row) +
  plot_layout(guides = "collect", heights = c(1, 1)) +
  plot_annotation(
    title = expression("Monitoring Metrics of " * italic("Acropora cervicornis") (Staghorn)),
    subtitle = "Includes Total Linear Extension, Mean Linear Extension, and Fragment Retention",
    #caption = "Note: 1-year data not shown due to 100% array mortality.",
    theme = theme(
      plot.title = element_text(hjust = 0.5, family = "serif", face = "plain", size = 14),
      plot.subtitle = element_text(hjust = 0.5, family = "serif", face = "italic", size = 11),
      plot.caption = element_text(hjust = 0.5, size = 10, face = "italic", family = "serif"),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.title = element_blank(),
      legend.text = element_text(family = "serif", size = 10),
      plot.background = element_rect(fill = "white", color = NA)  # Ensures saved image is white
    )
  )


# === Display ===
print(combined_final)

ggsave(
  filename = "Monitoring_Metrics_Acervicornis_TLE_MeanLE_FragRetention.png",
  plot = combined_final,
  width = 10,
  height = 10,
  dpi = 300,
  bg = "white"
)




