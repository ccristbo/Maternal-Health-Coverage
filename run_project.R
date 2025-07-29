# /*****************************************************************************************************
# Program:           UNICEF_Projections_Analysis.R
# Purpose:           Process and visualize under-5 population data from UNICEF/WPP based on progress status 
#                    toward under-five mortality rate (U5MR) reduction ("On-track" vs "Off-track").
# Data inputs:       - WPP2022_GEN_F01_DEMOGRAPHIC_INDICATORS_COMPACT_REV1.xlsx
#                    - On-track and off-track countries.xlsx
# Data outputs:      - World map showing country weights and U5MR status
#                    - Time series plot: annual percent change in population by region and U5MR status
# Author:            Applicant
# Date last modified: July 28, 2025
# Notes:             
#   - Map shows population-weighted visualization by U5MR classification for 2022
#   - Time series shows annual % change in under-5 population from 2018 to 2022
#   - Sources: UN WPP 2022 population projections and UNICEF U5MR classifications
# *****************************************************************************************************/

# /*----------------------------------------------------------------------------------
# Variables created in this file:
# unicef                "Merged estimates and projections dataset with U5MR status"
# weights               "Population weights: pop × births (used in map shading)"
# mapdata_filtered      "Spatial map dataset with U5MR status and ISO3 codes"
# unicefVariation       "Time-series data with % population change by region and U5MR status"
# ----------------------------------------------------------------------------------*/

# Load required packages
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)
library(stringr)
library(rnaturalearth)
library(sf)
library(ggrepel)

#------------------------
# Read Excel data (estimates + projections) and clean header rows
#------------------------
read_data <- function(file_path) {
  sheets <- c("Estimates", "Projections")
  result <- lapply(sheets, function(sheet) {
    read_excel(file_path, sheet = sheet) %>%
      filter(!is.na(...1)) %>%
      { colnames(.) <- as.character(.[1, ])
      .[-1, ]}
  })
  names(result) <- sheets
  return(result)
}
data <- read_data("01_rawdata/WPP2022_GEN_F01_DEMOGRAPHIC_INDICATORS_COMPACT_REV1.xlsx")

# Extract cleaned estimates and projections
estimates <- data$Estimates
projections <- data$Projections

#------------------------
# Read and clean SBA and ANC4 data
#------------------------

DATAFLOW <- read_excel("01_rawdata/GLOBAL_DATAFLOW_2018-2022.xlsx") %>% 
  filter(!is.na(`2022`)) %>% 
  select(-...3) %>% 
  rename(ANC_SBA = ...2) %>% 
  pivot_longer(    cols = `2018`:`2022`,
    names_to = "Year",
    values_to = "Percentage"  ) %>% 
  mutate(    Percentage = ifelse(grepl("^[0-9.]+$", Percentage), Percentage, NA),
             Percentage = as.numeric(Percentage),
    ANC_SBA = case_when(
      str_detect(ANC_SBA, "Skilled birth attendant") ~ "SBA",
      str_detect(ANC_SBA, "Antenatal care") ~ "ANC4",
      TRUE ~ "OTHER")  ) %>% 
  rename(OfficialName = `Time period`  ) %>% 
  pivot_wider(
    names_from = ANC_SBA,
    values_from = Percentage
  )
#------------------------
# Read and clean U5MR status classification
#------------------------
classification <- read_excel("01_rawdata/On-track and off-track countries.xlsx") %>% 
  mutate(Status.U5MR = ifelse(Status.U5MR == "Acceleration Needed", "Off-track", "On-track")) %>% 
  left_join(DATAFLOW, by = "OfficialName") %>% 
  filter(!is.na(Year)) %>% 
  mutate(across(c(ANC4, SBA), ~ as.character(map_chr(., ~ if(length(.) == 0) NA_character_ else as.character(.))))) %>% 
  mutate(across(c(ANC4, SBA), ~ na_if(., "NULL"))) %>% 
  mutate(across(c(ANC4, SBA), as.numeric)) %>% 
  filter(Year >= 2018 & Year <= 2022) %>%
  group_by(ISO3Code, OfficialName,Status.U5MR) %>%
  arrange(desc(Year)) %>%
  summarise(ANC4 = first(ANC4[!is.na(ANC4)]),
    SBA = first(SBA[!is.na(SBA)]),
    .groups = "drop"  )

#------------------------
# Merge data and compute population weights for map visualization
#------------------------
unicef <- bind_rows(
  estimates %>% mutate(source = "estimates"),
  projections %>% mutate(source = "projections")) %>%
  left_join(classification %>% rename(`ISO3 Alpha-code` = ISO3Code)) %>%
  filter(
    Year %in% 2018:2022,
    !is.na(`ISO3 Alpha-code`),
    !is.na(OfficialName)
  ) %>%
  group_by(Status.U5MR) %>%
  mutate(    across( `Births (thousands)`,
      ~ replace_na(as.numeric(str_remove_all(., ",")), 0)  ) ) %>%
  ungroup() %>%
  select(`ISO3 Alpha-code`,OfficialName,Year,Status.U5MR,
         `Births (thousands)`,SBA,ANC4,`Region, subregion, country or area *`, `Parent code`) %>%
    mutate(TB_ANC4 = `Births (thousands)` * ANC4/100,
         TB_SBA = `Births (thousands)` * SBA/100,
         `Births (thousands) ANC4` = ifelse(is.na(ANC4),0,`Births (thousands)`),
         `Births (thousands) SBA` = ifelse(is.na(SBA),0,`Births (thousands)`)) 

percentages = unicef %>% 
  filter(Year == 2022) %>%
  group_by(Status.U5MR) %>% 
  summarise(
    '% of women (aged 15–49) with at least 4 antenatal care visits' =100 * sum(TB_ANC4, na.rm = TRUE)/
      sum(`Births (thousands) ANC4`, na.rm = TRUE),
    '% of deliveries attended by skilled health personnel' = 100 * sum(SBA, na.rm = TRUE)/sum(`Births (thousands) SBA`, na.rm = TRUE)  ) 

#------------------------
# Load and merge spatial data for mapping
#------------------------
world <- ne_countries(scale = "medium", returnclass = "sf")

# Define color palette
cluster_colors <- c("Off-track" = "#E74C3C", "On-track" = "#1CABE2")
unicef_blue <- "#1CABE2"
offtrack_color <- "#D73027"
ontrack_color <- "#1A9850"
ontrack_label_color <- "#009B95"

# Get the most recent year for each country
unicef_recent <- unicef %>%
  group_by(`ISO3 Alpha-code`) %>%
  filter(Year == max(Year)) %>%
  ungroup()

# Join with world data
mapdata_filtered <- world %>%
  left_join(unicef_recent, by = c("iso_a3" = "ISO3 Alpha-code")) %>%
  mutate(
    dense = continent %in% c("EUR", "AFR", "SE_ASIA"),
    label_color = ifelse(Status.U5MR == "On-track", "OnLabel", "OffLabel")
  )

#------------------------
# Plot 1: World map with weights and U5MR status
#------------------------
ggplot() +
  geom_sf(data = filter(mapdata_filtered, is.na(TB_ANC4)),
          fill = "gray80", color = "white", size = 0.2) +
  geom_sf(data = filter(mapdata_filtered, !is.na(TB_ANC4)),
          aes(fill = TB_ANC4), color = "white", size = 0.2) +
  geom_sf(data = filter(mapdata_filtered, !is.na(TB_ANC4)),
          aes(color = Status.U5MR), fill = NA, size = 0.4) +
  geom_text_repel(data = filter(mapdata_filtered, !is.na(TB_ANC4)),
                  aes(geometry = geometry, label = iso_a3, color = label_color),
                  stat = "sf_coordinates",
                  size = 3, fontface = "bold",
                  box.padding = 0.3, max.overlaps = Inf,
                  segment.color = "gray50", bg.color = "white", bg.r = 0.15,
                  seed = 42, show.legend = FALSE) +
  scale_fill_gradient(low = "#f7fcf5", high = "#00441b", 
                      name = "Total Births", 
                      na.value = "gray80",
                      labels = scales::percent_format(scale = 1)) +
  scale_color_manual(
    name = "U5MR Status",
    values = c(
      "Off-track" = offtrack_color,
      "On-track" = ontrack_color,
      "OnLabel" = ontrack_label_color,
      "OffLabel" = offtrack_color),
    labels = c(
      "Off-track" = "Not on track",
      "On-track" = "On track",
      "OnLabel" = "",
      "OffLabel" = ""
    ),
    guide = guide_legend(override.aes = list(fill = NA))
  ) +
  labs(title = "Coverage of Antenatal Care (4+ Visits) Among Women Aged 15-49 in 2022",
       caption = "Source: UNICEF Data") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5, margin = margin(b = 15)),
        legend.position = "right",
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.caption = element_text(margin = margin(t = 10)))

# map 2
ggplot() +
  geom_sf(data = filter(mapdata_filtered, is.na(TB_SBA)),
          fill = "gray80", color = "white", size = 0.2) +
  geom_sf(data = filter(mapdata_filtered, !is.na(TB_SBA)),
          aes(fill = TB_SBA), color = "white", size = 0.2) +
  geom_sf(data = filter(mapdata_filtered, !is.na(TB_SBA)),
          aes(color = Status.U5MR), fill = NA, size = 0.4) +
  geom_text_repel(data = filter(mapdata_filtered, !is.na(TB_SBA)),
                  aes(geometry = geometry, label = iso_a3, color = label_color),
                  stat = "sf_coordinates",
                  size = 3, fontface = "bold",
                  box.padding = 0.3, max.overlaps = Inf,
                  segment.color = "gray50", bg.color = "white", bg.r = 0.15,
                  seed = 42, show.legend = FALSE) +
  scale_fill_gradient(low = "#f7fcf5", high = "#00441b", 
                      name = "Total Births", 
                      na.value = "gray80",
                      labels = scales::percent_format(scale = 1)) +
  scale_color_manual(
    name = "U5MR Status",
    values = c(
      "Off-track" = offtrack_color,
      "On-track" = ontrack_color,
      "OnLabel" = ontrack_label_color,
      "OffLabel" = offtrack_color),
    labels = c(
      "Off-track" = "Not on track",
      "On-track" = "On track",
      "OnLabel" = "",
      "OffLabel" = ""
    ),
    guide = guide_legend(override.aes = list(fill = NA))
  ) +
  labs(title = "Coverage of deliveries attended by skilled health personnel in 2022",
       caption = "Source: UNICEF Data") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5, margin = margin(b = 15)),
        legend.position = "right",
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.caption = element_text(margin = margin(t = 10)))


#------------------------
# Create time series data: annual % change by region and U5MR status
#------------------------
unicefVariation = unicef %>% 
  left_join(estimates %>%
              filter(Type == "Subregion") %>%
              distinct(`Region, subregion, country or area *`, `Location code`, `Parent code`) %>%
              rename(Subregion = `Region, subregion, country or area *`) %>%
              left_join(estimates %>%
                          filter(Type == "Region") %>%
                          distinct(`Region, subregion, country or area *`, `Location code`) %>%
                          rename(Region = `Region, subregion, country or area *`,
                                 `Parent code` = `Location code`)) %>%
              select(-`Parent code`) %>%
              rename(`Parent code` = `Location code`)) %>% 
  group_by(Year, Region, Status.U5MR) %>%
  summarise(Total = sum(TB_SBA)) %>%
  group_by(Region, Status.U5MR) %>%
  mutate(Total_diff = Total - lag(Total),
         Total_pct_change = (Total - lag(Total)) / lag(Total) * 100,
         Region = ifelse(is.na(Region), "NORTHERN AMERICA", Region)) %>%
  filter(!is.na(Total_pct_change))


#------------------------
# Plot 2: Time series of % population change
#------------------------
ggplot(unicefVariation, aes(x = as.integer(Year), y = Total_pct_change, color = Region, group = Region)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_text_repel(aes(label = paste0(round(Total_pct_change, 1), "%")),
                  size = 4,
                  fontface = "bold",
                  max.overlaps = 50,
                  show.legend = FALSE) +
  facet_wrap(~ Status.U5MR, scales = "fixed", labeller = as_labeller(c(
    "On-track" = "On-track",
    "Off-track" = "Off-track"))) +
  labs(
    title = "Annual % Change in Coverage of Total Deliveries Attended by Skilled Health Personnel (2018–2022)",
    x = "Year",
    y = "% Change",
    color = "Region",
    caption = "Source: UNICEF Population Data, 2018–2022") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 13),
        strip.text = element_text(size = 14, face = "bold"),
        legend.title = element_text(face = "bold"))

