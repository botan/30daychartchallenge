# Load packages
library(tidyverse)
library(scales)
library(showtext)
library(here)

# Load font
font_add_google("Bitter")
font_add_google("Kadwa")
showtext_auto()

# Load data
url <- "http://berkeleyearth.lbl.gov/auto/Global/Complete_TAVG_summary.txt"
global_warming_raw <- read_tsv(url, skip = 20)
global_warming_raw

# Data cleaning
global_warming <- global_warming_raw %>% 
  rename("year anomaly anomaly_unc. five_year_anomaly five_year_unc." = `% Year, Annual Anomaly, Annual Unc., Five-year Anomaly, Five-year Unc.`) %>% 
  mutate(`year anomaly anomaly_unc. five_year_anomaly five_year_unc.` = str_squish(`year anomaly anomaly_unc. five_year_anomaly five_year_unc.`)) %>% 
  separate(`year anomaly anomaly_unc. five_year_anomaly five_year_unc.`,
           into = c("year", "anomaly", "anomaly_unc", "five_year_anomaly", "five_year_unc."), 
           sep = " ") %>% 
  select(year, anomaly) %>% 
  mutate(across(where(is.character), parse_number)) %>% 
  arrange(desc(year))
global_warming

# Palette for strips
pal_strip <- c(
  "#00264d",
  "#2a486f",
  "#3f6ca6",
  "#8cbbd9",
  "#c3e4ef",
  "#eaf7fa",
  "#fedc81",
  "#fda64e",
  "#f25626",
  "#cc0000",
  "#990024",
  "#660018"
)

# Make plot
global_warming %>% 
  filter(year >= 1900) %>%
  ggplot(aes(0, 0, colour = anomaly, size = year)) + 
  geom_point(show.legend = FALSE) + 
  scale_radius(range = c(1, 139)) +
  scale_colour_gradientn(colours = pal_strip) +
  labs(
    title = "Global Surface Temperature Anomaly\n(1900 \u2013 2020)",
    caption = "#30DayChartChallenge\nData: Berkeley Earth\nPlot: Botan Ağın"
  ) +
  theme_void(base_family = "Kadwa") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, colour = "white"),
    plot.caption = element_text(hjust = 0.5, colour = "white"),
    plot.background = element_rect(fill = "black"),
    plot.margin = margin(1, 1, 0.5, 1, unit = "cm")
  )
ggsave(here("plots", "day6-experimental.png"), width = 6, height = 7, dpi = 320)