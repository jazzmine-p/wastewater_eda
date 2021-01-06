library(tidyverse)
library(dplyr)
library(readxl)
library(ggplot2)
library(scales)

# Import data
wastewater_data <- read_excel(here::here("covidupdate_202012211450194509.xlsx"))
wastewater_data

# Data overview
summary(wastewater_data)
str(wastewater_data)
tibble::glimpse(wastewater_data)

# Find date range
min(wastewater_data$Date)
max(wastewater_data$Date)

# Q1: How do the average wastewater viral loads in NCCo change before and after Thanksgiving ?
before_tday <- seq(as.Date("2020-11-12"), as.Date("2020-11-26"), "days")
before_tday_wastewater <- wastewater_data %>%
  filter(as.Date(Date) %in% before_tday) %>%
  group_by(`Station Friendly Name`) %>%
  summarize(avg_viral_levels_before_tday = round(mean(`SARS-CoV-2 Copies/L`))) %>%
  ungroup() %>%
  arrange(desc(avg_viral_levels_before_tday))

after_tday <- seq(as.Date("2020-11-27"), as.Date("2020-12-10"), "days")
after_tday_wastewater <- wastewater_data %>%
  filter(as.Date(Date) %in% after_tday) %>%
  group_by(`Station Friendly Name`) %>%
  summarize(avg_viral_levels_after_tday = round(mean(`SARS-CoV-2 Copies/L`))) %>%
  ungroup() %>%
  arrange(desc(avg_viral_levels_after_tday))

tday_wastewater <- full_join(before_tday_wastewater, after_tday_wastewater, by = "Station Friendly Name") %>%
  mutate(difference = avg_viral_levels_after_tday - avg_viral_levels_before_tday) %>%
  arrange(desc(difference)) 

# Plot Q1
tday_wastewater %>%
  ggplot(aes(x = difference,
             y = `Station Friendly Name`)) +
  labs(title = "Changes in Viral Loads Two Weeks Before and After Thanksgiving") +
  geom_col() +
  theme_minimal() +
  scale_x_continuous(labels = comma)

