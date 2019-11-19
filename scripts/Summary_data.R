library(dplyr)
library(tidyr)
library("stringr")

co2_emission_data <- read.csv("../data/fossil-fuel-co2-emissions-by-nation.csv")
human_development_data <- read.csv("../data/Human_development_index_(HDI).csv")

CombinedQoL_data <- read.csv("../data/CombinedQoLData.csv")
CombinedQoL_data$Country <- str_to_title(CombinedQoL_data$Country)

highest_Co2_country <- co2_emission_data %>%
  group_by(Country) %>%
  summarize(
    total_emission = sum(Total)
  ) %>%
  arrange(-total_emission) %>%
  head(1)

highest_Co2_year <- co2_emission_data %>%
  group_by(Year) %>%
  summarise(
    total_emission = sum(Total)
  ) %>%
  arrange(-total_emission) %>%
  head(1)

highest_HDI_rate_country <- human_development_data %>%
  gather(key = "Country", value = "HDI", -Year) %>%
  mutate(
     HDI_num = as.numeric(HDI)
  ) %>%
  group_by(Country) %>%
  summarize(
    rate = sum(diff(HDI_num), na.rm = TRUE) / (2017 - 1990)
  ) %>%
  arrange(-rate) %>%
  head(1)


Summary_table <- CombinedQoL_data %>%
  group_by(Country) %>%
  summarise(
    Pop_rate = sum(diff(Pop), na.rm = TRUE) / (2014-1949),
    Life_rate = sum(diff(LifeTime), na.rm = TRUE) / (2014-1949),
    Co2_emission_rate = sum(diff(Total), na.rm = TRUE) / (2014-1949)
  )


highest_Pop_rate_country <- Summary_table %>%
  filter(Pop_rate == max(Pop_rate))

highest_life_rate_country <- Summary_table %>%
  filter(Life_rate == max(Life_rate))
