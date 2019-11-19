library(dplyr)
library(tidyr)
library(stringr)

CombinedQoL_data <- read.csv("./data/CombinedQoLData.csv")
CombinedQoL_data$Country <- str_to_title(CombinedQoL_data$Country)

Summary_table <- CombinedQoL_data %>%
  group_by(Country) %>%
  summarise(
    Pop_rate = sum(diff(Pop), na.rm = TRUE) / (2014-1949),
    Life_rate = sum(diff(LifeTime), na.rm = TRUE) / (2014-1949),
    Co2_emission_rate = sum(diff(Total), na.rm = TRUE) / (2014-1949)
  )
