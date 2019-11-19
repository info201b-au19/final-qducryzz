library(dplyr)
library(tidyr)
library(stringr)

# This table talks about the growth rate of population
# lifetime, and CO2 emission for all the countries in
# the world from 1949 to 2014. With sum of the all diff,
# we get overall change for 1949 to 2014. Divided by the time
# interval, we get the the rate of change of Population, life_expectancy,
# and CO2 emission from 1949 to 2014.
# Important info:
# 1. Based on the table, it is clear to see that the population
# change rate is directly proportional to the CO2 emission rate.
# 2. Compare to two other columns(population change rate and
# CO2 emission change rate), the lifetime doesn't change that
# much with a less than one year increase each year from
# 1949 to 2014.
CombinedQoL_data <- read.csv("./data/CombinedQoLData.csv")
CombinedQoL_data$Country <- str_to_title(CombinedQoL_data$Country)

Summary_table <- CombinedQoL_data %>%
  group_by(Country) %>%
  summarise(
    Pop_rate = sum(diff(Pop), na.rm = TRUE) / (2014-1949),
    Life_rate = sum(diff(LifeTime), na.rm = TRUE) / (2014-1949),
    Co2_emission_rate = sum(diff(Total), na.rm = TRUE) / (2014-1949)
  )
