library(dplyr)
library(tidyr)
library("stringr")

electricity_access_data <- read.csv("./data/Electricity_Access_Percent_Population.csv")
co2_emission_data <- read.csv("./data/fossil-fuel-co2-emissions-by-nation.csv")
human_development_data <- read.csv("./data/Human_development_index_(HDI).csv")
renewable_energy_data <- read.csv("./data/Renewable_Energy_Production.csv")

electricity_access_data[is.na(electricity_access_data)] <- 0
co2_emission_data[is.na(co2_emission_data)] <- 0
human_development_data[is.na(human_development_data)] <- 0
renewable_energy_data[is.na(renewable_energy_data)] <- 0

# Standardlize the format of the electricity_access_data
electricity_access_data_nw <- electricity_access_data %>%
  gather(key = "Year", value = "Percent_Renewable", -Country.Name)

# Standardlize the Year format in electricity_access_data
colnames(electricity_access_data_nw)[1] <- 'Country'
electricity_access_data_nw$Year <- str_replace(electricity_access_data_nw$Year,"X","")
electricity_access_data_nw$Year <- as.numeric(electricity_access_data_nw$Year)

# Standardlize the country format in CO2 emission
co2_emission_data_nw <- co2_emission_data
co2_emission_data_nw$Country <- str_to_title(co2_emission_data_nw$Country)
co2_emission_data_nw$Year <- as.numeric(co2_emission_data_nw$Year)

# Standardlize the format of the human_development_data 
human_development_data_nw <- human_development_data %>%
  gather(key = "Country", value = "HDI", -Year) %>%
  filter(Year > 0)

# Uniform the Country Name
human_development_data_nw$Country <- str_to_title(human_development_data_nw$Country)
human_development_data_nw$Year <- as.numeric(human_development_data_nw$Year)

# Standardlize the format of the renewable_energy_data
renewable_energy_data_nw <- renewable_energy_data %>%
  gather(key = "Year", value = "Renewable consumption", -1)

# Standardlize the Year format in renewable_energy_data
colnames(renewable_energy_data_nw)[1] <- 'Country'
renewable_energy_data_nw$Country <- str_to_title(renewable_energy_data_nw$Country)
renewable_energy_data_nw$Year <- str_replace(renewable_energy_data_nw$Year,"X", "")
renewable_energy_data_nw$Year <- as.numeric(renewable_energy_data_nw$Year)

# Combined the data into dataset
data_collection <- renewable_energy_data_nw %>%
  left_join(electricity_access_data_nw, by = c("Country","Year")) %>%
  left_join(human_development_data_nw, by = c("Country", "Year")) %>%
  left_join(co2_emission_data_nw, by = c("Country", "Year"))

data_collection[is.na(data_collection)] <- 0
data_collection$HDI <- as.numeric(data_collection$HDI)

data_collection[data_collection == 0] <- NA

data_collection_summary <- data_collection %>%
  group_by(Country) %>%
  
