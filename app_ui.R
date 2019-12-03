library(shiny)
library(tidyr)
library(dplyr)
library(stringr)

# Adding empty (for now) introduction tab
introduction <- tabPanel(
  titlePanel("Introduction"),
  sidebarLayout(
    sidebarPanel(h5(HTML("<p>Data sources: <br>
                  GDP -  
                  <br>
                  Human Development Index - <br>
                  The United Nations Human Development Reports Office, <br>
                  Composite index of life expectancy, eductaion, and per capita income <br>
                  <br>
                  Renewable Energy Production - <br>
                  The World Bank, and the International Energy Agency, <br>
                  percentage of renewable enrgy production for a country <br>
                  <br>
                  **[CO2 Emissions](https://datahub.io/core/co2-fossil-by-nation#readme):** - <br>
                  The Carbon Dioxide Information Analysis Center (CDIAC) <br>
                  total CO2 emissions of each country in million metric tons <br> <p>"))),
    mainPanel(
      h3("Developed countires have benefited from decades and centuries of industrialization 
             fueled primarily by fossil fuels. This advancment allowed them to provide their citizens 
             with a quality of life previously unheard of, polluting the collective global atmosphere 
             for their individual gain and to facilitate the creation of seemingly endless amounts of 
             cheap energy. Contrarily this had allowed developed countries to begin to develop  cheap 
             renewable energy and move away from fossil fuels. The universal desire of devleoping coutries 
             to themselves extract their resources to better the lives of their citizens, while obviously 
             damaging to the planet, is therefore understandable. In order to show these relationships 
             we focused upon the interaction between CO2 emissions, a countries GDP, a countries adobtion 
             of green energy, and the Human Development Index used by the UN.")
    )
  )
)

# Take CO2_emission and reorganized the data
CO2_emission <- read.csv("./data/GDP&CO2.csv")
CO2_emission <- CO2_emission %>%
  filter(CO2_emissions_per_capita != 0) %>%
  filter(!is.na(GDP_per_capita))

# Take HDI_emission and reorganized the data
HDI <- read.csv("./data/Human_development_index_(HDI).csv")
HDI <- HDI %>%
  gather(key = "Country", value = "HDI", -Year) %>%
  select(Country, Year, HDI)
HDI$Country <- str_to_title(HDI$Country)

# Take Renewable Energy Production and reorganized the data
Renewable_ene <- read.csv("./data/Renewable_Energy_Production.csv")
Renewable_ene <- Renewable_ene %>%
  gather(key = "Year", value = "Renewable", -Country)

Renewable_ene$Year <- Renewable_ene$Year %>%
  str_replace("X", "") %>%
  as.numeric()

# Combine HDI & Renewable Energy
HDI_Renewable <- HDI %>%
  left_join(Renewable_ene, by = c("Country", "Year")) %>%
  na.omit() %>%
  filter(HDI != 0) %>%
  filter(Renewable != 0)

# Take CO2_total_emission & Renewable
CO2_emission_total <- read.csv("./data/fossil-fuel-co2-emissions-by-nation.csv")
CO2_emission_total <- CO2_emission_total %>%
  select(Year, Country, Total)

CO2_emission_total$Country <- str_to_title(CO2_emission_total$Country)

# Combine the data 
Emission_Renewable <- Renewable_ene %>%
  left_join(CO2_emission_total, by = c("Country","Year")) %>%
  na.omit()


# CO2&GDP Graph (Page 1)
data_page_one_graph <- mainPanel(
  plotlyOutput("data_one_scatter"),
  plotlyOutput("data_one_CO2_line"),
  plotlyOutput("data_one_GDP_line")
)
data_page_one_control <- sidebarPanel(
  selectInput("data_page_one_country", "Country", choices = CO2_emission$Entity)
)
data_page_one <- tabPanel(
  "GDP & CO2 Emission Level ",
  sidebarLayout(
    data_page_one_control,
    data_page_one_graph
  )
)

data_page_two_graph <- mainPanel(
  plotlyOutput("data_two_scatter"),
  plotlyOutput("data_two_HDI_line"),
  plotlyOutput("data_two_Renewable_line")
)

data_page_two_control <- sidebarPanel(
  selectInput("data_page_two_country", "Country", choices = HDI_Renewable$Country)
)

data_page_two <- tabPanel(
  "Renewable Consumption & HDI ",
  sidebarLayout(
    data_page_two_control,
    data_page_two_graph
  )
)

data_page_three_graph <- mainPanel(
  plotlyOutput("data_three_total_emission"),
  plotlyOutput("data_three_year_emission"),
  plotlyOutput("data_three_energy_usage_pie")
)

data_page_three_control <- sidebarPanel(
  selectInput("data_page_three_year", "Year", choices = unique(Emission_Renewable$Year)),
  selectInput("data_page_three_country", "Country", choices = list())
)

data_page_three <- tabPanel(
  "Global CO2 Emission Level  ",
  sidebarLayout(
    data_page_three_control,
    data_page_three_graph
  )
)

# Adding empty (for now) summary tab
summary <- tabPanel(
  "Summary",
)

ui <- navbarPage(
  "Final Project",
  introduction,
  data_page_one,
  data_page_two,
  data_page_three,
  summary
)

