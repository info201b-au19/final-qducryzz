
library(shiny)
library(tidyr)
library(dplyr)
library(stringr)
library(plotly)

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
  left_join(CO2_emission_total, by = c("Country", "Year")) %>%
  na.omit()


# CO2&GDP Graph (Page 1)
data_page_one_graph <- mainPanel(
  plotlyOutput("data_one_scatter"),
  tags$hr(),
  plotlyOutput("data_one_CO2_line"),
  tags$hr(),
  plotlyOutput("data_one_GDP_line")
)
data_page_one_control <- sidebarPanel(
  selectInput("data_page_one_country", "Country",
    choices = unique(CO2_emission$Entity),
    selected = unique(CO2_emission$Entity)[1]
  ),
  HTML(
    "<p>Descriptions: <br>
1. The purpose of the first page is to show audiences  
the relationship between CO2 emissions and GDP per capita 
in both global and individual countries' perspectives,
which helps people to understand how much does 
CO2 emission influences GDP per capita. <br>

2. For the first scatter plot, 
by using the mean value of each countries’ CO2 emission 
and GDP per capita, it’s obvious to see that most counties 
lie in the CO2 emission of 2millions tons and GDP of 50K USD area, 
along with a positive relationship between two variables. 
However, there are some outliers like Kuwait, UAE, and Qatar, 
which have much higher CO2 emissions and GDP per capita as
Qatar is the extremest one in the chart. <br>

3. By choosing a specific country, 
it is clear for audiences to see that CO2 emission level and 
GDP per capita has a very similar 
increasing and decreasing pattern. 
Take the US as an example, 
the charts of both CO2 emissions and GDP per capita 
show a highly similar trend as the increase was pretty 
slow before 1900 and started surging after 1932. </p>"
  )
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
  selectInput("data_page_two_country", "Country",
    choices = unique(HDI_Renewable$Country),
    selected = unique(HDI_Renewable$Country[2])
  ),
  HTML(
    "<p>Descriptions: <br>
1. The purpose of the second page is to show audiences 
the relationship between renewable energy production 
and Human Development Index as we want to know that 
whether or not does higher renewable energy consumption 
mean a higher standard of living (eg. HDI). <br>

2. From the first chart, we can see a general downward 
trend of higher HDI links to lower renewable energy 
consumption. However, there are also some countries 
like Iceland, Liechtenstein, and Norway, which have 
both relatively high HDI levels and renewable energy consumption. <br>

3. Selecting a specific country gives readers the 
chance of getting to know the overall trend of HDI 
and renewable energy consumption in that given 
country since different countries tend to have 
different relationships regarding those variables. 
For example, in Afghanistan, the HDI level kept increasing 
while renewable energy consumption generally kept declining
(with a slight increase from 2011-2013) from 2002 to 2014. </p>"
  )
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
  selectInput("data_page_three_year", "Year",
    choices = unique(Emission_Renewable$Year)
  ),
  selectInput("data_page_three_country", "Country", choices = list()),
  HTML(
    "<p>Descriptions: <br>
1. The purpose of the first page is to show audiences  
the relationship between CO2 emissions and GDP per capita 
in both global and individual countries' perspectives,
which helps people to understand how much does 
CO2 emission influences GDP per capita. <br>

2. For the first scatter plot, 
by using the mean value of each countries’ CO2 emission 
and GDP per capita, it’s obvious to see that most counties 
lie in the CO2 emission of 2millions tons and GDP of 50K USD area, 
along with a positive relationship between two variables. 
However, there are some outliers like Kuwait, UAE, and Qatar, 
which have much higher CO2 emissions and GDP per capita as
Qatar is the extremest one in the chart. <br>

3. By choosing a specific country, 
it is clear for audiences to see that CO2 emission level and 
GDP per capita has a very similar 
increasing and decreasing pattern. 
Take the US as an example, 
the charts of both CO2 emissions and GDP per capita 
show a highly similar trend as the increase was pretty 
slow before 1900 and started surging after 1932. </p>"
  )
)

data_page_three <- tabPanel(
  "Global CO2 Emission Level  ",
  sidebarLayout(
    data_page_three_control,
    data_page_three_graph
  )
)

ui <- navbarPage(
  "Final Project",
  data_page_one,
  data_page_two,
  data_page_three
)

# Adding introduction tab
introduction <- tabPanel(
  "Introduction",
  sidebarLayout(
    sidebarPanel(h5(HTML("<p>Data sources: <br>
                  <ul>
                  <li> GDP </li>   
                  <li> Human Development Index </li>
                  <li> The United Nations Human Development Reports Office, <br>
                  Composite index of life expectancy, eductaion, and per capita income </li>
                  <li> Renewable Energy Production </li>
                  <li> The World Bank, and the International Energy Agency, <br>
                  percentage of renewable enrgy production for a country </li>
                  <li> <a href = 'https://datahub.io/core/co2-fossil-by-nation#read'> CO2 Emissions 
                  The Carbon Dioxide Information Analysis Center (CDIAC) <br>
                  total CO2 emissions of each country in million metric tons </a> </li>
                  </ul>
                  </p>"))),
    mainPanel(
      HTML("
      <div class = 'intro'>
          Developed countires have benefited from decades and centuries of industrialization 
          fueled primarily by fossil fuels. This advancment allowed them to provide their citizens 
          with a quality of life previously unheard of, polluting the collective global atmosphere 
          for their individual gain and to facilitate the creation of seemingly endless amounts of 
          cheap energy. Contrarily this had allowed developed countries to begin to develop  cheap 
          renewable energy and move away from fossil fuels. The universal desire of devleoping coutries 
          to themselves extract their resources to better the lives of their citizens, while obviously 
          damaging to the planet, is therefore understandable. In order to show these relationships 
          we focused upon the interaction between CO2 emissions, a countries GDP, a countries adobtion 
          of green energy, and the Human Development Index used by the UN.
      </div>
      ")
    )
  )
)



# Adding empty (for now) summary tab
summary <- tabPanel(
  "Summary",
    mainPanel(
      includeMarkdown("summary.md"),
    )
)

ui <- navbarPage(
  "Final Project",
  introduction,
  data_page_one,
  data_page_two,
  data_page_three,
  summary
)

