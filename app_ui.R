library(shiny)
library(tidyr)
library(dplyr)
library(stringr)
library(plotly)
library(rmarkdown)
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

Renewable_ene$Country <- str_replace(Renewable_ene$Country, "0", " ")

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
    "<p>Descriptions: 
    <ol>
<li> The purpose of the first page is to show audiences  
the relationship between CO2 emissions and GDP per capita 
in both global and individual countries' perspectives,
which helps people to understand how much does 
CO2 emission influences GDP per capita. </li>

<li> For the first scatter plot, 
by using the mean value of each countries’ CO2 emission 
and GDP per capita, it’s obvious to see that most counties 
lie in the CO2 emission of 2millions tons and GDP of 50K USD area, 
along with a positive relationship between two variables. 
However, there are some outliers like Kuwait, UAE, and Qatar, 
which have much higher CO2 emissions and GDP per capita as
Qatar is the extremest one in the chart. </li>

<li> By choosing a specific country, 
it is clear for audiences to see that CO2 emission level and 
GDP per capita has a very similar 
increasing and decreasing pattern. 
Take the US as an example, 
the charts of both CO2 emissions and GDP per capita 
show a highly similar trend as the increase was pretty 
slow before 1900 and started surging after 1932. </li> 
    </ol>
    </p>"
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
  tags$hr(),
  plotlyOutput("data_two_HDI_line"),
  tags$hr(),
  plotlyOutput("data_two_Renewable_line")
)

data_page_two_control <- sidebarPanel(
  selectInput("data_page_two_country", "Country",
    choices = unique(HDI_Renewable$Country),
    selected = unique(HDI_Renewable$Country[2])
  ),
  HTML(
    "<p>Descriptions: 
    <ol>
    
<li> The purpose of the second page is to show audiences 
the relationship between renewable energy production 
and Human Development Index as we want to know that 
whether or not does higher renewable energy consumption 
mean a higher standard of living (eg. HDI). </li>

<li> From the first chart, we can see a general downward 
trend of higher HDI links to lower renewable energy 
consumption. However, there are also some countries 
like Iceland, Liechtenstein, and Norway, which have 
both relatively high HDI levels and renewable energy consumption. </li>

<li> Selecting a specific country gives readers the 
chance of getting to know the overall trend of HDI 
and renewable energy consumption in that given 
country since different countries tend to have 
different relationships regarding those variables. 
For example, in Afghanistan, the HDI level kept increasing 
while renewable energy consumption generally kept declining
(with a slight increase from 2011-2013) from 2002 to 2014. </li> 
    
    </ol>
    </p>"
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
  tags$hr(),
  plotlyOutput("data_three_year_emission"),
  tags$hr(),
  plotlyOutput("data_three_energy_usage_pie")
)

data_page_three_control <- sidebarPanel(
  selectInput("data_page_three_year", "Year",
    choices = unique(Emission_Renewable$Year)
  ),
  selectInput("data_page_three_country", "Country", choices = list()),
  HTML(
    "<p>Descriptions: 
    <ol>
<li> The third page contains three charts about global 
CO2 emissions, and it reveals an increasing trend 
using the line chart since 1990 with a slight 
drop around 2008 and 3.2 million in 2014. </li>

<li> The bar chart gives an overall insight 
into global total CO2 consumption. 
By selecting a different year from the sidebar, 
the chart changes with the different total 
amount in that year. Besides, audiences can 
zoom in to have a closer look at the rank 
of different countries. </li>

<li> By choosing the year and country, 
the pie chart at the bottom can let audiences s
ee the breakdown of that chosen country’s 
energy consumption as the orange part 
represents the percent of both renewable 
energy and Nonrenewable energy consumption 
in regards to the total energy consumption. 
For example, in the year of 2009, 25.9% of 
Iceland’s energy consumption was nonrenewable 
consumption while 74.1% of the energy consumption
was renewable energy consumption. </li>
    </ol>
    </p>"
  )
)

data_page_three <- tabPanel(
  "Global CO2 Emission Level  ",
  sidebarLayout(
    data_page_three_control,
    data_page_three_graph
  )
)

# Adding introduction tab
introduction <- tabPanel(
  "Introduction",
  sidebarLayout(
    sidebarPanel(HTML("<p> <span id = 'link_title'>Data sources: </span><br>
                  
    <ul>
    <li> <a href = 'https://ourworldindata.org/co2-and-other-greenhouse-gas-emissions'>
         GDP per capita v.s. CO2 emission dataset</a> </li>   
    <li> <a href = 'http://hdr.undp.org/en/data'> Human Development Index </a> </li>
    <li> <a href = 'http://hdr.undp.org/en/composite/HDI'> 
         The United Nations Human Development Reports Office, <br>
         Composite index of life expectancy, eductaion, and per 
         capita income </a> </li>
    <li> <a href = 'https://data.world/oecd/renewable-energy'> 
         Renewable Energy Production </a> </li>
    <li> <a href = 'https://ourworldindata.org/renewable-energy'>The World Bank, 
         and the International Energy Agency, <br>
         percentage of renewable energy production</a> </li>
    <li> <a href = 'https://datahub.io/core/co2-fossil-by-nation#read'> CO2 Emissions 
         The Carbon Dioxide Information Analysis Center (CDIAC) <br>
         total CO2 emissions of each country in million metric tons </a> </li>
    </ul>
    </p>")),
    mainPanel(
      img(src = './co2.0.jpg',alt = 'Intro image'),
      HTML("
      <div class = 'intro'>
         Industrialization has allowed developed countries 
         to dramatically increase the size and scale of their 
         economies and well as improve the quality of life of their
         citizens. These developed countries have benefited
         from decades of fossil fuel powered economic growth;
         their prosperity fueled via the pollution of the
         environment. In this project, we seek to figure out 
         if it is in a countries best interest to use 
         fossil fuels by looking at the relationships between 
         Goss Domestic Product (GDP), the Human Development Index (HDI
         renewable energy production, and CO2 emissions.
      </div>
      ")
    )
  )
)


# Adding empty (for now) summary tab
summary <- tabPanel(
  "Summary",
  img(src = './images.png', alt = 'Summary Image', id = "Sum_image"),
  mainPanel(
   includeMarkdown("summary.md")
  )
)

ui <- navbarPage(
  HTML("<span class = Title><b>Final Project</b><span>"),
  introduction,
  data_page_one,
  data_page_two,
  data_page_three,
  summary,
  includeCSS("./CSS/Final.css")
)
