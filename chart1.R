library(shiny)
library(plotly)
library(tidyr)
library(dplyr)

# This chart was intended to show the relationship between
# CO2 emmisions per capita and GDP per capita for
# diffrent countries in the world from 1800-2016.
# It is pretty obvious that the U.S. always in a
# very high position on both GDP per capita and
# CO2 emission per capita after 1900.

co_emission_data <- read.csv("./data/GDP&CO2.csv")

co_emission_data[is.na(co_emission_data)] <- 0

co_emission_data_nw <- co_emission_data %>%
  filter(Year >= 1800 & Year <= 2016)
View(co_emission_data_nw)
ui <- fluidPage(
  h1("CO2 emissions per capita vs GDP per capita"),

  sidebarLayout(
    sidebarPanel(
      sliderInput("year", "Year",
        min = min(co_emission_data_nw$Year),
        max = max(co_emission_data_nw$Year), value = 2000
      ),
      verbatimTextOutput("Text")
    ),

    mainPanel(
      plotlyOutput("BubbleChart")
    )
  )
)
# Since both China and India has very high population, I
# choose to calculate the parameter of those markers in
# a slight different way so that they won't take over the whole
# chart.
server <- function(input, output) {
  output$BubbleChart <- renderPlotly({
    co_emission_year <- co_emission_data_nw %>%
      filter(Year == input$year)

    co_emission_year[
      co_emission_year$Entity == "China" |
        co_emission_year$Entity == "India",
      "Total_population"
    ] <-
      co_emission_year[
        co_emission_year$Entity == "China" |
          co_emission_year$Entity == "India",
        "Total_population"
      ] * 5 / 100


    plot_ly(co_emission_year,
      x = co_emission_year$GDP_per_capita,
      y = co_emission_year$CO2_emissions_per_capita,
      color = co_emission_year$Entity, type = "scatter", mode = "markers",
      marker = list(
        symbol = "cirlce", sizemode = "diameter", opacity = 0.8,
        size = co_emission_year$Total_population * 5 / 20000000
      ),
      source = "plot",
      key = co_emission_year$Entity
    ) %>%
      layout(
        xaxis = list(title = "GDP per capita", range = c(
          0,
          max(co_emission_year$GDP_per_capita)
        )),
        yaxis = list(
          title = "CO2 emissions per capita (in tons)",
          range = c(0, max(co_emission_year$CO2_emissions_per_capita) + 5)
        )
      )
  })

  output$Text <- renderText({
    data <- event_data("plotly_hover", source = "plot")
    if (is.null(data) == T) {
      return(NULL)
    }

    paste0(
      "Country: ", data$key, "\n", "CO2 emission: ",
      round(data$y, digits = 2), "\n",
      "GDP per capita: ", round(data$x, digits = 2)
    )
  })
}
shinyApp(ui, server)
