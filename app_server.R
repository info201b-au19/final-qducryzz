library(shiny)
library(tidyr)
library(dplyr)
library(plotly)

server <- function(input, output, session) {
  # Take CO2_emission and reorganized the dataset
  CO2_emission <- read.csv("./data/GDP&CO2.csv")
  CO2_emission <- CO2_emission %>%
    filter(CO2_emissions_per_capita != 0) %>%
    filter(!is.na(GDP_per_capita)) %>%
    mutate(Emission = CO2_emissions_per_capita * GDP_per_capita)

  # Take HDI_emission and reorganized the dataset
  HDI <- read.csv("./data/Human_development_index_(HDI).csv")
  HDI <- HDI %>%
    gather(key = "Country", value = "HDI", -Year) %>%
    select(Country, Year, HDI)
  HDI$Country <- str_to_title(HDI$Country)

  # Take Renewable Energy Production and reorganized the dataset
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

  HDI_Renewable$HDI <- as.numeric(HDI_Renewable$HDI)

  # Take CO2_total_emission & Renewable
  CO2_emission_total <- read.csv("./data/fossil-fuel-co2-emissions-by-nation.csv")
  CO2_emission_total <- CO2_emission_total %>%
    select(Year, Country, Total)

  CO2_emission_total$Country <- str_to_title(CO2_emission_total$Country)

  # Combine the data
  Emission_Renewable <- Renewable_ene %>%
    left_join(CO2_emission_total, by = c("Country", "Year")) %>%
    na.omit()




  # Display the scatter plot for the first plot
  output$data_one_scatter <- renderPlotly({
    CO2_emission_sum <- CO2_emission %>%
      group_by(Entity) %>%
      summarise(
        CO2_emission_ave = mean(Emission),
        GDP_ave = mean(GDP_per_capita),
        population = mean(Total_population)
      )

    plot_ly(CO2_emission_sum,
      x = ~GDP_ave, y = ~CO2_emission_ave,
      color = ~Entity, text = ~ paste0(
        "Country: ", Entity, "<br>Average CO2 Emission: ",
        round(CO2_emission_ave, digits = 2),
        "T<br>Average GDP: ",
        round(GDP_ave, digits = 2),
        "$<br>Average Population: ", round(population)
      ),
      hoverinfo = "text"
    ) %>%
      layout(
        xaxis = list(title = "GDP per Capita (USD)"),
        yaxis = list(title = "CO2 Emission (Tons)"),
        title = "CO2 Emission v.s GDP per capita (Mean)"
      )
  })

  # Graph the CO2 emission by years in the given country
  output$data_one_CO2_line <- renderPlotly({
    CO2_emission_nw <- CO2_emission %>%
      filter(Entity == input$data_page_one_country)
    plot_ly(CO2_emission_nw,
      x = ~Year,
      y = ~Emission,
      type = "scatter",
      mode = "lines",
      text = ~ paste0(
        "Year: ", Year,
        "<br>CO2 Emission: ",
        round(Emission, digits = 2)
      ),
      hoverinfo = "text"
    ) %>%
      layout(
        xaxis = list(title = "Year"),
        yaxis = list(title = "CO2 Emission (Tons)"),
        title = paste0(input$data_page_one_country, " CO2 emission")
      )
  })

  # Graph the GDP change by years in the given country
  output$data_one_GDP_line <- renderPlotly({
    CO2_emission_nw <- CO2_emission %>%
      filter(Entity == input$data_page_one_country)
    plot_ly(CO2_emission_nw,
      x = ~Year,
      y = ~GDP_per_capita,
      type = "scatter",
      mode = "lines",
      text = ~ paste0(
        "Year: ", Year,
        "<br>GDP per capita: ",
        round(GDP_per_capita, digits = 2)
      ),
      hoverinfo = "text"
    ) %>%
      layout(
        xaxis = list(title = "Year"),
        yaxis = list(title = "GDP per capita (USD)"),
        title = paste0(
          input$data_page_one_country,
          " GDP per capita"
        )
      )
  })

  # Graph the scatter plot for the second graph
  output$data_two_scatter <- renderPlotly({
    HDI_Renewable_sum <- HDI_Renewable %>%
      group_by(Country) %>%
      summarise(
        HDI_ave = mean(HDI),
        Renewable_ave = mean(Renewable)
      )

    plot_ly(HDI_Renewable_sum,
      x = ~HDI_ave,
      y = ~Renewable_ave,
      color = ~Country,
      text = ~ paste0(
        "Country: ", Country,
        "<br>Average HDI: ", round(HDI_ave, digits = 2),
        "<br>Average Renewable Consumption: ",
        round(Renewable_ave, digits = 2), "%"
      ),
      hoverinfo = "text"
    ) %>%
      layout(
        xaxis = list(title = "HDI"),
        yaxis = list(title = "Renewable Consumption (%)"),
        title = "Renewable Consumption v.s HDI"
      )
  })

  # Graph the HDI change by years in the given country
  output$data_two_HDI_line <- renderPlotly({
    HDI_Renewable_nw <- HDI_Renewable %>%
      filter(Country == input$data_page_two_country)

    plot_ly(HDI_Renewable_nw,
      x = ~Year,
      y = ~HDI,
      type = "scatter",
      mode = "lines",
      text = ~ paste0(
        "Year: ",
        Year, "<br>HDI: ", HDI
      ),
      hoverinfo = "text"
    ) %>%
      layout(
        xaxis = list(title = "Year"),
        yaxis = list(title = "HDI"),
        title = paste0(
          input$data_page_two_country,
          " HDI"
        )
      )
  })

  # Graph the renewable energy consumption by years in the given country
  output$data_two_Renewable_line <- renderPlotly({
    HDI_Renewable_nw <- HDI_Renewable %>%
      filter(Country == input$data_page_two_country)

    plot_ly(HDI_Renewable_nw,
      x = ~Year,
      y = ~Renewable,
      type = "scatter",
      mode = "lines",
      text = ~ paste0(
        "Year: ", Year,
        "<br>Renewable Consumption: ",
        Renewable, "%"
      ),
      hoverinfo = "text"
    ) %>%
      layout(
        xaxis = list(title = "Year"),
        yaxis = list(title = "Renewable consumption (%)"),
        title = paste(input$data_page_two_country, " Renewable Energy Consumption")
      )
  })


  # Graph the Total CO2 emission in years
  output$data_three_total_emission <- renderPlotly({
    Emission_Renewable_sum <- Emission_Renewable %>%
      group_by(Year) %>%
      summarise(total_emission = sum(Total))

    plot_ly(Emission_Renewable_sum,
      x = ~Year,
      y = ~total_emission,
      type = "scatter",
      mode = "lines", text = ~ paste0(
        "Year: ", Year,
        "<br>Total CO2 Emission: ",
        total_emission, "T"
      ),
      hoverinfo = "text"
    ) %>%
      layout(
        xaxis = list(title = "Year"),

        yaxis = list(title = "Total CO2 Emission (Tons)"),
        title = "Global Total CO2 Emission"
      )
  })

  # Graph the annual CO2 emission
  output$data_three_year_emission <- renderPlotly({
    Emission_Renewable_nw <- Emission_Renewable %>%
      filter(Year == input$data_page_three_year)

    plot_ly(Emission_Renewable_nw,
      x = ~Total,
      y = ~ reorder(Country, Total),
      type = "bar",
      orientation = "h",
      text = ~ paste0(
        "Country: ", Country,
        "<br>CO2 Emission: ", Total, "T"
      ),
      hoverinfo = "text"
    ) %>%
      layout(
        xaxis = list(title = "Total CO2 Emission (Tons)"),
        yaxis = list(title = "Country"),
        title = paste0(
          "Total CO2 Emission in ",
          input$data_page_three_year
        )
      )
  })

  # Graph the country Energy consumption
  output$data_three_energy_usage_pie <- renderPlotly({
    Emission_Renewable_nw <- Emission_Renewable %>%
      filter(Year == input$data_page_three_year) %>%
      filter(Country == input$data_page_three_country)

    plot_ly(Emission_Renewable_nw,
      labels = c("Renewable", "Nonrenewable"),
      values = ~ c(Renewable, 100 - Renewable),
      type = "pie",
      textposition = "inside",
      textinfo = "label+percent",
      marker = list(color = c("rgb(0, 164, 0)", "rgb(0, 18, 0)"))
    ) %>%
      layout(
        title = paste0(
          input$data_page_three_country,
          " Energy Consumption Breakdown"
        ),
        xaxis = list(
          showgrid = FALSE,
          zeroline = FALSE,
          showticklabels = FALSE
        ),
        yaxis = list(
          showgrid = FALSE,
          zeroline = FALSE,
          showticklabels = FALSE
        )
      )
  })

  observe({
    choice <- Emission_Renewable %>%
      filter(Year == input$data_page_three_year) %>%
      pull(Country)

    updateSelectInput(session,
      "data_page_three_country",
      choices = choice
    )
  })
  
  
}

