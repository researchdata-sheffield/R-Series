# INSERT Libraries HERE
library(shiny)
library(tidyverse)
library(lubridate)
library(highcharter)


# INSERT Dataset HERE
source('./climateData.R')


##########################
##### User interface #####
##########################
ui <- fluidPage(
  
  # Title 
  titlePanel(
    h1("Hadfield Green Roof 5-year Dataset", style = "padding-bottom: 20px")
  ),
  
  # Sidebar layout
  sidebarLayout(
    sidebarPanel(
      dateRangeInput(
        'dateRange',
        label = paste('Date range'),
        start = "2011-03-01", end = "2011-12-31",
        min = "2011-03-01", max = "2016-02-28",
        separator = " to ", format = "dd/mm/yyyy",
        startview = 'month', weekstart = 1
      )
    ),
    mainPanel(
      DT::dataTableOutput(outputId = "dataTable"),
      highchartOutput(outputId = "timeSeries"),
    )
  )
)

###########################
##### Server function #####
###########################
server <- function(input, output, session) {
  
  # Filter data based on selections
  output$dataTable <- DT::renderDataTable(DT::datatable({
    shefClimateNoNA %>%
      filter(
        between(
          TIMESTAMP, 
          as_datetime(as.character(input$dateRange[1])), 
          as_datetime(as.character(input$dateRange[2]))
        )
      )
  }))
  
  # Time series for air temperatures
  output$timeSeries <- renderHighchart({
    # hc <- highchart() %>% hc_xAxis(type = 'datetime') %>%
    #   hc_add_series(
    #     data = shefClimateDf, 
    #     type = "line",
    #     hcaes(x = TIMESTAMP, y = AirTC_Avg)
    #   )
    hchart(
      shefClimateNoNA %>% 
        filter(
          between(
            TIMESTAMP, 
            as_datetime(as.character(input$dateRange[1])), 
            as_datetime(as.character(input$dateRange[2]))
          )
        ) %>% 
        mutate(
          TIMESTAMP = format.Date(TIMESTAMP, "%m-%d")
        ),
      type = "line",
      hcaes(x = TIMESTAMP, y = AirTC_Avg),
      name = "Air temperature"
    )
  })

}


##################################
##### Call shinyApp function #####
##################################
shinyApp(ui = ui, server = server)



