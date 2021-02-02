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
          TIMESTAMP = format.Date(TIMESTAMP, "%d %b %y")
        ),
      type = "line",
      hcaes(x = TIMESTAMP, y = AirTC_Avg),
      name = "Air temperature"
    ) %>%
      hc_xAxis(
        tickAmount = 10,
        tickInterval = 30,
        labels = list(format = "{value:%b %y}")
      )
  })
  

}


##################################
##### Call shinyApp function #####
##################################
shinyApp(ui = ui, server = server)



