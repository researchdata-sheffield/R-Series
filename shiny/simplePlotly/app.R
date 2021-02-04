# INSERT Libraries HERE
library(shiny)
library(tidyverse)
library(lubridate)
library(plotly)

# INSERT Dataset HERE
source('./climateData.R') 
# OR source('climateData.R') if placed in the same directory

histogramName <- c("Wind speed", "Air temperature", "Relative Humidity", "Solar radiation", "Barometric Pressure")

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
      ),
      selectInput(
        'histOptions', 
        'Histogram Options', 
        histogramName, 
        selected = "Air temperature",
        multiple = TRUE, 
        selectize = TRUE
      )
    ),
    mainPanel(
      plotlyOutput(outputId = "timeSeries"),
      div(plotlyOutput(outputId = "histogram"), style = "margin-top: 50px"),
      div(DT::dataTableOutput(outputId = "dataTable"), style = "margin-top: 50px")
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
  output$timeSeries <- renderPlotly({
    shefClimateNoNA %>% 
      filter(
        between(
          TIMESTAMP, 
          as_datetime(as.character(input$dateRange[1])), 
          as_datetime(as.character(input$dateRange[2]))
        )
      ) %>% 
      plot_ly(
        x = ~TIMESTAMP, 
        y = ~AirTC_Avg, 
        type = 'scatter', 
        mode = 'lines',
        line = list(color = '#251d5a', width = 1.5)
      ) %>%
      layout(
        title = "Average air temperature over the selected time frame",
        xaxis = list(title = "Date"),
        yaxis = list(title = "Air temperature")
      )
  })
  
  # Histogram for each variables
  output$histogram <- renderPlotly({ 
    filteredData <- shefClimateNoNA %>% 
      filter(
        between(
          TIMESTAMP, 
          as_datetime(as.character(input$dateRange[1])), 
          as_datetime(as.character(input$dateRange[2]))
        )
      )
    
    histogramUnit <- c("m/s", "C", "%", "KW/m2", "mbar")
    histogramVar <- c("WS_ms_Avg", "AirTC_Avg", "RH", "Slr_kW", "BP_mbar")
    figs <- list()
    
    for(i in 1:length(input$histOptions)) {
      index <- match(input$histOptions[i], histogramName)
      
      fig <- plot_ly(
        filteredData, 
        x = as.formula(paste0("~", histogramVar[index]))
      ) %>% 
        add_trace( 
          name = histogramName[index],
          type = "histogram",
          hovertemplate = paste(
            '<b>Range</b>: %{x} ',
            histogramUnit[index],
            '<br><b>Frequency</b>: %{y:.3f}',
            '<extra>', histogramName[index], '</extra>'
          ))
      figs[length(figs)+1] <- list(fig)
    }
    
    subplot(figs, nrows = ceiling(length(input$histOptions)/2))
  })
}


##################################
##### Call shinyApp function #####
##################################
shinyApp(ui = ui, server = server)

