# INSERT Libraries HERE
library(shiny)
library(tidyverse)
library(lubridate)
library(plotly)
library(shinythemes)

# INSERT Dataset HERE
source('./climateData.R')


##########################
##### User interface #####
##########################
ui <- fluidPage(
  theme = shinytheme("sandstone"),
  navbarPage(
    ##########
    ## Page 1
    ##########
    "Hadfield Roof Dataset",
    tabPanel(
      "Home",
      sidebarLayout(
        sidebarPanel(
          h3(
            "This is a simple shiny template with the navbar.", 
            style = "padding-bottom: 20px"
          )
        ),
        mainPanel(
          h4(" dataset posted on 03.12.2020, by Simon De-Ville, Virginia Stovin, Christian Berretta, JOERG WERDIN, Simon Poë"),
          a(
            href="https://figshare.shef.ac.uk/articles/dataset/Hadfield_Green_Roof_5-year_Dataset/11876736", 
            "Click here for data!"
          ),
          h5("Data collected as part of the EU funded 'Collaborative research and development of green roof system technology' project from the Sheffield, UK, green roof testbeds."),
          h5("Data includes 5 years of:"),
          tags$li("Rainfall data (1-minute resolution)"),
          tags$li("Green roof runoff data for 9 roof configurations (1-minute resolution)"),
          tags$li("Soil moisture content at 3 depths for 4 roof configurations (5-minute resolution)"),
          tags$li("Climate data sufficient to calculate FAO-56 Penman-Monteith (1-hour resolution)"),
          h5("Due to difficulties in monitoring testbed runoff, there are occasions where runoff data is considered invalid. A separate data-file indicates individual storm events where runoff data is considered to be valid."),
          a(href="https://github.com/yld-weng/hadfield-green-roof", "Click here for the GitHub repo!")
        )
      )
    ),
    ##########
    ## Page 2
    ##########
    tabPanel(
      "Climate Data Table",
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
          div(DT::dataTableOutput(outputId = "dataTable"))
        )
      )
    ),
    ##########
    ## Page 3
    ##########
    tabPanel(
      "Climate Data Histograms",
      sidebarLayout(
        sidebarPanel(
          dateRangeInput(
            'histDateRange',
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
          plotlyOutput(outputId = "histogram")
        )
      )
    ),
    ############
    ## Nav menu
    ############
    navbarMenu(
      "More",
      tabPanel(
        "Climate Data Time series",
        sidebarLayout(
          sidebarPanel(
            dateRangeInput(
              'timeDateRange',
              label = paste('Date range'),
              start = "2011-03-01", end = "2011-12-31",
              min = "2011-03-01", max = "2016-02-28",
              separator = " to ", format = "dd/mm/yyyy",
              startview = 'month', weekstart = 1
            )
          ),
          mainPanel(
            plotlyOutput(outputId = "timeSeries")
          )
        )
      )
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
  
  # Histogram for each variables
  output$histogram <- renderPlotly({ 
    filteredData <- shefClimateNoNA %>% 
      filter(
        between(
          TIMESTAMP, 
          as_datetime(as.character(input$histDateRange[1])), 
          as_datetime(as.character(input$histDateRange[2]))
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
    
    subplot(figs, nrows = length(input$histOptions))
  })
  
  # Time series for air temperatures
  output$timeSeries <- renderPlotly({
    shefClimateNoNA %>% 
      filter(
        between(
          TIMESTAMP, 
          as_datetime(as.character(input$timeDateRange[1])), 
          as_datetime(as.character(input$timeDateRange[2]))
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
}

##################################
##### Call shinyApp function #####
##################################
shinyApp(ui = ui, server = server)

