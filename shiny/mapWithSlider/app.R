# INSERT Libraries HERE
library(shiny)
library(tidyverse)
library(lubridate)
library(plotly)


# INSERT Dataset HERE
# Get current directory path
directoryPath <- dirname(rstudioapi::getActiveDocumentContext()$path)
source(paste(directoryPath, '/data.R', sep = ''))

dateRange <- joinDataItinerary %>% summarise(min = min(Date), max = max(Date)) %>% as_vector() %>% as_date()

##########################
##### User interface #####
##########################
ui <- fluidPage(
  
  # Title 
  titlePanel(
    h1("Map + slider", style = "padding-bottom: 20px")
  ),
  
  # Sidebar layout
  sidebarLayout(
    sidebarPanel(
      dateRangeInput(
        'dateRange',
        label = paste('Date range'),
        start = dateRange[1], end = dateRange[1] %m+% years(1),
        min = dateRange[1], max = dateRange[2],
        separator = " to ", format = "dd/mm/yyyy",
        startview = 'month', weekstart = 1
      ),
      sliderInput(
        "animation", 
        "Looping Date:",
        min = dateRange[1], max = dateRange[2],
        value = dateRange[2], timeFormat="%Y-%m-%d",
        animate = animationOptions(interval = 200, loop = FALSE)
      ),
    ),
    mainPanel(
      #verbatimTextOutput("text1"),
      plotlyOutput(outputId = "map")

    )
  )
)

###########################
##### Server function #####
###########################
server <- function(input, output, session) {
  # Change slider range when Dates are changed
  observeEvent(input$dateRange, {
    updateSliderInput(
      session, "animation", 
      min = as_date(as.character(input$dateRange[1])),
      max = as_date(as.character(input$dateRange[2]))
    )
  })
  
  # output$text1 = renderText({ as.character(input$animation)  })
  
  output$map = renderPlotly({
    filterData <- joinDataItinerary %>%
      filter(
        between(
          Date,
          as_date(as.character(input$dateRange[1])),
          as_date(as.character(input$animation))
        )
      )
  
    # Scattergeo plot properties
    g <- list(
      scope = 'europe',
      projection = list(type = 'equirectangular', scale = 5),
      showland = TRUE,
      landcolor = toRGB("gray95"),
      subunitcolor = toRGB("gray85"),
      countrycolor = toRGB("gray85"),
      countrywidth = 0.5,
      subunitwidth = 0.5,
      center = list(lon = "-1.685677", lat = "53.394009")
    )

    # Add layers
    # Border -> Peace -> War -> Current move & text -> Castles
    fig <- plot_geo(dataBorders, lat = ~latitude, lon = ~longitude, height = 700)
    fig <- fig %>%
      add_markers(
        name = "Border",
        text = ~paste("Hadrian's Wall Places", ifelse(latitude >= 54, "North", "West"), sep = ", "),
        marker = list(color = "rgb(87, 87, 87)"),
        symbol = I(4)
      ) %>% 
      add_markers(
        name = "Itinerary - Peace",
        data = filterData %>% filter(`peace or war` == 'Peace'),
        x = ~latitude,
        y = ~longitude,
        text = ~paste(
          paste(Place, County, sep = ",   "),
          paste("Date: ", as.character.Date(Date, "%d %B %Y")),
          sep = "<br>"
        ),
        marker = list(color = "#beff78", line = list(color = "#82fa00")),
        symbol = I("square"),
        size = I(12),
        hoverinfo = "text"
      ) %>%
      add_markers(
        name = "Itinerary - War",
        data = filterData %>% filter(`peace or war` == 'War'),
        x = ~latitude,
        y = ~longitude,
        text = ~paste(
          paste(Place, County, sep = ", "),
          paste("Date: ", as.character.Date(Date, "%d %B %Y")),
          sep = "<br>"
        ),
        marker = list(color = "#ff7c5c", line = list(color = "#ff3300")),
        symbol = I("square"),
        size = I(25),
        hoverinfo = "text"
      ) %>%
      add_markers(
        name = "Current move",
        data = filterData %>% filter(row_number() == n()),
        x = ~latitude,
        y = ~longitude,
        marker = list(color = "#5e85e6", line = list(color = "#00247d")),
        symbol = I("square"),
        size = I(40),
      ) %>%
      add_text(
        name = "",
        data = filterData %>% filter(row_number() == n()),
        x = ~latitude,
        y = ~longitude,
        text = ~paste(
          "",
          paste(Place, County, sep = ", "),
          paste(as.character.Date(Date, "%d %B %Y"), " (", `peace or war`, ")", sep = ""),
          sep = "<br>"
        ),
        textfont = list(
          size = 12,
          color = toRGB("grey50")
        ),
        textposition = "bottom right",
        showlegend = FALSE
      ) %>%
      add_markers(
        name = "Castles",
        data = dataCastles,
        x = ~latitude,
        y = ~longitude,
        text = ~paste(
          paste(Castle, Country, sep = ", "),
          paste("Date built or rebuilt estimate: ", `date built or rebuilt estimate`),
          sep = "<br>"
        ),
        marker = list(color = "#383838", line = list(color = "#000")),
        symbol = I("hexagram"),
        size = I(12),
        hoverinfo = "text"
      )

    fig <- fig %>% layout(
      title = 'Mapping The Itinerary of King Edward I',
      geo = g,
      margin = list(l=0,r=0,t=50,b=0,pad=0)
    )
    
    # return the chart
    fig
  })
}


##################################
##### Call shinyApp function #####
##################################
shinyApp(ui = ui, server = server)



