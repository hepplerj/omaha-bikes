library(tidyverse)
library(leaflet)
library(shiny)
library(RColorBrewer)

data <- read_csv("accidents.csv")
omaha_data <- data %>% filter(CityName == "Omaha")

vars <- c(
  "Total Injuries" = "TotalInjuries",
  "Total Fatalities" = "TotalFatalities"
)



ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body { width: 100%; height: 100%"),
  tags$head(includeCSS("styles.css")),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(h3("Pedestrian and Bicycle Accidents in Omaha"),
                id = "controls", class = "panel panel-default", top = 10, right = 10,
                fixed = TRUE, draggable = FALSE, width = 330, height = "auto",
                #sliderInput("range", "Year", min(omaha_data$AccidentYear), max(omaha_data$AccidentYear),
                #            value = 2008, sep = "", step = 1, width = 300
                #            ),
                # Year filter
                selectInput("range", "Year:",
                            c("2008", "2009", "2010", "2011", "2012", "2013", "2014")),
                
                # Type filter
                selectInput("bytype", "Choose type: ",
                            choices = c("All" = NULL,
                                        "Bike" = "Cyclists",
                                        "Pedestrian" = "Pedestrian")),
                
                # Histogram 
                plotOutput("histCentile", height = 200),
                plotOutput("lineTrend", height = 140),
               
                # Conditions filter 
                selectInput("conditions", "Conditions:",
                            choices = c("Rain", "Clear"))
                
  )
)

server <- function(input, output, session) {
  
  
  palette_rev <- rev(brewer.pal(5, "YlOrRd"))
  
  colorpal <- reactive({
    colorNumeric(palette_rev, omaha_data$AccidentSeverity_CodeFromNDOR)
  })
  
  filteredData <- reactive({
    #if(is.null(input$bytype)) {
    #  return()
    #}
    
    omaha_data %>% filter(AccidentYear == input$range)# %>%
    #print(input$bytype)
                   #filter(PedestrianOrPedalcyclist %in% input$bytype)
  })
  
  output$map <- renderLeaflet({
    leaflet(omaha_data) %>% addProviderTiles(providers$CartoDB.Positron) %>% 
      fitBounds(~min(Longitude), ~min(Latitude), ~max(Longitude), ~max(Latitude))
  })
  
  filteredSeverity <- reactive({
    omaha_data %>% filter(AccidentYear == input$range)
  })
  
  observe({
    pal <- colorpal()
    
    leafletProxy("map", data = filteredData()) %>% 
      clearMarkers() %>% 
      clearControls() %>% 
      addCircleMarkers(radius = 6,
                       stroke = FALSE,
                       fillColor = ~pal(AccidentSeverity_CodeFromNDOR),
                       fillOpacity = 0.7,
                       popup = ~paste("Severity: ", AccidentSeverity_CodeFromNDOR, 
                                      "<br/>",
                                      "Injuries: ", TotalInjuries,
                                      "<br/>",
                                      "Fatalities: ", TotalFatalities,
                                      "<br/>",
                                      "Type: ", PedestrianOrPedalcyclist,
                                      "<br/>",
                                      "Conditions: ", WeatherConditions,
                                      "<br/>",
                                      "Alcohol involved: ", AlcoholInvolved)
      ) %>% 
      addLegend("bottomright", pal = pal, values = ~AccidentSeverity_CodeFromNDOR,
                title = "Accident Severity",
                opacity = 1)
  })
  
  output$histCentile <- renderPlot({
    ggplot(filteredSeverity(), aes(x = AccidentSeverity_CodeFromNDOR)) +
      geom_bar(stat = "count", aes(fill = PedestrianOrPedalcyclist)) +
      theme_minimal() +
      labs(title = paste("Accident Severity in", input$range)) +
      xlab("Accident Severity (1 = most severe)") +
      ylab("No. of Accidents")
  })
  
  output$lineTrend <- renderPlot({
    ggplot(omaha_data, aes(x = AccidentYear, color = PedestrianOrPedalcyclist)) +
      geom_line(stat = "count") +
      theme(legend.title = element_blank()) +
      labs(title = "Trend for All Years")
  })
  
}

shinyApp(ui, server)
