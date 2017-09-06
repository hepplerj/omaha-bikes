library(tidyverse)
library(leaflet)
library(shiny)
library(RColorBrewer)

data <- read_csv("accidents.csv")
omaha_data <- data %>% filter(CityName == "Omaha")
names(omaha_data) <- c("accidentkey","year","date","bytype","latitude","longitude","accidentseverity","totalfatalities","totalinjuries","time","alcohol","weather","landusecode","county","city","population2000","population2010","STATEFP10","PLACEFP10","PLACENS10")

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
                selectInput("bytype", "Choose type: ", choices = NULL),
                
                # Histogram 
                plotOutput("histCentile", height = 200),
                plotOutput("lineTrend", height = 140),
               
                # Conditions filter 
                selectInput("conditions", "Conditions:", choices = NULL),
                
                tags$p(tags$small(includeHTML("attr.html")))
                
  )
)

server <- function(input, output, session) {
  
  conditions_list <- omaha_data$weather
  names(conditions_list) <- conditions_list
  updateSelectInput(session, "conditions", choices = conditions_list)
  
  type_list <- omaha_data$bytype
  names(type_list) <- type_list
  updateSelectInput(session, "bytype", choices = type_list)
  
  palette_rev <- rev(brewer.pal(5, "YlOrRd"))
  
  colorpal <- reactive({
    colorNumeric(palette_rev, omaha_data$accidentseverity)
  })
  
  filteredData <- reactive({
    omaha_data %>% filter(year == input$range,
                          bytype == input$bytype,
                          weather == input$conditions)# %>%
    #print(input$bytype)
                   #filter(bytype %in% input$bytype)
    
  })
  
  output$map <- renderLeaflet({
    leaflet(omaha_data) %>% addProviderTiles(providers$CartoDB.Positron) %>% 
      fitBounds(~min(longitude), ~min(latitude), ~max(longitude), ~max(latitude))
  })
  
  filteredSeverity <- reactive({
    omaha_data %>% filter(year == input$range)
  })
  
  observe({
    pal <- colorpal()
    
    leafletProxy("map", data = filteredData()) %>% 
      clearMarkers() %>% 
      clearControls() %>% 
      addCircleMarkers(radius = 6,
                       stroke = FALSE,
                       fillColor = ~pal(accidentseverity),
                       fillOpacity = 0.7,
                       popup = ~paste("Severity: ", accidentseverity, 
                                      "<br/>",
                                      "Injuries: ", totalinjuries,
                                      "<br/>",
                                      "Fatalities: ", totalfatalities,
                                      "<br/>",
                                      "Type: ", bytype,
                                      "<br/>",
                                      "Conditions: ", weather,
                                      "<br/>",
                                      "Alcohol involved: ", alcohol)
      ) %>% 
      addLegend("bottomright", pal = pal, values = ~accidentseverity,
                title = "Accident Severity",
                opacity = 1)
  })
  
  output$histCentile <- renderPlot({
    ggplot(filteredSeverity(), aes(x = accidentseverity)) +
      geom_bar(stat = "count", aes(fill = bytype)) +
      theme_minimal() +
      labs(title = paste("Accident Severity in", input$range)) +
      xlab("Accident Severity (1 = most severe)") +
      ylab("No. of Accidents")
  })
  
  output$lineTrend <- renderPlot({
    ggplot(omaha_data, aes(x = year, color = bytype)) +
      geom_line(stat = "count") +
      theme(legend.title = element_blank()) +
      labs(title = "Trend for All Years") +
      geom_vline(xintercept=as.numeric(input$range), linetype = 1)
  })
  
}

shinyApp(ui, server)
