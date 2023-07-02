# Install and import required libraries
require(shiny)
require(ggplot2)
require(leaflet)
require(tidyverse)
require(httr)
require(scales)
# Import model_prediction R which contains methods to call OpenWeather API
# and make predictions
source("model_prediction.R")


test_weather_data_generation<-function(){
  #Test generate_city_weather_bike_data() function
  city_weather_bike_df<-generate_city_weather_bike_data()
  stopifnot(length(city_weather_bike_df)>0)
  print(head(city_weather_bike_df))
  return(city_weather_bike_df)
}

# Create a RShiny server
shinyServer(function(input, output){
  # Define a city list
  city_l <- c("Seoul", "New York", "Paris", "Suzhou", "London")
  # Define color factor
  color_levels <- colorFactor(c("green", "yellow", "red"), 
                              levels = c("small", "medium", "large"))
  size_levels <- c('small' = 6,'medium'= 10,'large'= 12)
  city_weather_bike_df <- test_weather_data_generation()
  # Create another data frame called `cities_max_bike` with each row contains city location info and max bike
  cities_max_bike <- generate_city_weather_bike_data()
  r <- cities_max_bike %>% group_by(CITY_ASCII) %>% summarise(BIKE_PREDICTION = max(BIKE_PREDICTION)) %>% inner_join(cities_max_bike, by='BIKE_PREDICTION')
  # Then render output plots with an id defined in ui.R
  
  observeEvent(input$city_select, {
    selected_city <- input$city_select
    
    # Then render output plots with an id defined in ui.R
    if (selected_city == "ALL") {
      # If "All" was selected from dropdown, render a leaflet map with circle markers
      # and popup weather LABEL for all five cities
      output$city_bike_map <- renderLeaflet({
        m <- leaflet() %>% addProviderTiles("OpenStreetMap.Mapnik")
        for (i in 1:nrow(r)) {
          n <- r[i,]
          m <- m %>% addCircleMarkers(
            data = n,
            lat = ~LAT,
            lng = ~LNG,
            radius = ~as.numeric(size_levels[n$BIKE_PREDICTION_LEVEL]),
            color = ~color_levels(n$BIKE_PREDICTION_LEVEL),
            fillOpacity = 0.6,
            popup = ~paste0("City: ", n$CITY_ASCII.x, "<br>Weather: ", n$LABEL)
          )
        }
        m
      })
      
    } else {
      # If just one specific city was selected, render a leaflet map with one marker
      # on the map and a popup with DETAILED_LABEL displayed
      c <- r %>% filter(CITY_ASCII.x == selected_city)
      output$city_bike_map <- renderLeaflet({
        leaflet() %>%
          addProviderTiles("OpenStreetMap.Mapnik") %>%
          addCircleMarkers(
            data = c,
            lat = ~LAT,
            lng = ~LNG,
            radius = ~as.numeric(size_levels[BIKE_PREDICTION_LEVEL]),
            color = ~color_levels(BIKE_PREDICTION_LEVEL),
            popup = ~paste0("City: ", CITY_ASCII.x, "<br>Weather: ", DETAILED_LABEL)
          )
      }) 
      output$bike_line <- renderPlot({
        city_weather_bike_df %>% 
          filter(CITY_ASCII == selected_city) %>%
          ggplot(aes(x=as.POSIXct(FORECASTDATETIME), y=BIKE_PREDICTION)) +
          geom_line() + geom_point() + 
          scale_x_datetime(date_labels = "%m-%d %H:%M", date_breaks = "12 hour") + 
          geom_text(aes(label=BIKE_PREDICTION), vjust=-0.5, size=2.5)+
          theme(axis.text.x = element_text(angle = 90, hjust = 1))
      })
      output$temp_line <- renderPlot({
        city_weather_bike_df %>% 
          filter(CITY_ASCII == selected_city) %>%
          ggplot(aes(x=as.POSIXct(FORECASTDATETIME), y=TEMPERATURE)) +
          geom_line() + geom_point() + 
          scale_x_datetime(date_labels = "%m-%d %H:%M", date_breaks = "12 hour") + 
          geom_text(aes(label=TEMPERATURE), vjust=-0.5, size=2.5)+
          theme(axis.text.x = element_text(angle = 90, hjust = 1))
      })
      output$humid_bike <- renderPlot({
        city_weather_bike_df %>% 
          filter(CITY_ASCII == selected_city) %>% ggplot(aes(x=HUMIDITY, y=BIKE_PREDICTION)) + geom_point() + geom_smooth(method = 'lm', formula = y ~ poly(x, 4))
        
      })
    }
    
})
  
})
