# Load required libraries
require(leaflet)


# Create a RShiny UI
shinyUI(
  fluidPage(padding=5,
  titlePanel("Bike-sharing demand prediction app"), 
  # Create a side-bar layout
  sidebarLayout(
    # Create a main panel to show cities on a leaflet map
    mainPanel(
      # leaflet output with id = 'city_bike_map', height = 1000
      leafletOutput('city_bike_map', height = "1000px")
    ),
    # Create a side bar to show detailed plots for a city
    sidebarPanel(
      # select drop down list to select city
      selectInput("city_select", 'Select City:', choices = c("ALL","Seoul", "New York", "Paris", "Suzhou", "London")),
      plotOutput("bike_line", click = "plot_click", height = "400px", width = "100%"),
      #verbatimeTextOutput('bike_date_output')
      plotOutput("temp_line", click = "plot_click", height = "400px", width = "100%"),
      plotOutput("humid_bike", click = "plot_click", height = "400px", width = "100%")
    ))
))