library(shiny)
library(shinydashboard)
library(leaflet)

ui <- dashboardPage(
  dashboardHeader(
    title = tags$span("RainCheck", style = "color: white; font-weight: bold;"),
    titleWidth = 250
  ),
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      menuItem("Map", tabName = "map", icon = icon("map")),
      menuItem("Daily Trends", tabName = "trends", icon = icon("bar-chart")),
      menuItem("Forecast", tabName = "forecast", icon = icon("search")),
      menuItem("Models", tabName = "models", icon = icon("database")),
      menuItem("About", tabName = "about", icon = icon("user"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        /* Header */
        .main-header {
          background-color: #000080 !important;
        }
        .main-header .logo {
          background-color: #000080 !important;
          color: white !important;
        }
        .main-header .navbar {
          background-color: #000080 !important;
        }

        /* Sidebar */
        .main-sidebar {
          background-color: #000080 !important;
        }
        .main-sidebar .sidebar {
          color: white !important;
        }
        .main-sidebar .sidebar a {
          color: white !important;
        }

        /* Active menu item */
        .main-sidebar .sidebar .active a {
          background-color: #000060 !important;
          color: white !important;
        }
        
        /* Custom button style */
        .custom-btn {
          background-color: #000080 !important;
          color: white !important;
          border: none;
          padding: 8px 16px;
          margin-top: 10px;
        }
      "))
    ),
    tabItems(
      tabItem(tabName = "map",
              h2("Map"),
              h4("Tap the map to choose a coordinate"),
              leafletOutput("map"),
              verbatimTextOutput("coordinates"),
              actionButton("okBtn", "OK", class = "custom-btn")
      ),
      tabItem(tabName = "trends",
              h2("Daily Trends")),
      tabItem(tabName = "forecast",
              h2("Forecast")),
      tabItem(tabName = "models",
              h2("Models")),
      tabItem(tabName = "about",
              h2("About"))
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive value to store clicked point
  clicked_point <- reactiveVal(NULL)
  
  # Render the leaflet map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%  # Adds default OpenStreetMap tiles
      setView(lng = 121.7740, lat = 12.8797, zoom = 6)  # Philippines view
  })
  
  # Observe map clicks and add markers
  observeEvent(input$map_click, {
    click <- input$map_click
    clicked_point(click)
    
    leafletProxy("map") %>%
      clearMarkers() %>%
      addMarkers(lng = click$lng, lat = click$lat)
  })
  
  # Display coordinates
  output$coordinates <- renderText({
    if (!is.null(clicked_point())) {
      paste0("Latitude: ", round(clicked_point()$lat, 4), 
             ", Longitude: ", round(clicked_point()$lng, 4))
    } else {
      "No location selected yet"
    }
  })
  
  # Observe OK button click
  observeEvent(input$okBtn, {
    if (!is.null(clicked_point())) {
      showNotification(
        paste("Location selected:", 
              round(clicked_point()$lat, 4), ",", 
              round(clicked_point()$lng, 4)),
        type = "message"
      )
    } else {
      showNotification("Please select a location first", type = "warning")
    }
  })
}

shinyApp(ui, server)