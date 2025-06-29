library(shiny)
library(shinydashboard)
library(leaflet)
library(httr)
library(jsonlite)
library(dplyr)
library(purrr)
library(dotenv)

# Load the .env file (make sure it's in your working directory)
dotenv::load_dot_env(file = ".env")

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
        
        /* Data table styling */
        .data-table {
          margin-top: 20px;
        }
      "))
    ),
    tabItems(
      tabItem(tabName = "map",
              h2("Map"),
              h4("Tap the map to choose a coordinate"),
              leafletOutput("map"),
              verbatimTextOutput("coordinates"),
              actionButton("okBtn", "Get Weather Data", class = "custom-btn"),
              div(class = "data-table",
                  dataTableOutput("weatherTable"))
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
  weather_data <- reactiveVal(NULL)
  
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
  
  # Function to fetch weather data
  fetch_weather_data <- function(lat, lon) {
    api_key <- Sys.getenv("API_KEY")
    url <- paste0("https://weather.visualcrossing.com/VisualCrossingWebServices/rest/services/timeline/",
                  lat, ",", lon, "?key=", api_key)
    
    tryCatch({
      response <- GET(url)
      
      if (status_code(response) == 200) {
        content_json <- content(response, as = "text", encoding = "UTF-8")
        weather_data <- fromJSON(content_json, flatten = TRUE)
        
        if (!is.null(weather_data$days)) {
          hourly_data <- weather_data$days %>%
            {if (!is.data.frame(.)) as.data.frame(.) else .} %>%
            {if ("hours" %in% names(.)) {
              tryCatch({
                tidyr::unnest(., cols = c(hours), names_sep = "_")
              }, error = function(e) {
                cat("⚠Could not unnest hours data:", e$message, "\n")
                NULL
              })
            } else {
              cat("'hours' field not found in days data\n")
              NULL
            }}
          
          if (!is.null(hourly_data)) {
            hourly_clean <- hourly_data %>%
              select(where(~ !is.list(.)))
            
            if (!"date" %in% names(hourly_clean) && "datetime" %in% names(hourly_clean)) {
              hourly_clean$date <- hourly_clean$datetime
            }
            
            return(hourly_clean)
          }
        }
      }
      return(NULL)
    }, error = function(e) {
      cat("Error", e$message, "\n")
      return(NULL)
    })
  }
  
  # Observe OK button click to fetch weather data
  observeEvent(input$okBtn, {
    if (!is.null(clicked_point())) {
      lat <- clicked_point()$lat
      lon <- clicked_point()$lng
      
      showNotification("Fetching weather data...", type = "message")
      
      # Fetch data in a reactive context
      data <- fetch_weather_data(lat, lon)
      
      if (!is.null(data)) {
        weather_data(data)
        showNotification(
          paste("Weather data loaded for:", 
                round(lat, 4), ",", 
                round(lon, 4)),
          type = "message"
        )
      } else {
        showNotification("Failed to fetch weather data", type = "error")
      }
    } else {
      showNotification("Please select a location first", type = "warning")
    }
  })
  
  # Display weather data table
  output$weatherTable <- renderDataTable({
    req(weather_data())
    weather_data()
  })
}

shinyApp(ui, server)