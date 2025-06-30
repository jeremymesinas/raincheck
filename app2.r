library(shiny)
library(shinydashboard)
library(leaflet)
library(httr)
library(jsonlite)
library(dplyr)
library(purrr)
library(dotenv)
library(tidyr)
library(DT)

# Load the .env file
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
          overflow-x: auto;
        }
        
        /* Make table scrollable */
        .dataTables_wrapper {
          overflow-x: auto;
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
                  DTOutput("weatherTable"))
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
      addTiles() %>%
      setView(lng = 121.7740, lat = 12.8797, zoom = 6)
  })
  
  # Observe map clicks
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
  
  # Function to fetch and preprocess weather data
  fetch_and_preprocess_weather_data <- function(lat, lon) {
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
            # First cleaning step
            hourly_clean <- hourly_data %>%
              select(where(~ !is.list(.)))
            
            if (!"date" %in% names(hourly_clean) && "datetime" %in% names(hourly_clean)) {
              hourly_clean$date <- hourly_clean$datetime
            }
            
            # Preprocessing steps
            cols_to_remove <- c(
              "datetimeEpoch", "tempmax", "tempmin", "feelslikemax", "feelslikemin", "feelslike",
              "solarenergy", "sunrise", "sunriseEpoch", "sunset", "sunsetEpoch",
              "moonphase", "source", "hours_datetimeEpoch", "hours_feelslike",
              "hours_dew", "hours_solarenergy", "hours_uvindex", "hours_icon",
              "hours_source", "date", "snow", "snowdepth", "hours_snow", "hours_snowdepth"
            )
            
            hourly_clean <- hourly_clean[ , !(names(hourly_clean) %in% cols_to_remove)]
            
            # Flag outliers
            numeric_cols <- sapply(hourly_clean, is.numeric)
            
            for (col_name in names(hourly_clean)[numeric_cols]) {
              IQR_value <- IQR(hourly_clean[[col_name]], na.rm = TRUE)
              lower_bound <- quantile(hourly_clean[[col_name]], 0.25, na.rm = TRUE) - 1.5 * IQR_value
              upper_bound <- quantile(hourly_clean[[col_name]], 0.75, na.rm = TRUE) + 1.5 * IQR_value
              
              new_col_name <- paste0(col_name, "_outlier")
              hourly_clean[[new_col_name]] <- ifelse(
                hourly_clean[[col_name]] < lower_bound | hourly_clean[[col_name]] > upper_bound,
                TRUE, FALSE
              )
              
              deviation_col <- paste0(col_name, "_deviation")
              hourly_clean[[deviation_col]] <- ifelse(
                hourly_clean[[new_col_name]],
                ifelse(
                  hourly_clean[[col_name]] < lower_bound,
                  (lower_bound - hourly_clean[[col_name]]) / IQR_value,
                  (hourly_clean[[col_name]] - upper_bound) / IQR_value
                ),
                0
              )
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
  
  # Observe OK button click
  observeEvent(input$okBtn, {
    if (!is.null(clicked_point())) {
      lat <- clicked_point()$lat
      lon <- clicked_point()$lng
      
      showNotification("Fetching and processing weather data...", type = "message")
      
      data <- fetch_and_preprocess_weather_data(lat, lon)
      
      if (!is.null(data)) {
        weather_data(data)
        # Automatically save the data when it's processed
        write.csv(data, "hourly_weather_data_final.csv", row.names = FALSE)
        showNotification(
          paste("Weather data saved to hourly_weather_data_final.csv for:", 
                round(lat, 4), ",", 
                round(lon, 4)),
          type = "message"
        )
      } else {
        showNotification("Failed to fetch or process weather data", type = "error")
      }
    } else {
      showNotification("Please select a location first", type = "warning")
    }
  })
  
  # Display processed weather data table
  output$weatherTable <- renderDT({
    req(weather_data())
    datatable(
      weather_data(),
      options = list(
        scrollX = TRUE,
        autoWidth = TRUE,
        pageLength = 10
      ),
      rownames = FALSE
    )
  })
}

shinyApp(ui, server)
