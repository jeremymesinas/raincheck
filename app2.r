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
library(ggplot2)
library(lubridate)
library(scales)

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
        
        /* Chart containers */
        .chart-container {
          background-color: white;
          border-radius: 5px;
          padding: 15px;
          margin-bottom: 20px;
          box-shadow: 0 2px 5px rgba(0,0,0,0.1);
        }
        
        /* Scrollable content */
        .scrollable-content {
          max-height: 600px;
          overflow-y: auto;
          padding-right: 10px;
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
              h2("Daily Trends"),
              uiOutput("trendsUI")
      ),
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
  
  # Render the trends UI
  output$trendsUI <- renderUI({
    if (is.null(weather_data())) {
      return(div(class = "alert alert-info", 
                 style = "padding: 20px; background-color: #e7f3fe; border-left: 5px solid #2196F3;",
                 "Please choose a coordinate first in the Map tab and click 'Get Weather Data'"))
    }
    
    div(class = "scrollable-content",
        div(class = "chart-container",
            h3("Today's Precipitation with Flood Warnings"),
            plotOutput("todayPrecipPlot", height = "300px")),
        
        div(class = "chart-container",
            h3("Weather Condition Frequency (Last 7 Days)"),
            plotOutput("conditionFreqPlot", height = "300px")),
        
        div(class = "chart-container",
            h3("Peak Hourly Precipitation Per Day"),
            plotOutput("peakPrecipPlot", height = "300px")),
        
        div(class = "chart-container",
            h3("Overall Weather Condition Frequency"),
            plotOutput("conditionPiePlot", height = "300px")),
        
        div(class = "chart-container",
            h3("Hourly Precipitation Points"),
            plotOutput("hourlyPrecipPlot", height = "300px")),
        
        div(class = "chart-container",
            h3("Hourly Wind Speeds"),
            plotOutput("windSpeedPlot", height = "300px"))
    )
  })
  
  # Reactive data processing for charts
  chart_data <- reactive({
    req(weather_data())
    
    data <- weather_data() %>%
      mutate(
        date_parsed = as.Date(datetime),
        full_datetime = as.POSIXct(paste(datetime, hours_datetime), format = "%Y-%m-%d %H:%M", tz = "Asia/Manila")
      )
    
    list(
      today_data = data %>% filter(date_parsed == Sys.Date()),
      latest_7_days = {
        latest_dates <- data %>%
          distinct(date_parsed) %>%
          arrange(desc(date_parsed)) %>%
          slice(1:7) %>%
          pull(date_parsed)
        
        data %>%
          filter(date_parsed %in% latest_dates) %>%
          count(date_parsed, hours_conditions) %>%
          mutate(date_parsed = factor(date_parsed, levels = sort(latest_dates)))
      },
      peak_precip = data %>%
        group_by(date_parsed) %>%
        summarise(max_hourly_precip = max(hours_precip, na.rm = TRUE), .groups = 'drop'),
      condition_freq = data %>%
        count(hours_conditions) %>%
        mutate(percentage = n / sum(n) * 100),
      today_unique = data %>%
        filter(date_parsed == Sys.Date()) %>%
        distinct(hours_datetime, .keep_all = TRUE)
    )
  })
  
  # Chart 1: Today's Line Graph with Flood Warnings
  output$todayPrecipPlot <- renderPlot({
    data <- chart_data()$today_data
    if (nrow(data) == 0) return(NULL)
    
    ggplot(data, aes(x = full_datetime, y = hours_precip)) +
      geom_line(color = "#375a7f", linewidth = 1) +
      geom_point(aes(color = hours_precip >= 50), size = 3) +
      scale_color_manual(
        values = c("FALSE" = "#7FDBFF", "TRUE" = "#FF4136"),
        labels = c("Normal", "Flood Warning"),
        name = "Precip Level"
      ) +
      scale_x_datetime(date_labels = "%H:%M", breaks = pretty_breaks(n = 10)) +
      labs(x = "Time", y = "Precipitation (mm)") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Chart 2: Stacked Bar - Condition Frequency for Last 7 Days
  output$conditionFreqPlot <- renderPlot({
    data <- chart_data()$latest_7_days
    if (nrow(data) == 0) return(NULL)
    
    ggplot(data, aes(x = date_parsed, y = n, fill = hours_conditions)) +
      geom_bar(stat = "identity", position = "stack") +
      labs(x = "Date", y = "Count", fill = "Condition") +
      scale_fill_brewer(palette = "Pastel2") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Chart 3: Bar Chart - Peak Precip Per Day
  output$peakPrecipPlot <- renderPlot({
    data <- chart_data()$peak_precip
    if (nrow(data) == 0) return(NULL)
    
    ggplot(data, aes(x = date_parsed, y = max_hourly_precip)) +
      geom_col(fill = "#2E86AB") +
      geom_text(aes(label = max_hourly_precip), vjust = -0.5, size = 3) +
      labs(x = "Date", y = "Max Precipitation (mm)") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Chart 4: Pie Chart - Overall Weather Condition Frequency
  output$conditionPiePlot <- renderPlot({
    data <- chart_data()$condition_freq
    if (nrow(data) == 0) return(NULL)
    
    ggplot(data, aes(x = "", y = n, fill = hours_conditions)) +
      geom_bar(width = 1, stat = "identity") +
      coord_polar(theta = "y") +
      geom_text(aes(label = paste0(round(percentage), "%")),
                position = position_stack(vjust = 0.5), color = "white") +
      labs(fill = "Condition") +
      theme_void() +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  # Chart 5: SCATTERPLOT - Hourly Precip for Today
  output$hourlyPrecipPlot <- renderPlot({
    data <- chart_data()$today_unique
    if (nrow(data) == 0) return(NULL)
    
    ggplot(data, aes(x = hours_datetime, y = hours_precip)) +
      geom_point(color = "#0074D9", size = 3) +
      labs(x = "Hour", y = "Precipitation (mm)") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Chart 6: LINE GRAPH - Hourly Wind Speed Today
  output$windSpeedPlot <- renderPlot({
    data <- chart_data()$today_data
    if (nrow(data) == 0) return(NULL)
    
    ggplot(data, aes(x = full_datetime, y = hours_windspeed)) +
      geom_line(color = "#20B2AA", linewidth = 1) +
      geom_point(color = "#00CED1", size = 2) +
      scale_x_datetime(date_labels = "%H:%M", breaks = pretty_breaks(n = 10)) +
      labs(x = "Time", y = "Wind Speed (km/h)") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
}

shinyApp(ui, server)
