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
library(plotly)
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
      #menuItem("Models", tabName = "models", icon = icon("database")),
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
        
        /* Two-column layout */
        .chart-row {
          display: flex;
          flex-wrap: wrap;
          margin: -10px;
        }
        .chart-col {
          flex: 1;
          min-width: 400px;
          padding: 10px;
        }
        
        /* Plotly modebar customization */
        .modebar-container {
          padding: 5px !important;
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
              h2("Weather Forecasting Models"),
              fluidRow(
                box(width = 12, title = "Model Selection",
                    selectInput("modelType", "Choose Model Type:",
                                choices = c("Precipitation Regression", 
                                            "Weather Condition Classification",
                                            "Weather Pattern Clustering"),
                                selected = "Precipitation Regression")
                )
              ),
              uiOutput("forecastModelUI")
      ),
     # tabItem(tabName = "models",
     #         h2("Models")),
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
  # Render the trends UI
  output$trendsUI <- renderUI({
    if (is.null(weather_data())) {
      return(div(class = "alert alert-info", 
                 style = "padding: 20px; background-color: #e7f3fe; border-left: 5px solid #2196F3;",
                 "Please choose a coordinate first in the Map tab and click 'Get Weather Data'"))
    }
    
    div(class = "scrollable-content",
        div(class = "chart-row",
            div(class = "chart-col",
                div(class = "chart-container",
                    h3("Today's Precipitation with Flood Warnings"),
                    plotlyOutput("todayPrecipPlot", height = "300px"))
            ),
            div(class = "chart-col",
                div(class = "chart-container",
                    h3("Hourly Wind Speeds"),
                    plotlyOutput("windSpeedPlot", height = "300px"))
            )
        ),
        div(class = "chart-row",
            div(class = "chart-col",
                div(class = "chart-container",
                    h3("Peak Hourly Precipitation Per Day"),
                    plotlyOutput("peakPrecipPlot", height = "300px"))
            ),
            div(class = "chart-col",
                div(class = "chart-container",
                    h3("Hourly Precipitation Points"),
                    plotlyOutput("hourlyPrecipPlot", height = "300px"))
            )
        ),
        div(class = "chart-container",
            h3("Weather Condition Frequency (Last 7 Days)"),
            plotlyOutput("conditionFreqPlot", height = "300px")),
        div(class = "chart-container",
            h3("Overall Weather Condition Frequency"),
            plotOutput("conditionPiePlot", height = "300px"))
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
  output$todayPrecipPlot <- renderPlotly({
    data <- chart_data()$today_data
    if (nrow(data) == 0) return(NULL)
    
    p <- ggplot(data, aes(x = full_datetime, y = hours_precip, 
                          text = paste("Time:", format(full_datetime, "%H:%M"),
                                       "<br>Precipitation:", hours_precip, "mm",
                                       "<br>Status:", ifelse(hours_precip >= 50, "Flood Warning", "Normal")))) +
      geom_line(color = "#00CED1", linewidth = 1) +
      geom_point(aes(color = hours_precip >= 50), size = 3) +
      geom_line(color = "#00CED1", alpha = 0.5) +  # Added extra line for better visibility
      scale_color_manual(
        values = c("FALSE" = "#7FDBFF", "TRUE" = "#FF4136"),
        labels = c("Normal", "Flood Warning"),
        name = "Precip Level"
      ) +
      scale_x_datetime(date_labels = "%H:%M", breaks = pretty_breaks(n = 10)) +
      labs(x = "Time", y = "Precipitation (mm)") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        hovermode = "x unified",
        margin = list(l = 50, r = 50, b = 50, t = 50, pad = 4),
        xaxis = list(
          rangeslider = list(visible = TRUE),
          type = "date"
        )
      ) %>%
      config(displayModeBar = TRUE)
  })
  
  # Chart 2: Stacked Bar - Condition Frequency for Last 7 Days (static)
  output$conditionFreqPlot <- renderPlotly({
    data <- chart_data()$latest_7_days
    if (nrow(data) == 0) return(NULL)
    
    p <- ggplot(data, aes(x = date_parsed, y = n, fill = hours_conditions,
                          text = paste("Date:", date_parsed,
                                       "<br>Condition:", hours_conditions,
                                       "<br>Count:", n,
                                       "<br>Percentage:", round(n/sum(n)*100, 1), "%"))) +
      geom_bar(stat = "identity", position = "stack") +
      labs(x = "Date", y = "Count", fill = "Condition") +
      scale_fill_brewer(palette = "Pastel2") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        hovermode = "closest",
        margin = list(l = 50, r = 50, b = 100, t = 50, pad = 4)
      ) %>%
      config(displayModeBar = TRUE)
  })
  
  # Chart 3: Bar Chart - Peak Precip Per Day
  output$peakPrecipPlot <- renderPlotly({
    data <- chart_data()$peak_precip
    if (nrow(data) == 0) return(NULL)
    
    p <- ggplot(data, aes(x = date_parsed, y = max_hourly_precip,
                          text = paste("Date:", date_parsed,
                                       "<br>Max Precipitation:", max_hourly_precip, "mm"))) +
      geom_col(fill = "#00CED1", alpha = 0.7) +  # Made color lighter
      geom_text(aes(label = max_hourly_precip), vjust = -0.5, size = 3, color = "#333333") +  # Darker text for better contrast
      labs(x = "Date", y = "Max Precipitation (mm)") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        hovermode = "x unified",
        xaxis = list(
          rangeslider = list(visible = TRUE),
          type = "date"
        )
      ) %>%
      config(displayModeBar = TRUE)
  })
  
  # Chart 4: Pie Chart - Overall Weather Condition Frequency (static)
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
      theme(plot.title = element_text(hjust = 0.5, size = 14),
      legend.text = element_text(size=12))
  })
  
  # Chart 5: SCATTERPLOT - Hourly Precip for Today
  output$hourlyPrecipPlot <- renderPlotly({
    data <- chart_data()$today_unique
    if (nrow(data) == 0) return(NULL)
    
    p <- ggplot(data, aes(x = hours_datetime, y = hours_precip,
                          text = paste("Time:", hours_datetime,
                                       "<br>Precipitation:", hours_precip, "mm"))) +
      geom_line(color = "#0074D9", linewidth = 1) +
      geom_point(color = "#00CED1", size = 3) +
      geom_line(color = "#0074D9", alpha = 0.5) +  # Added extra line for better visibility
      labs(x = "Hour", y = "Precipitation (mm)") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        hovermode = "x unified",
        margin = list(l = 50, r = 50, b = 50, t = 50, pad = 4),
        xaxis = list(
          rangeslider = list(visible = TRUE),
          type = "date"
        )
      ) %>%
      config(displayModeBar = TRUE)
  })
  
  # Chart 6: LINE GRAPH - Hourly Wind Speed Today
  output$windSpeedPlot <- renderPlotly({
    data <- chart_data()$today_data
    if (nrow(data) == 0) return(NULL)
    
    p <- ggplot(data, aes(x = full_datetime, y = hours_windspeed,
                          text = paste("Time:", format(full_datetime, "%H:%M"),
                                       "<br>Wind Speed:", hours_windspeed, "km/h"))) +
      geom_line(color = "#20B2AA", linewidth = 1) +
      geom_point(color = "#00CED1", size = 2) +
      geom_line(color = "#20B2AA", alpha = 0.5) +  # Added extra line for better visibility
      scale_x_datetime(date_labels = "%H:%M", breaks = pretty_breaks(n = 10)) +
      labs(x = "Time", y = "Wind Speed (km/h)") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        hovermode = "x unified",
        margin = list(l = 50, r = 50, b = 50, t = 50, pad = 4),
        xaxis = list(
          rangeslider = list(visible = TRUE),
          type = "date"
        )
      ) %>%
      config(displayModeBar = TRUE)
  })
  
  # Forecast Tab Server Logic
  output$forecastModelUI <- renderUI({
    req(weather_data())  # Ensure weather data is loaded
    
    if(input$modelType == "Precipitation Regression") {
      tagList(
        box(width = 12, title = "Precipitation Regression Model",
            plotlyOutput("regressionPlot"),
            verbatimTextOutput("regressionSummary")
        ),
        box(width = 12, title = "Prediction Results",
            DTOutput("regressionResults")
        )
      )
    } else if(input$modelType == "Weather Condition Classification") {
      tagList(
        box(width = 12, title = "Weather Condition Classification",
            plotOutput("classificationPlot"),
            verbatimTextOutput("classificationSummary")
        ),
        box(width = 12, title = "Confusion Matrix",
            verbatimTextOutput("confusionMatrix")
        )
      )
    } else {
      tagList(
        box(width = 12, title = "Weather Pattern Clusters",
            plotOutput("clusterPlot"),
            verbatimTextOutput("clusterSummary")
        ),
        box(width = 12, title = "Cluster Characteristics",
            DTOutput("clusterTable")
        )
      )
    }
  })
  
  # Regression Model
  output$regressionPlot <- renderPlotly({
    req(weather_data())
    data <- weather_data() %>%
      mutate(datetime = as.Date(datetime)) %>%
      select(temp, dew, humidity, windgust, windspeed, winddir, pressure,
             cloudcover, visibility, solarradiation, uvindex, severerisk,
             hours_temp, hours_humidity, hours_windgust, hours_windspeed,
             hours_winddir, hours_pressure, hours_visibility, hours_cloudcover,
             hours_solarradiation, hours_severerisk, precip) %>%
      na.omit()
    
    set.seed(123)
    train_rows <- sample(1:nrow(data), size = 0.7 * nrow(data))
    training_data <- data[train_rows, ]
    testing_data <- data[-train_rows, ]
    
    model_lm <- lm(precip ~ ., data = training_data)
    predictions <- predict(model_lm, newdata = testing_data)
    predictions[predictions < 0] <- 0
    
    results <- data.frame(
      Actual = testing_data$precip,
      Predicted = predictions
    )
    
    p <- ggplot(results, aes(x = Actual, y = Predicted)) +
      geom_point(color = "#00CED1", alpha = 0.6) +
      geom_abline(intercept = 0, slope = 1, color = "red") +
      labs(title = "Actual vs Predicted Precipitation",
           x = "Actual Precipitation (mm)",
           y = "Predicted Precipitation (mm)") +
      theme_minimal()
    
    ggplotly(p) %>%
      layout(hovermode = "closest")
  })
  
  output$regressionSummary <- renderPrint({
    req(weather_data())
    data <- weather_data() %>%
      mutate(datetime = as.Date(datetime)) %>%
      select(temp, dew, humidity, windgust, windspeed, winddir, pressure,
             cloudcover, visibility, solarradiation, uvindex, severerisk,
             hours_temp, hours_humidity, hours_windgust, hours_windspeed,
             hours_winddir, hours_pressure, hours_visibility, hours_cloudcover,
             hours_solarradiation, hours_severerisk, precip) %>%
      na.omit()
    
    set.seed(123)
    train_rows <- sample(1:nrow(data), size = 0.7 * nrow(data))
    training_data <- data[train_rows, ]
    
    model_lm <- lm(precip ~ ., data = training_data)
    summary(model_lm)
  })
  
  output$regressionResults <- renderDT({
    req(weather_data())
    data <- weather_data() %>%
      mutate(datetime = as.Date(datetime)) %>%
      select(temp, dew, humidity, windgust, windspeed, winddir, pressure,
             cloudcover, visibility, solarradiation, uvindex, severerisk,
             hours_temp, hours_humidity, hours_windgust, hours_windspeed,
             hours_winddir, hours_pressure, hours_visibility, hours_cloudcover,
             hours_solarradiation, hours_severerisk, precip) %>%
      na.omit()
    
    set.seed(123)
    train_rows <- sample(1:nrow(data), size = 0.7 * nrow(data))
    training_data <- data[train_rows, ]
    testing_data <- data[-train_rows, ]
    
    model_lm <- lm(precip ~ ., data = training_data)
    predictions <- predict(model_lm, newdata = testing_data, interval = "prediction")
    predictions[predictions < 0] <- 0
    
    results <- data.frame(
      Actual = testing_data$precip,
      Predicted = predictions[, "fit"],
      Lower_Bound = predictions[, "lwr"],
      Upper_Bound = predictions[, "upr"]
    )
    
    datatable(results, options = list(scrollX = TRUE, pageLength = 5))
  })
  
  # Classification Model
  output$classificationPlot <- renderPlot({
    req(weather_data())
    data <- weather_data() %>%
      select(temp, dew, humidity, precip, windspeed, pressure, 
             cloudcover, visibility, hours_conditions) %>%
      na.omit()
    
    data$hours_conditions <- as.factor(data$hours_conditions)
    
    set.seed(123)
    train_index <- createDataPartition(data$hours_conditions, p = 0.7, list = FALSE)
    train_data <- data[train_index, ]
    test_data <- data[-train_index, ]
    
    nb_model <- naiveBayes(hours_conditions ~ ., data = train_data)
    predictions <- predict(nb_model, test_data)
    
    conf_matrix <- confusionMatrix(predictions, test_data$hours_conditions)
    
    ggplot(as.data.frame(conf_matrix$table), 
           aes(x = Reference, y = Prediction, fill = Freq)) +
      geom_tile() +
      geom_text(aes(label = Freq), color = "white") +
      scale_fill_gradient(low = "#00CED1", high = "#000080") +
      labs(title = "Confusion Matrix",
           x = "Actual Conditions",
           y = "Predicted Conditions") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$classificationSummary <- renderPrint({
    req(weather_data())
    data <- weather_data() %>%
      select(temp, dew, humidity, precip, windspeed, pressure, 
             cloudcover, visibility, hours_conditions) %>%
      na.omit()
    
    data$hours_conditions <- as.factor(data$hours_conditions)
    
    set.seed(123)
    train_index <- createDataPartition(data$hours_conditions, p = 0.7, list = FALSE)
    train_data <- data[train_index, ]
    
    nb_model <- naiveBayes(hours_conditions ~ ., data = train_data)
    print(nb_model)
  })
  
  output$confusionMatrix <- renderPrint({
    req(weather_data())
    data <- weather_data() %>%
      select(temp, dew, humidity, precip, windspeed, pressure, 
             cloudcover, visibility, hours_conditions) %>%
      na.omit()
    
    data$hours_conditions <- as.factor(data$hours_conditions)
    
    set.seed(123)
    train_index <- createDataPartition(data$hours_conditions, p = 0.7, list = FALSE)
    train_data <- data[train_index, ]
    test_data <- data[-train_index, ]
    
    nb_model <- naiveBayes(hours_conditions ~ ., data = train_data)
    predictions <- predict(nb_model, test_data)
    
    confusionMatrix(predictions, test_data$hours_conditions)
  })
  
  # Clustering Model
  output$clusterPlot <- renderPlot({
    req(weather_data())
    data <- weather_data() %>%
      select(starts_with("hours_"),
             -hours_datetime,
             -hours_conditions,
             -ends_with("_outlier"),
             -ends_with("_deviation")) %>%
      mutate(across(everything(), ~ as.numeric(.))) %>%
      mutate(across(everything(), ~ ifelse(is.infinite(.), NA, .))) %>%
      na.omit()
    
    scaled_data <- scale(data)
    
    set.seed(123)
    kmeans_result <- kmeans(scaled_data, centers = 3, nstart = 25)
    
    fviz_cluster(kmeans_result, data = scaled_data,
                 palette = c("#00CED1", "#000080", "#20B2AA"),
                 geom = "point",
                 ggtheme = theme_minimal()) +
      labs(title = "Weather Pattern Clusters (k=3)")
  })
  
  output$clusterSummary <- renderPrint({
    req(weather_data())
    data <- weather_data() %>%
      select(starts_with("hours_"),
             -hours_datetime,
             -hours_conditions,
             -ends_with("_outlier"),
             -ends_with("_deviation")) %>%
      mutate(across(everything(), ~ as.numeric(.))) %>%
      mutate(across(everything(), ~ ifelse(is.infinite(.), NA, .))) %>%
      na.omit()
    
    scaled_data <- scale(data)
    
    set.seed(123)
    kmeans_result <- kmeans(scaled_data, centers = 3, nstart = 25)
    
    cat("Cluster sizes:\n")
    print(kmeans_result$size)
    cat("\nCluster centers (scaled):\n")
    print(kmeans_result$centers)
  })
  
  output$clusterTable <- renderDT({
    req(weather_data())
    data <- weather_data() %>%
      select(starts_with("hours_"),
             -hours_datetime,
             -hours_conditions,
             -ends_with("_outlier"),
             -ends_with("_deviation")) %>%
      mutate(across(everything(), ~ as.numeric(.))) %>%
      mutate(across(everything(), ~ ifelse(is.infinite(.), NA, .))) %>%
      na.omit()
    
    scaled_data <- scale(data)
    
    set.seed(123)
    kmeans_result <- kmeans(scaled_data, centers = 3, nstart = 25)
    
    # Calculate mean values for each cluster
    data$cluster <- kmeans_result$cluster
    cluster_means <- data %>%
      group_by(cluster) %>%
      summarise(across(everything(), mean, na.rm = TRUE))
    
    datatable(cluster_means, options = list(scrollX = TRUE))
  })
  
}

shinyApp(ui, server)
