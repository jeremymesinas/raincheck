install.packages("dotenv")
# Check first so as to not reload r every single time
if (!require("httr")) install.packages("httr", dependencies = TRUE)
if (!require("jsonlite")) install.packages("jsonlite", dependencies = TRUE)
if (!require("dplyr")) install.packages("dplyr", dependencies = TRUE)
if (!require("purrr")) install.packages("purrr", dependencies = TRUE)

#load libraries
library(httr)
library(jsonlite)
library(dplyr)
library(purrr)
library(dotenv)

# Load the .env file
dotenv::load_dot_env(file = ".env")

# Access the variables
api_key <- Sys.getenv("API_KEY")

# Set URL params
#api_key <- ""
lat <- 14.621795
lon <- 121.065476
url <- paste0("https://weather.visualcrossing.com/VisualCrossingWebServices/rest/services/timeline/",
              lat, ",", lon, "?key=", api_key)

# Make GET request 
tryCatch({
  response <- GET(url)
  
  # Check status code if successful
  if (status_code(response) == 200) {
    
    # Parse JSON
    content_json <- content(response, as = "text", encoding = "UTF-8")
    weather_data <- fromJSON(content_json, flatten = TRUE)
    
    # Check if the expected structure exists
    if (!is.null(weather_data$days)) {
      # extract hourly data
      hourly_data <- weather_data$days %>%
        
        # Ensure we have a data frame
        {if (!is.data.frame(.)) as.data.frame(.) else .} %>%
       
         # Unnest the hourly data if it exists
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
        # Clean the data
        hourly_clean <- hourly_data %>%
          select(where(~ !is.list(.)))
        
        # Add date column if not already present
        if (!"date" %in% names(hourly_clean) && "datetime" %in% names(hourly_clean)) {
          hourly_clean$date <- hourly_clean$datetime
        }
        
        # Save as CSV
        write.csv(hourly_clean, "weather_hourly_data.csv", row.names = FALSE)
        cat("Hourly weather data saved to 'weather_hourly_data.csv'\n")
        print(head(hourly_clean))
      } else {
        cat("No hourly data available to save\n")
      }
    } else {
      cat("'days' field not found in JSON response\n")
      print(names(weather_data)) # Print available fields for debugging
    }
  } else {
    cat("Failed to fetch data. Status code:", status_code(response), "\n")
    print(content(response, as = "text", encoding = "UTF-8")) 
  }
}, error = function(e) {
  cat("Error", e$message, "\n")
})