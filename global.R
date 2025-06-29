# Load required libraries
library(shiny)
library(shinydashboard)
library(leaflet)
library(httr)
library(jsonlite)
library(dplyr)
library(purrr)
library(dotenv)

# Load environment variables
dotenv::load_dot_env(file = ".env")

# Source all utility files
source("utils/api_utils.R")
source("utils/style_utils.R")

# Source all modules
source("modules/map_module.R")
#source("modules/weather_module.R")