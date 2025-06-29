library(shiny)
library(shinydashboard)

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
      menuItem("Models", tabName = "models", icon = icon("database")), # Suggested icon for 'models'
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
      "))
    ),
    tabItems(
      tabItem(tabName = "map",
              h2("Map")),
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

server <- function(input, output) { }

shinyApp(ui, server)
