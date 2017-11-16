library(shiny)
library(arules)
library(arulesViz)
library(shinydashboard)
library(jsonlite)
library(httr)
library(fireData)
library(shinyBS)
library(ggplot2)
library(xts)
library(lubridate)
library(ggmap)
library(leaflet)
library(googleway)
library(plotly)
library(DT)
library(data.table)
library(shinythemes)
library(qpcR)
library(zoo)
library(forecast)

# import all the helper functions
source(file.path("helper", "helper_functions.R"),  local = TRUE)$value

## Testing commit

#######################################
##### Initialize global variables #####
#######################################

# Initialize summaryTable
summaryTable <- getSummaryTable()

# Initialize DP Matrix
current <- updateServiceMatrix()

## Google api keys
google_api_key <- "AIzaSyDYNsZrtv9hzi25no8QtdyCf2ukrjssqA0"
google_dir_key <- "AIzaSyCdv_FPO47Hjcn4BT_zc12tlUIBj9XRG-Q"
google_java_key <- "AIzaSyCcEfR68HfFZbE3f-IcwXvRaEphuLqQjtM"


# UI Section
ui <- fluidPage(
  theme = shinytheme("united"),
  navbarPage("FlexiBus",
             source(file.path("ui", "james_ui.R"),  local = TRUE)$value,
             source(file.path("ui", "wee_seng_ui.R"),  local = TRUE)$value,
             source(file.path("ui", "shriya_ui.R"),  local = TRUE)$value,
             source(file.path("ui", "jianzhi_ui.R"),  local = TRUE)$value,
             source(file.path("ui", "yuan_yuan_ui.R"),  local = TRUE)$value
  )
)

# Server Section
server <- function(input, output, session) {
  source(file.path("server", "james_server.R"),  local = TRUE)$value
  source(file.path("server", "shriya_server.R"),  local = TRUE)$value
  source(file.path("server", "yuan_yuan_server.R"),  local = TRUE)$value
  source(file.path("server", "jianzhi_server.R"),  local = TRUE)$value
  source(file.path("server", "wee_seng_server.R"),  local = TRUE)$value
  
}


shinyApp(ui = ui, server = server)