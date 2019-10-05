
library(shiny)
library(shinydashboard)
library(data.table)
#library(mxnet)

dt_cities <- fread("data/worldcities.csv")
dt_cities[, label_search := paste0(city, " (", admin_name, ")")]
dt_wiki_airports <- data.table(fromJSON("data/WIKIDATA_AIRPORT_passagers.json"))
dt_wiki_airports[, label_search := paste0(itemLabel, " (", iatacode, ")")]
dt_wiki_airports <- dt_wiki_airports[, (c("iatacode", "longitude", "latitude", "label_search")), with = FALSE]

server <- function(input, output, server) {
    
    hotel_cities <- reactive({
      list(
          city = input$hotel_cities,
          lat = dt_cities[label_search == input$hotel_cities,]$lat,
          lon = dt_cities[label_search == input$hotel_cities,]$lng
        )
    })
    
    
    nation <- reactive({
      dt_
    })
    
}

shinyServer(server)
