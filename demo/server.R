
library(shiny)
library(shinydashboard)
library(data.table)
library(sf)
library(RANN)
library(DT)
#library(mxnet)


# Load datasets -----------------------------------------------------------

dt_cities <- fread("data/worldcities.csv")
dt_cities[, label_search := city]
dt_cities[!(admin_name == ""), label_search := paste0(city, " (", admin_name, ")")]
dt_cities[country == "United States", country := "United States of America"]

dt_wiki_airports <- data.table(fromJSON("data/WIKIDATA_AIRPORT_passagers.json"))
dt_wiki_airports[, label_search := paste0(itemLabel, " (", iatacode, ")")]
dt_wiki_airports <- dt_wiki_airports[, (c("iatacode", "longitude", "latitude", "label_search", "paysLabel")), with = FALSE]

dt_reviews <- fread("data/Hotel_Reviews.csv")
dt_reviews <- dt_reviews[, c("Hotel_Name", "Hotel_Address", "lng", "lat")]
dt_reviews <- dt_reviews[!duplicated(dt_reviews[, c("Hotel_Name", "lng", "lat")])]
dt_reviews <- dt_reviews[!is.na(lng) & !is.na(lat)]


radius_search <- function(pt) {
  q <- nn2(data = dt_reviews[, c("lng", "lat")], query = data.table(pt), radius = 1, k = min(200, nrow(data)))
  dt <- dt_reviews[as.integer(q$nn.idx)]
  return(dt)
}


# Server for app ----------------------------------------------------------

server <- function(input, output, session) {
    
    hotel_cities <- reactive({
      list(
          city = input$hotel_cities,
          lat = dt_cities[label_search == input$hotel_cities,]$lat,
          lon = dt_cities[label_search == input$hotel_cities,]$lng
        )
    })
    
    dt_hotels_filtered <- reactive({
      radius_search(pt = data.table(lng = hotel_cities()$lon, lat = hotel_cities()$lat))
    })
    
    default_hotel_city <- reactive({
      dt_cities[country == dt_wiki_airports[label_search == input$location_to,]$paysLabel & capital == "primary",]$city
    })
    
    output$test <- renderText({
      print(default_hotel_city())
    })
    
    observeEvent(input$submit, {
      updateSelectizeInput(session, "hotel_cities",
                           selected = dt_cities[city == default_hotel_city() & capital == "primary",]$label_search
      )
    })
    
    
    # Ajout variable reactive de nation pour le filtering
    nation_from <- reactive({
      nation <- dt_wiki_airports[label_search == input$location_from,]$paysLabel
      if (nation == "People's Republic of China") {
        nation <- "China"
      }
      nation
    })
    
    
    # Sorter les hotels 
    
    # Afficher le data.table d'hotels
    output$hotels_recommended <- renderDataTable({
      datatable(dt_hotels_filtered()[, c("Hotel_Name", "Hotel_Address")], 
                colnames = c("Hotel Name", "Hotel Adress"),
                rownames = FALSE,
                options = list(
                  pageLength = 10,
                  initComplete = JS(
                    "function(settings, json) {",
                    "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                    "}")
                ))
    })
    
}

shinyServer(server)
