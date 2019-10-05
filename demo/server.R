
library(shiny)
library(shinydashboard)
library(data.table)
library(sf)
library(RANN)
library(DT)
library(mxnet)
library(fst)


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

model <- mx.model.load(prefix = "data/recommender", iteration = 8)
nationality_table <- read_fst("data/nationality_table.fst", as.data.table = T)
dt_hotel <- read_fst("data/dt_hotel.fst", as.data.table = T)
setkeyv(nationality_table, "nationality")
setkeyv(dt_hotel, "Hotel_Name")



# Fonctions ---------------------------------------------------------------

radius_search <- function(pt) {
  q <- nn2(data = dt_reviews[, c("lng", "lat")], query = data.table(pt), radius = 1, k = min(200, nrow(data)))
  dt <- dt_reviews[as.integer(q$nn.idx)]
  return(dt)
}

recommender <- function(nationality, hotel) {
  dt <- data.table(nationality = nationality, Hotel_Name = hotel)
  setkeyv(dt, "nationality")
  dt <- nationality_table[dt]
  setkeyv(dt, "Hotel_Name")
  dt <- dt_hotel[dt]

  data <- dt[, c("nationality_index", paste0("embed_", 1:10))]
  label <- rep(0, nrow(dt))
  iter <- mx.io.arrayiter(data = t(as.matrix(data)), label = label, batch.size = nrow(dt), shuffle = F)
  pred_eval <- as.numeric(predict(model, iter))
  dt[, infer := pred_eval]
  dt <- dt[, c("nationality", "Hotel_Name", "infer")]
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
    hotels_sorted <- reactive({
      recommender(nationality = nation_from(), hotel = dt_hotels_filtered()$Hotel_Name)
    })
    
    # Afficher le data.table d'hotels
    output$hotels_recommended <- renderDataTable({
      datatable(hotels_sorted()[order(-infer),][, c("Hotel_Name")],
                colnames = c("Hotel Name"),
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
