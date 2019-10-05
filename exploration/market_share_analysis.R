
# Load libraries ----------------------------------------------------------

library(data.table)
library(jsonlite)


# Load and merge data -----------------------------------------------------

# Hopper data
dt_hopper_airports <- fread("exploration/hopper_airports.csv")
dt_hopper_flights <- fread("exploration/hopper_shop_flights_2018.csv")
dt_hopper <- merge(dt_hopper_flights, dt_hopper_airports[, (c("iata_code", "country_code", "name")), with = F], by.x = "airpot_code", by.y = "iata_code", all.x = TRUE)
setnames(dt_hopper, old = c("airpot_code", "nb_flights_shop", "country_code", "name"), new = c("iata_code", "hopper_flights", "airport_country_code", "airport_name"))
         
# Wiki data
dt_wiki_airports <- data.table(fromJSON("exploration/WIKIDATA_AIRPORT_passagers.json"))
setnames(dt_wiki_airports, old = c("MAX_patronage", "paysLabel", "countryFlag"), new = c("wikidata_flights", "airport_country_name", "country_flag"))

# Merge data
dt_flights <- merge(dt_hopper, dt_wiki_airports[, (c("iatacode", "wikidata_flights", "airport_country_name", "country_flag")), with = F], by.x = "iata_code", by.y = "iatacode", all.x = TRUE)

# Compute market variables
dt_flights[, hopper_market_ratio := hopper_flights/wikidata_flights]



