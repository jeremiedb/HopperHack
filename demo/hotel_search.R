library(data.table)
library(sf)
library(RANN)

dt <- fread("../data/Hotel_Reviews.csv", data.table = T)
dt <- dt[, c("Hotel_Name", "lng", "lat")]
dt <- dt[!duplicated(dt[, c("Hotel_Name", "lng", "lat")])]
dt <- dt[!is.na(lng) & !is.na(lat)]
head(dt)

radius_search <- function(pt) {
  q <- nn2(data = dt[, c("lng", "lat")], query = data.table(pt), radius = 1)
  dt <- dt[as.integer(q$nn.idx)]
  return(dt)
}


pt <- data.table(lng= 4.916, lat = 52.36)
radius_search(pt = pt)
