rm(list=ls())
library(mxnet)
library(data.table)

model <- mx.model.load(prefix = "recommender", iteration = 8)
nationality_table <- read_fst("nationality_table.fst", as.data.table = T)
dt_hotel <- read_fst("dt_hotel.fst", as.data.table = T)

setkeyv(nationality_table, "nationality")
setkeyv(dt_hotel, "Hotel_Name")

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

pred_canada <- recommender(nationality = "Canada", hotel = sort(unique(dt_hotel$Hotel_Name)))
pred_france <- recommender(nationality = "France", hotel = sort(unique(dt_hotel$Hotel_Name)))
pred_india <- recommender(nationality = "India", hotel = sort(unique(dt_hotel$Hotel_Name)))
pred_sweden <- recommender(nationality = "Sweden", hotel = sort(unique(dt_hotel$Hotel_Name)))

pred_canada <- pred_canada[order(-infer)]
pred_france <- pred_france[order(-infer)]
pred_india <- pred_india[order(-infer)]
pred_sweden <- pred_sweden[order(-infer)]

top_canada <- pred_canada$Hotel_Name[1:10]
top_france <- pred_france$Hotel_Name[1:10]
top_india <- pred_india$Hotel_Name[1:10]
top_sweden <- pred_sweden$Hotel_Name[1:10]

setdiff(top_canada, top_france)
setdiff(top_canada, top_india)
setdiff(top_canada, top_sweden)

hist(pred_canada$infer)
hist(pred_france$infer)
