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

pred <- recommender(nationality = "Canada", hotel = "88 Studios")
