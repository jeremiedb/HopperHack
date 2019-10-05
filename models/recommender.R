rm(list=ls())
library(data.table)
library(fst)
library(mxnet)
library(ggplot2)

data <- mx.symbol.Variable("data")
label <- mx.symbol.Variable("label")

hidden_weight <- mx.symbol.Variable("hidden_weight")
hidden_bias <- mx.symbol.Variable("hidden_bias")

proj_1_weight <- mx.symbol.Variable("proj_1_weight")
proj_1_bias <- mx.symbol.Variable("proj_1_bias")

proj_2_weight <- mx.symbol.Variable("proj_2_weight")
proj_2_bias <- mx.symbol.Variable("proj_2_bias")

num_embed <- 2
num_hidden <- 16
p <- 0.5
act_type <- "relu"

index_levels <- c("nationality"=227)
num_embed_vars <- length(index_levels)

# num features
hotel_features <- mx.symbol.slice_axis(data = data, axis = -1, begin = num_embed_vars, end="None", name = "hotel_features")

# embedings
embed_weights <- lapply(seq_len(num_embed_vars), function(x) mx.symbol.Variable(paste0(names(index_levels)[x], "embed_weight")))

embed_list <- NULL
for (j in 1:num_embed_vars) {
  embed <- mx.symbol.slice_axis(data = data, axis = -1, begin = j-1, end = j, name = paste0("slice_", names(index_levels)[j]))
  embed <- mx.symbol.reshape(data = embed, shape = 0, name = paste0("reshape_", names(index_levels)[j]))
  embed <- mx.symbol.Embedding(data = embed, weight = embed_weights[[j]], input_dim = index_levels[j], output_dim = num_embed, name = paste0("embed_", names(index_levels)[j]))
  embed_list <- c(embed_list, embed)
}

embed_1 <- mx.symbol.concat(c(embed_list[1:1]), num.args = 1, dim = -1)
embed_2 <- hotel_features

embed_1 <- mx.symbol.FullyConnected(data = embed_1, num_hidden=num_hidden) %>%
  # mx.symbol.Dropout(p=p[1]) %>%
  mx.symbol.BatchNorm() %>%
  mx.symbol.Activation(act_type = "tanh")

# embed_1 <- mx.symbol.Dropout(data = embed_1, p=p[1])
# embed_1 <- mx.symbol.FullyConnected(data = embed_1, weight = proj_1_weight, bias = proj_1_bias, num_hidden=num_hidden) %>%
#   mx.symbol.Activation(act_type = "relu")
# embed_1 <- mx.symbol.FullyConnected(data = embed_1, weight = proj_2_weight, bias = proj_2_bias, num_hidden=num_hidden)

embed_2 <- mx.symbol.FullyConnected(data = embed_2, num_hidden=num_hidden) %>%
  # mx.symbol.Dropout(p=p[1]) %>%
  mx.symbol.BatchNorm() %>%
  mx.symbol.Activation(act_type = "tanh")

# embed_2 <- mx.symbol.Dropout(data = embed_2, p=p[1])
# embed_2 <- mx.symbol.FullyConnected(data = embed_2, weight = proj_1_weight, bias = proj_1_bias, num_hidden=num_hidden) %>%
#   mx.symbol.Activation(act_type = "relu")
# embed_2 <- mx.symbol.FullyConnected(data = embed_2, weight = proj_2_weight, bias = proj_2_bias, num_hidden=num_hidden)

embed_1 <- mx.symbol.expand_dims(embed_1, axis = 1)
embed_2 <- mx.symbol.expand_dims(embed_2, axis = 2)
final  <- mx.symbol.batch_dot(embed_1, embed_2)
final  <- mx.symbol.reshape(final, shape = c(0,0))
final <- mx.symbol.FullyConnected(data = final, num_hidden=1)

loss <- mx.symbol.LogisticRegressionOutput(data = final, label = label, name = "loss")

# loss$arguments
graph.viz(loss, shape = list(data=c(11, 128), label=128))


dt <- read_fst("../data/score_hotel_embeddings.fst", as.data.table = T)
vars <- paste0("embed_", 1:10)
dt[, (vars) := lapply(.SD, function(x) mean(x)), .SDcols = vars, by = "Hotel_Name"]

# lead features
train_id <- sample(nrow(dt))[1:400000]
nationality_levels <- sort(unique(dt$Reviewer_Nationality))
dt[, nationality_index := as.integer(factor(Reviewer_Nationality, levels = nationality_levels))-1]

# save nationality table
nationality_table <- data.table(nationality = nationality_levels, nationality_index = 1:length(nationality_levels)-1)
write_fst(nationality_table, "nationality_table.fst")

# save hotel embeddings
dt_hotel <- dt[!duplicated(dt[, c("Hotel_Name")])]
dt_hotel <- dt_hotel[, c("Hotel_Name", paste0("embed_", 1:10))]
write_fst(dt_hotel, "dt_hotel.fst")

dt_train <- dt[train_id]
dt_eval <- dt[-train_id]

data <- dt[, c("nationality_index", paste0("embed_", 1:10))]
label <- dt$Reviewer_Score / 10
X_train <- as.matrix(data[train_id, ])
X_eval <- as.matrix(data[-train_id, ])
Y_train <- label[train_id]
Y_eval <- label[-train_id]

batch.size <- 128
iter_train <- mx.io.arrayiter(data = t(X_train), label = Y_train, batch.size = batch.size, shuffle = F)
iter_eval <- mx.io.arrayiter(data = t(X_eval), label = Y_eval, batch.size = batch.size, shuffle = F)

initializer <- mx.init.Xavier(rnd_type = "gaussian", factor_type = "avg", magnitude = 1)
# optimizer <- mx.opt.create(name = "adadelta", rho = 0.9, epsilon = 1e-7,
#                            wd = 1e-8, rescale.grad = 1, clip_gradient = 1)
optimizer <- mxnet:::mx.opt.adam(learning.rate = 0.001, beta1 = 0.9, beta2 = 0.999, wd = 1e-5, rescale.grad = 1 / batch.size, clip_gradient = 1)

metric <- mx.metric.mse
batch.end.callback <- mx.callback.log.speedometer(batch.size = batch.size, frequency = 50)
epoch.end.callback <- mx.callback.log.train.metric(period = 1)

# ctx <- mx.cpu(0)
ctx <- mx.gpu(0)

mx.set.seed(123)
model <- mx.model.buckets(symbol = loss,
                          train.data = iter_train,
                          eval.data = iter_eval,
                          ctx=ctx,
                          num.round=8,
                          optimizer = optimizer,
                          initializer = initializer,
                          metric=metric,
                          kvstore = "local",
                          batch.end.callback = batch.end.callback,
                          epoch.end.callback = epoch.end.callback)

mxnet::mx.model.save(model = model, prefix = "recommender", iteration = 8)
mxnet::mx.nd.save(ndarray = model$arg.params$nationalityembed_weight, filename = "nationalityembed_weight.params")

nationality_weight <- t(as.array(model$arg.params$nationalityembed_weight))

nationality_embed_dt <- data.table(nationality_weight)
nationality_embed_dt <- cbind(nationality_table, nationality_embed_dt)

country_list <- c("Canada", "France", "India", "Norway", "Sweden", "Germany", "United States of America", "Mexico", "Japan", "China", "Belgium", "Brazil", "United Kingdom")
embed_ids <- match(country_list, nationality_embed_dt$nationality)
embeddings_sample <- nationality_embed_dt[embed_ids, ]

p <- ggplot(embeddings_sample, aes(x = V1, y = V2, label = nationality))
p <- p + geom_text(check_overlap = F, color="navy") + theme_bw() +  theme(panel.grid=element_blank()) +
  labs(x = "embed 1", y = "embed 2")

ggplot2::ggsave(p, filename = "country_embed.png")
