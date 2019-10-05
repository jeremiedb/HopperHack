
data <- mx.symbol.Variable("data")
label <- mx.symbol.Variable("label")

hidden_weight <- mx.symbol.Variable("hidden_weight")
hidden_bias <- mx.symbol.Variable("hidden_bias")

proj_1_weight <- mx.symbol.Variable("proj_1_weight")
proj_1_bias <- mx.symbol.Variable("proj_1_bias")

proj_2_weight <- mx.symbol.Variable("proj_2_weight")
proj_2_bias <- mx.symbol.Variable("proj_2_bias")

num_embed <- 32
num_hidden <- 16
p <- 0.5
num_hidden <- as.integer(unlist(strsplit(as.character(num_hidden), split = ":")))
act_type <- "relu"

index_levels <- c("user"=20, "hotel"=200)
num_embed_vars <- length(index_levels)

# num features
features <- mx.symbol.slice_axis(data = data, axis = -1, begin = 2*num_embed_vars, end="None")
features <- mx.symbol.split(data = features, num_outputs = 2, axis = -1)

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
embed_2 <- mx.symbol.concat(c(embed_list[2:2]), num.args = 1, dim = -1)

embed_1 <- mx.symbol.FullyConnected(data = embed_1, weight = proj_1_weight, bias = proj_1_bias, num_hidden=num_hidden) %>%
  # mx.symbol.Dropout(p=p[1]) %>%
  mx.symbol.BatchNorm() %>%
  mx.symbol.Activation(act_type = "relu")
# embed_1 <- mx.symbol.Dropout(data = embed_1, p=p[1])
# embed_1 <- mx.symbol.FullyConnected(data = embed_1, weight = proj_1_weight, bias = proj_1_bias, num_hidden=num_hidden) %>%
#   mx.symbol.Activation(act_type = "relu")
# embed_1 <- mx.symbol.FullyConnected(data = embed_1, weight = proj_2_weight, bias = proj_2_bias, num_hidden=num_hidden)

embed_2 <- mx.symbol.FullyConnected(data = embed_2, weight = proj_1_weight, bias = proj_1_bias, num_hidden=num_hidden) %>%
  # mx.symbol.Dropout(p=p[1]) %>%
  mx.symbol.BatchNorm() %>%
  mx.symbol.Activation(act_type = "relu")
# embed_2 <- mx.symbol.Dropout(data = embed_2, p=p[1])
# embed_2 <- mx.symbol.FullyConnected(data = embed_2, weight = proj_1_weight, bias = proj_1_bias, num_hidden=num_hidden) %>%
#   mx.symbol.Activation(act_type = "relu")
# embed_2 <- mx.symbol.FullyConnected(data = embed_2, weight = proj_2_weight, bias = proj_2_bias, num_hidden=num_hidden)

embed_1 <- mx.symbol.expand_dims(embed_1, axis = 1)
embed_2 <- mx.symbol.expand_dims(embed_2, axis = 2)
final  <- mx.symbol.batch_dot(embed_1, embed_2)
final  <- mx.symbol.reshape(final, shape = c(0,0))
final <- mx.symbol.FullyConnected(data = final, num_hidden=1)

loss <- mx.symbol.LinearRegressionOutput(data = final, label = label, name = "loss")

loss$arguments

graph.viz(loss)
