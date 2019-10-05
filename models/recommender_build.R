library(data.table)
library(mxnet)

# initializer
mxnet_ini_build <- function(model_params) {
  initializer <- mx.init.Xavier(rnd_type = model_params$ini_rnd_type,
                                factor_type = model_params$ini_factor_type,
                                magnitude = model_params$ini_magnitude)
  return(initializer)
}

# metric
mxnet_metric_build <- function(model_params) {
  metric <- switch(model_params$eval_metric,
                   mae = mx.metric.mae,
                   mse = mx.metric.mse,
                   gini = mx.metric.gini,
                   logloss = mx.metric.logloss,
                   mlogloss = mx.metric.mlogloss,
                   logistic_acc = mx.metric.logistic_acc,
                   accuracy = mx.metric.accuracy,
                   RNN_gini = metric_RNN_gini,
                   RNN_mae = metric_RNN_mae,
                   RNN_logloss = metric_RNN_logloss,
                   identity = mx.metric.identity)
  return(metric)
}

mxnet_opt_build <- function(model_params) {
  optimizer <- switch(model_params$opt_type,
                      sgd = mx.opt.create(name = "sgd",
                                          learning.rate = model_params$opt_learning_rate,
                                          momentum = model_params$opt_momentum,
                                          wd = model_params$opt_wd),
                      adam = mx.opt.create(name = "adam",
                                           learning.rate = model_params$opt_learning_rate,
                                           beta1 = model_params$opt_beta1,
                                           beta2 = model_params$opt_beta2,
                                           epsilon = model_params$opt_epsilon,
                                           clip_gradient = 1,
                                           wd = model_params$opt_wd),
                      rmsprop = mx.opt.create(name = "rmsprop",
                                              learning.rate = model_params$opt_learning_rate,
                                              gamma1 = model_params$opt_gamma1,
                                              gamma2 = model_params$opt_gamma2,
                                              wd = model_params$opt_wd),
                      adadelta = mx.opt.create(name = "adadelta",
                                               eps = model_params$opt_eps,
                                               rho = model_params$opt_rho,
                                               wd = model_params$opt_wd)
  )
  return(optimizer)
}

##################################
# Basic MLP
##################################

mx_sym_basic <- function(model_params, index_levels) {

  data <- mx.symbol.Variable("data")
  label <- mx.symbol.Variable("label")

  hidden_weight <- mx.symbol.Variable("hidden_weight")
  hidden_bias <- mx.symbol.Variable("hidden_bias")

  proj_1_weight <- mx.symbol.Variable("proj_1_weight")
  proj_1_bias <- mx.symbol.Variable("proj_1_bias")

  proj_2_weight <- mx.symbol.Variable("proj_2_weight")
  proj_2_bias <- mx.symbol.Variable("proj_2_bias")

  num_embed <- model_params$embed_dim
  num_hidden <- as.integer(unlist(strsplit(as.character(model_params$num_hidden), split = ":")))
  p <- as.numeric(unlist(strsplit(as.character(model_params$p), split = ":")))
  act_type <- as.character(unlist(strsplit(as.character(model_params$act_type), split = ":")))

  # split data in 2
  # data <- mx.symbol.split(data = data, num_outputs = 2)

  num_embed_vars <- length(index_levels)/2

  # num features
  features <- mx.symbol.slice_axis(data = data, axis = -1, begin = 2*num_embed_vars, end="None")
  features <- mx.symbol.split(data = features, num_outputs = 2, axis = -1)

  # embedings
  if (num_embed_vars > 0) {

    embed_weights <- lapply(seq_len(num_embed_vars), function(x) mx.symbol.Variable(paste0(names(index_levels)[x], "embed_weight")))
    embed_levels <- c(num_embed, num_embed)

    embed_list <- list()
    for (i in 1:2) {
      for (j in 1:num_embed_vars) {
        embed <- mx.symbol.slice_axis(data = data, axis = -1, begin = (i-1)*num_embed_vars + j - 1, end = (i-1)*num_embed_vars + j)
        embed <- mx.symbol.reshape(data = embed, shape = 0)
        embed <- mx.symbol.Embedding(data = embed, weight = embed_weights[[j]], input_dim = index_levels[j], output_dim = embed_levels[j])
        embed_list <- c(embed_list, embed)
      }
      # features <- mx.symbol.slice_axis(data = data, axis = -1, begin = length(index_levels), end="None")
      # data <- mx.symbol.concat(c(embed, features), num.args = 2, dim = -1)
    }

    embed_1 <- mx.symbol.concat(c(embed_list[1:num_embed_vars], features[[1]]) , num.args = num_embed_vars+1, dim = -1)
    embed_2 <- mx.symbol.concat(c(embed_list[1:num_embed_vars + num_embed_vars], features[[2]]), num.args = num_embed_vars+1, dim = -1)

  } else {
    embed_1 <- features[[1]]
    embed_2 <- features[[2]]
  }

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

  # data <- mx.symbol.elemwise_mul(embed_1, embed_2)
  # data <- mx.symbol.Dropout(data, p=p)

  # final <- mx.symbol.FullyConnected(data, num_hidden = model_params$num_output, name = "final")

  loss <- switch(model_params$loss_type,
                 linear = mx.symbol.LinearRegressionOutput(data = final, label = label, name = "loss"),
                 mae = mx.symbol.MAERegressionOutput(data = final, label = label, name = "loss"),
                 logistic = mx.symbol.LogisticRegressionOutput(data = final, label = label, name = "loss"),
                 softmax = mx.symbol.SoftmaxOutput(data = final, label = label, name = "loss"))

  loss <- mx.symbol.Group(label, loss)

  return(loss)
}



#' Callback with logging and early stop
#'
#'@export
mx.callback.log.early.stop <- function(period, early_stop_rounds, logger=NULL, maximise) {
  function(iteration, nbatch, env, verbose) {
    if (nbatch %% period == 0 && !is.null(env$metric)) {
      result <- env$metric$get(env$train.metric)
      if (nbatch != 0)
        if(verbose) cat(paste0("Batch [", nbatch, "] Train-", result$name, "=", result$value, "\n"))
      if (!is.null(logger)) {
        if (class(logger) != "mx.metric.logger") {
          stop("Invalid mx.metric.logger.")
        }
        logger$train <- c(logger$train, result$value)
        if (!is.null(env$eval.metric)) {
          result <- env$metric$get(env$eval.metric)
          if (nbatch != 0)
            cat(paste0("Batch [", nbatch, "] Validation-", result$name, "=", result$value, "\n"))
          logger$eval <- c(logger$eval, result$value)

          if (maximise) opt_eval_id <- which.max(logger$eval) else
            opt_eval_id <- which.min(logger$eval)
          if(is.na(last(logger$eval))) return(FALSE) else
            if (length(logger$eval) - opt_eval_id == early_stop_rounds) return(FALSE)
        }
      }
    }
    return(TRUE)
  }
}

