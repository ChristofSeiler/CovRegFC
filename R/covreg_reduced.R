#' Reduced covariance regression model using Stan
#'
#' @import rstan
#' @export
#'
covreg_reduced <- function(data,
                           response,
                           condition,
                           #batch,
                           participant,
                           seed = 112243) {

  num_chains = 1

  # load stan model from file
  file = system.file("exec", "covreg_reduced.stan", package = "CovRegFC")
  model = stan_model(file = file,
                     model_name = "covreg_reduced_model")

  # build design matrix
  Y = as.matrix(data[,response])
  colnames(Y)
  #X = model.matrix(as.formula(paste("~",condition,"+",batch)), data = data)
  X = model.matrix(as.formula(paste("~",condition,"+",participant)), data = data)
  #X = model.matrix(as.formula(paste("~",condition)), data = data)
  attr(X, "assign") = NULL
  stan_data = list(K = ncol(Y),
                   J = ncol(X),
                   N = nrow(X),
                   x = X,
                   y = Y)

  # initializing with pooled empirical covariance matrix
  sample_cor = cor(Y)
  sigma = sqrt(diag(cov(Y)))
  init_data = list(sigma = sigma)
  init_data_all = NULL
  for(i in 1:num_chains)
    init_data_all[[i]] = init_data

  sampling(model,
           data = stan_data,
           init = init_data_all,
           iter = 1000,
           chains = num_chains,
           cores = num_chains,
           seed = seed,
           pars = c("sigma","B"))
}
