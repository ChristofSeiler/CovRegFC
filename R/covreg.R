#' Covariance regression model using Stan
#'
#' @import rstan
#' @import rprojroot
#' @export
#'
covreg <- function(data,response,condition,seed = 231248) {

  num_chains = 1

  # load stan model from file
  file = find_package_root_file("R", "covreg.stan")
  model = stan_model(file = file,
                     model_name = "covreg_model")

  # build design matrix
  Y = data[,response] %>% as.matrix
  colnames(Y)
  X = model.matrix(as.formula(paste("~",condition)), data = data)
  attr(X, "assign") = NULL
  stan_data = list(K = ncol(Y),
                   J = ncol(X),
                   N = nrow(X),
                   x = X,
                   y = Y)

  # initializing with pooled empirical covariance matrix
  sample_cor = cor(Y)
  L_Omega = t(chol(sample_cor))
  L_sigma = sqrt(diag(cov(Y)))
  init_data = list(L_Omega = L_Omega,
                   L_sigma = L_sigma)
  init_data_all = NULL
  for(i in 1:num_chains)
    init_data_all[[i]] = init_data

  # run Stan with variational inference
  # seed = 231248
  fit = sampling(model,
                 data = stan_data,
                 init = init_data_all,
                 iter = 200,
                 chains = num_chains,
                 cores = num_chains,
                 seed = seed,
                 pars = c("L_sigma","Omega","beta"))
  # fit = vb(model,
  #          data = stan_data,
  #          init = init_data_all[[1]],
  #          seed = seed,
  #          pars = c("pi","beta"))
  fit
}
