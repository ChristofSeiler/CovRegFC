#' Full covariance regression model using Stan
#'
#' @import rstan
#' @export
#'
covreg_full <- function(YY,X,N,scov_short_mean,scov_conventional_mean,seed) {

  stan_data = list(K = dim(YY)[1],
                   P = dim(YY)[2],
                   N = N,
                   YY = YY,
                   X = X)
  sigma_short = sqrt(diag(scov_short_mean))
  Omega_short =   cov2cor(scov_short_mean)
  sigma_conventional = sqrt(diag(scov_conventional_mean))
  Omega_conventional =   cov2cor(scov_conventional_mean)
  Omega = abind(Omega_short,Omega_conventional,along = 0)
  sigma = rbind(sigma_short,sigma_conventional)
  init_data = list(Omega = Omega,
                   sigma = sigma)
  init_data_all = NULL
  num_chains = 4
  for(i in 1:num_chains)
    init_data_all[[i]] = init_data
  file = system.file("exec", "covreg_full.stan", package = "CovRegFC")
  model = stan_model(file = file,
                     model_name = "covreg_full_model")

  sampling(model,
           data = stan_data,
           init = init_data_all,
           iter = 1000,
           chains = num_chains,
           cores = num_chains,
           seed = seed)

}
