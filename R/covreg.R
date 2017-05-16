#' Covariance regression model using Stan
#'
#' @import rstan
#' @import rprojroot
#' @export
#'
covreg <- function(fmean,fcov,data,R) {

  # build design matrix

  set.seed(12345)
  #num_markers = 3
  num_cells = as.integer(params$num_cells)
  num_cells
  df_samples_subset = subset(df_samples,donor == "160512")
  subsample_ids = sample(x = nrow(df_samples_subset),
                         size = num_cells,
                         replace = FALSE)
  # cell_n = round(num_cells/length(levels(df_samples$donor)))
  # subsample_ids = lapply(levels(df_samples$donor),function(donor_id) {
  #   all_ids = which(df_samples$donor == donor_id)
  #   sample(x = all_ids,size = cell_n)
  # }) %>% unlist
  df_samples_subset = df_samples[subsample_ids,]
  Y = as.matrix( asinh((df_samples_subset[,1:num_markers])/5) )
  colnames(Y)
  ggcorr(Y)
  treatmentH1 = ifelse(df_samples_subset$treatment == "UI", 0, 1)
  X = cbind(intercept=1,treatmentH1)
  # table(df_samples_subset$donor)
  # X = model.matrix(formula(~ treatment + donor), df_samples_subset)
  colnames(X)
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
  for(i in 1:num_chains)
    init_data_all[[i]] = init_data

  # run Stan with variational inference

  fit_vb = vb(model,
              data = stan_data,
              init = init_data_all,
              seed = seed) #,
              #pars = c("pi","beta"))
  fit_vb

}
