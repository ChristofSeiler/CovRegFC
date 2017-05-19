#' Trace plots of unexplained covariance parameters.
#'
#' @export
#'
plot_unexplained_cov_diagnostics <- function(fit,num_par = 8) {
  param = rstan::extract(fit,
                         pars = "Omega",
                         permuted = FALSE,
                         inc_warmup = TRUE)
  total_num_par = dim(param)[3]
  num_channels = sqrt(total_num_par)
  diagonal_names = paste0("Omega[",1:num_channels,",",1:num_channels,"]")
  all_names = dimnames(param)$parameters
  diagonal_ids = sapply(diagonal_names,function(name) which(all_names == name))
  offdiagonal_names = all_names[-diagonal_ids]
  plot_diagnostics(fit,offdiagonal_names,num_par)
}
