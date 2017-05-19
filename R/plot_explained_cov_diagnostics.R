#' Trace plots of explained covariance parameters.
#'
#' @export
#'
plot_explained_cov_diagnostics <- function(fit,num_par = 8) {
  plot_diagnostics(fit,"beta",num_par)
}
