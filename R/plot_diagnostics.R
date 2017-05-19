#' Trace plots for HMC.
#'
#' @import rstan
#' @import reshape2
#' @import ggplot2
#' @import cowplot
#' @export
#'
plot_diagnostics <- function(fit,par_name,num_par = 8) {

  # during warmup
  param = rstan::extract(fit,
                         pars = par_name,
                         permuted = FALSE,
                         inc_warmup = TRUE)
  # keep only 8 paramters to avoid an overcrowed plot
  par_subset_ids = sample(dim(param)[3],size = num_par) %>% sort
  param_long = melt(param[1:fit@stan_args[[1]]$warmup,,par_subset_ids],
                      varnames = c("iteration","chain","parameter"))
  p1 = ggplot(param_long, aes(x = iteration, y = value, color = parameter)) +
    geom_line() +
    facet_wrap(~ chain) +
    ggtitle("During Warmup")

  # after warmup
  param_long = melt(param[fit@stan_args[[1]]$warmup:dim(param)[1],,par_subset_ids],
                      varnames = c("iteration","chain","parameter"))
  p2 = ggplot(param_long, aes(x = iteration, y = value, color = parameter)) +
    geom_line() +
    facet_wrap(~ chain) +
    ggtitle("After Warmup")

  # combine
  plot_grid(p1,p2,nrow = 2,ncol = 1,align = "v")
}
