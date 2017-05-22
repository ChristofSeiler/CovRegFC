#' Trace plots for HMC.
#'
#' @import rstan
#' @import reshape2
#' @import ggplot2
#' @import cowplot
#' @export
#'
plot_diagnostics <- function(fit,par_name,num_par = 8) {
  # keep only num_par paramters to avoid overloaded plots
  param = rstan::extract(fit,
                         pars = par_name,
                         permuted = FALSE,
                         inc_warmup = TRUE)
  par_subset_ids = sample(dim(param)[3],size = num_par) %>% sort
  param_long_during = melt(param[1:fit@stan_args[[1]]$warmup,,par_subset_ids],
                      varnames = c("iteration","parameter"))
  param_long_during$phase = "during warmup"
  param_long_after = melt(param[(fit@stan_args[[1]]$warmup+1):dim(param)[1],,par_subset_ids],
                    varnames = c("iteration","parameter"))
  param_long_after$phase = "after warmup"
  param_long = rbind(param_long_during,param_long_after)
  param_long$phase = factor(param_long$phase,levels = c("during warmup","after warmup"))
  ggplot(param_long, aes(x = iteration, y = value, color = parameter)) +
    geom_line() +
    facet_wrap(~ phase) +
    ggtitle("Traceplot")
}
