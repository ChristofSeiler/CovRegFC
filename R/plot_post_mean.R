#' Plot posterior mean correlation and standard deviation.
#'
#' @import rstan
#' @import reshape2
#' @import ggplot2
#' @import cowplot
#' @export
#'
plot_post_mean = function(fit,channel_names) {
  if (!inherits(fit, "stanfit"))
    stop("Not a stanfit object.")
  if (fit@mode != 0)
    stop("Stan model does not contain posterior draws.")

  # correlations
  Omega = rstan::extract(fit,pars = "Omega")[[1]]
  Omega_mean = apply(abs(Omega[,2,,]),c(2,3),mean)
  rownames(Omega_mean) = colnames(Omega_mean) = channel_names
  get_upper_tri = function(cormat) {
    cormat[lower.tri(cormat)] = NA
    cormat
  }
  abs_limit = Omega_mean[lower.tri(Omega_mean)] %>% range
  Omega_mean_long = melt(get_upper_tri(Omega_mean), na.rm = TRUE)
  p_corr = ggplot(data = Omega_mean_long, aes(Var2, Var1, fill = value)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                         midpoint = abs_limit[1], space = "Lab",
                         limit = abs_limit,
                         name = "Correlation") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          legend.title=element_blank()) +
    coord_fixed() +
    ggtitle("Correlation Magnitude")

  # standard devitations
  sigma = rstan::extract(fit,pars = "sigma")[[1]]
  sigma_median = apply(X = sigma[,2,],MARGIN = c(2),FUN = median)
  df_std = data.frame(channel = channel_names,sigma = sigma_median)
  df_std$channel = factor(df_std$channel,levels = channel_names)
  p_std = ggplot(df_std,aes(channel,sigma)) +
    geom_point(size = 3) +
    #geom_bar(colour="black", stat="identity") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0, hjust = 0),
          axis.title.x=element_blank(),
          axis.title.y=element_blank()) +
    ggtitle("SD") +
    coord_flip()

  # combine
  plot_grid(p_corr, p_std, labels = NULL, align = 'h', rel_widths = c(3,1))
}
