#' Plot unexplained covariance part.
#'
#' @import ggplot2
#' @import reshape2
#' @import cowplot
#' @export
#'
plot_unexplained_cov <- function(fit,response) {

  # correalations
  Omega = rstan::extract(fit,pars = "Omega")[[1]]
  Omega_median = apply(X = Omega,MARGIN = c(2,3),FUN = median)
  rownames(Omega_median) = colnames(Omega_median) = response
  get_upper_tri = function(cormat) {
    cormat[lower.tri(cormat)] = NA
    cormat
  }
  Omega_long = melt(get_upper_tri(Omega_median), na.rm = TRUE)
  ggobj_corr = ggplot(data = Omega_long, aes(Var2, Var1, fill = value)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                         midpoint = 0, space = "Lab",
                         limit = c(-1,1),
                         name = "") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1),
          axis.title.x=element_blank(),
          axis.title.y=element_blank()) +
    coord_fixed() +
    ggtitle("Correlations")

  # standard deviations
  sigma = rstan::extract(fit,pars = "L_sigma")[[1]]
  sigma_median = apply(X = sigma,MARGIN = c(2),FUN = median)
  df_std = data.frame(channel = response,sigma = sigma_median)
  df_std$channel = factor(df_std$channel,levels = response)
  ggobj_std = ggplot(df_std,aes(channel,sigma)) +
    geom_bar(colour="black", stat="identity") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1),
          axis.title.x=element_blank(),
          axis.title.y=element_blank()) +
    ggtitle("Standard Deviations") +
    coord_flip()

  # combine
  plot_grid(ggobj_corr, ggobj_std, labels = NULL, align = 'h', rel_widths = c(2,1))
}
