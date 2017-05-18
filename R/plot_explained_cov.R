#' Plot unexplained covariance part.
#'
#' @import rprojroot
#' @import ggplot2
#' @import cowplot
#' @import reshape2
#' @export
#'
plot_explained_cov <- function(fit,data,response,condition) {
  condition_names = data[,condition] %>% contrasts %>% rownames %>% rev
  beta = rstan::extract(fit_vb,pars = "beta")[[1]]
  col_number = 2
  beta_2_perc = apply(beta[,,col_number],MARGIN = 2,function(vec)
    quantile(vec,probs = c(0.025/nrow(beta),
                           0.5,
                           1-0.025/nrow(beta))))
  df = data.frame(x = factor(response, levels = response),
                  t(beta_2_perc))
  names(df) = c("x","low","mid","high")
  ggplot(df) +
    geom_segment(mapping=aes(x=x, y=low, xend=x, yend=high)) +
    geom_point(mapping=aes(x=x, y=mid)) +
    xlab("Channel") +
    ylab(paste0("Contrast: ",paste(condition_names,collapse = " vs. "))) +
    geom_hline(yintercept=0,col = "red") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
}
