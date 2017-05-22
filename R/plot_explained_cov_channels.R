#' Plot channel explained cov in brain coordinates.
#'
#' @import ggplot2
#' @export
#'
plot_explained_cov_channels <- function(fit,data,response,condition,channel_location,x_label,y_label) {
  condition_names = data[,condition] %>% contrasts %>% rownames %>% rev
  beta = rstan::extract(fit,pars = "beta")[[1]]
  col_number = 2
  beta_2_perc = apply(beta[,,col_number],MARGIN = 2,function(vec)
    quantile(vec,probs = c(0.025/nrow(beta),
                           0.5,
                           1-0.025/nrow(beta))))
  df = data.frame(x = factor(response, levels = response),
                  t(beta_2_perc))
  names(df) = c("names","low","mid","high")
  df_combo = cbind(df,channel_location)
  df_combo$uncertainty = abs(df_combo$high-df_combo$low)
  detect_sign = function(a,b) {
    if(sign(a) > 0 & sign(b) > 0) { return("positive") }
    else if(sign(a) < 0 & sign(b) < 0) { return("negative") }
    else { return("unclear") }
  }
  df_combo$sign = sapply(1:nrow(df_combo),function(i)
    detect_sign(df_combo$low[i],df_combo$high[i])) %>%
    factor(.,levels = c("positive","negative","unclear"))
  corcir = circle(channel_location,c(0, 0), npoints = 100)
  ggplot(df_combo) +
    geom_path(data = corcir, aes(x = x, y = y), colour = "gray65") +
    geom_point(aes(x = x, y = y,size = uncertainty,color = sign)) +
    scale_size_continuous(range = c(3,6)) +
    theme(axis.line=element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank()) +
    xlab(x_label) +
    ylab(y_label) +
    coord_fixed()
}
