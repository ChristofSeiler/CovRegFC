#' Plot unexplained covariance part.
#'
#' @import rprojroot
#' @import ggplot2
#' @import cowplot
#' @import reshape2
#' @export
#'
plot_explained_cov <- function(fit,data,response,condition,channel_location,x_label,y_label) {

  condition_names = data[,condition] %>% contrasts %>% rownames %>% rev
  beta = rstan::extract(fit,pars = "beta")[[1]]
  col_number = 2
  beta_2_perc = apply(beta[,,col_number],MARGIN = 2,function(vec)
    quantile(vec,probs = c(0.025/nrow(beta),
                           0.5,
                           1-0.025/nrow(beta))))
  df = data.frame(x = factor(response, levels = response),
                  t(beta_2_perc))
  names(df) = c("channel_name","low","mid","high")
  matching_ids = sapply(as.character(df$channel_name),function(channel_name)
    str_detect(channel_name,channel_location$names) %>% which)
  df_combo = cbind(df,channel_location[matching_ids,])
  df_combo$uncertainty = abs(df_combo$high-df_combo$low)
  detect_sign = function(a,b) {
    if(sign(a) > 0 & sign(b) > 0) { return("positive") }
    else if(sign(a) < 0 & sign(b) < 0) { return("negative") }
    else { return("unclear") }
  }
  df_combo$sign = sapply(1:nrow(df_combo),function(i)
    detect_sign(df_combo$low[i],df_combo$high[i])) %>%
    factor(.,levels = c("positive","negative","unclear"))
  df_combo$channel_name = factor(df_combo$channel_name,levels = rev(df_combo$channel_name))

  p_coeff = ggplot(df_combo, aes(x=mid, y=channel_name)) +
    geom_vline(xintercept = 0, col = "red") +
    geom_point() +
    geom_segment(mapping=aes(x=low, y=channel_name, xend=high, yend=channel_name)) +
    labs(title = paste0("Contrast: ",paste(condition_names,collapse = " vs. "))) +
    xlab("Coefficient") +
    theme(axis.title.y = element_blank())

  coor_brain = brain(channel_location,c(0, 0), npoints = 100)
  p_brain = ggplot(df_combo) +
    geom_path(data = coor_brain, aes(x = x, y = y), colour = "gray65") +
    geom_point(aes(x = x, y = y,size = uncertainty,color = sign)) +
    geom_text(aes(x = x, y = y, label = names)) +
    scale_size_continuous(range = c(3,6)) +
    theme(axis.line=element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank()) +
    xlab(x_label) +
    ylab(y_label) +
    coord_fixed()

  plot_grid(p_coeff,p_brain,rel_widths = c(1,1.5),align = "h")
}
