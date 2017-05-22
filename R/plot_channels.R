#' Plot channel locations.
#'
#' @import ggplot2
#' @export
#'
plot_channels <- function(channel_location,x_label,y_label) {
  corcir = circle(channel_location,c(0, 0), npoints = 100)
  ggplot(channel_location) +
    geom_path(data = corcir, aes(x = x, y = y), colour = "gray65") +
    geom_text(aes(x = x, y = y, label = names)) +
    theme(#axis.title = element_blank(),
      axis.line=element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank()) +
    xlab(x_label) +
    ylab(y_label) +
    coord_fixed()
}
