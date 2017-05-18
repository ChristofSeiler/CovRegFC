#' Plot channel locations.
#'
#' @import ggplot2
#' @export
#'
plot_channels <- function(channel_location,x_label,y_label) {
  circle = function(center = c(0, 0), npoints = 100) {
    r = range(c(channel_location$x,channel_location$y)) %>% abs %>% max * 1.1
    tt = seq(0, 2*pi, length = npoints)
    xx = center[1] + r*cos(tt)
    yy = center[1] + r*sin(tt)
    return(data.frame(x = xx, y = yy))
  }
  corcir = circle(c(0, 0), npoints = 100)
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
