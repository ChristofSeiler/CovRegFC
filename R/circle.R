#' Generate circle coordinate.
#'
circle = function(channel_location,center = c(0, 0), npoints = 100) {
  r = range(c(channel_location$x,channel_location$y)) %>% abs %>% max * 1.1
  tt = seq(0, 2*pi, length = npoints)
  xx = center[1] + r*cos(tt)
  yy = center[1] + r*sin(tt)
  return(data.frame(x = xx, y = yy))
}
