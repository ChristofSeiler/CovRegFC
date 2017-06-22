#' Plot brain slices.
#'
#' @import ggplot2
#' @export
#'
plot_brain <- function(brain_slice,title) {
  img = readPNG(brain_slice)
  m = 10 # margin
  d = dim(img)
  img = img[m:(d[1]-m),m:(d[2]-m),]
  d = dim(img)
  ggplot() +
    annotation_raster(img, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
    geom_point() +
    coord_fixed(ratio = d[1]/d[2]) +
    annotate("text",x = 0,y = 0,label = title,size = 8,color = "white") +
    theme_void()
}
