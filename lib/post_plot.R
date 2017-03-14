#' post_plot.ggplot
#' 
#' \code(post_plot.ggplot) returns a ggplot object with certain added elements at the end.
#' 
#' The \code{\link[tis][dest=tis-package]{tis package}} function \code{\link[tis]{nberShade}}
#' draws polygons 
#' NBER shading, formatted y labels, and a repositioned legend.
#' 
#' that must be added after the plot with other elements is constructed.
#' Thie is a kludge primarily to work around \code{\link{nberShade.ggplot}} quirks.
#'
#' @param x A ggplot object
#' @param nber=TRUE Include NBER recession bars
#' @param nberFill A character string or variable indicating the color of the nber bars
#' @param yLabels A label parameter for formatting the y labels
#' @param legendPos A 2-item vector giving the coordinate for the legend box.
#'
#' @return The modified plot
#' @export
#'
#' @examples
# post_plot <- function(plot, nber = TRUE, nberFill = "#a6cee3", yLabels = comma, legendPos = c(0.25,.75), alpha = 0.5, ...) {
post_plot.ggplot <- function(x, nber = TRUE, yLabels = comma, legendPos = c(0.25,.75), fill="#a6cee3", ...) {
  if (nber) {
    new.x <- nberShade(x, fill = fill, ...) 
  }
  else
    new.x <- x
  new.x <- new.x + scale_y_continuous(labels = yLabels) + theme(legend.position = legendPos)
  return(new.x)
}