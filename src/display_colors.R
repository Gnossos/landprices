#' display_colors
#' 
#' Plot a diagram displaying a set of colors
#'
#' @param colors A vector of colors. E.g., using the RColorBrewer package, it can be in the form: brewer.pal(8, "Dark2)[5] to select the 5th color and display it. 
#' @param max_colors A scalar indicating the maximum number of colors to display
#'
#' @return returns the image
#' @export
#'
#' @examples
display_colors <- function(colors, max_colors = length(colors)) {
  image(1:max_colors, 1, as.matrix(1:max_colors), col = colors, axes = FALSE, xlab = "", ylab = "")
}