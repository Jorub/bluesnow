#' Fractions of dark, medium, light and total blue
#'
#' A simple function that reads that takes the ouput of BB_filter and calculates the proportion or fraction of total blue pixels, dark blue pixels, medium blue pixels and light blue pixels.
#'
#' @param filtered_image the returned matrix of the function BB_filter with values 0, 1, 2 and 3
#'
#' @return The fraction of total, dark, medium and light blue
#'
#' @author Johanna Bloecher, \email{bloecher@fzp.czu.cz}
#'
#' @examples
#' library(jpeg)
#' raw_image <- <- readJPEG(system.file("example.jpg", package = "bluesnow"))
#' ## Apply filter to raw image data
#' BB_filtered_image <- BB_filter(rgb_image = raw_image)
#' BB_fraction(filtered_image = BB_filtered_image)
#' @export


BB_fraction <- function(filtered_image) {
  all_pixels <- as.data.frame(which(filtered_image == 0 | filtered_image == 1 | filtered_image == 2 | filtered_image == 3))
  total_pixels <- dim(all_pixels)[1]
  colnames(all_pixels) <- 'all_pixels'
  all_blues <- as.data.frame(which(filtered_image == 1 | filtered_image == 2 | filtered_image == 3))
  colnames(all_blues) <- 'all_blues'
  total_blues <- dim(all_blues)[1]
  blue_fraction <- (total_blues / total_pixels) * 100

  #Light Blue
  light_blues <- as.data.frame(which(filtered_image == 1))
  colnames(light_blues) <- 'light_blues'
  light_blues <- dim(light_blues)[1]
  light_blue_fraction <- (light_blues / total_pixels) * 100

  #Medium Blue
  medium_blues <- as.data.frame(which(filtered_image == 2))
  colnames(medium_blues) <- 'medium_blues'
  medium_blues <- dim(medium_blues)[1]
  medium_blue_fraction <- (medium_blues / total_pixels) * 100

  #Dark Blue
  dark_blues <- as.data.frame(which(filtered_image == 3))
  colnames(dark_blues) <- 'dark_blues'
  dark_blues <- dim(dark_blues)[1]
  dark_blue_fraction <- (dark_blues / total_pixels) * 100

  result <- rbind(c("all blue [%]", blue_fraction),
                  c("light  blue [%]", light_blue_fraction),
                  c("medium blue [%]", medium_blue_fraction),
                  c("dark blue [%]", dark_blue_fraction))

  return(result)
}
