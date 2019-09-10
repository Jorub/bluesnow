#' Vertical distribution of Brilliant Blue in Snow image
#'
#' A simple function that processes the result of BB_filter and returns a vector with fractions over the depth of an image of a sprinkling experiment with Brilliant Blue.
#'
#' @param filtered_image the returned matrix of the function BB_filter with values 0, 1, 2 and 3
#' @param blue_selection the blue to consider. By default, the value for blue_selection is 'all'
#' \itemize{
#' \item 'all' (all blues)
#' \item 'dark blue' (only dark blue),
#' \item 'medium blue' (only medium blues) or
#' \item 'light blue' (only light blues).
#' }
#'
#' @return Returns the vertical distribution of blue color as a vector.
#'
#' @author Johanna Bloecher, \email{bloecher@fzp.czu.cz}
#'
#' @examples
#' library(jpeg)
#' raw_image <- <- readJPEG(system.file("example.jpg", package = "bluesnow"))
#' ## Apply filter to raw image data
#' BB_filtered_image <- BB_filter(rgb_image = raw_image)
#' BB_v_dist <- BB_vert_distribution(filtered_image = BB_filtered_image, blue_selection = 'all')
#' plot(BB_v_dist$Depth, BB_v_dist$Bluefraction, xlab = "Depth [Pixels]", ylab = "Fraction of Blue [-]")
#' @export


BB_vert_distribution <- function(filtered_image, blue_selection = 'all'){
  filtered_image_tmp <- filtered_image
  if(blue_selection == "all"){
    filtered_image_tmp[filtered_image == 1 | filtered_image == 2 | filtered_image == 3] <- 1
  }
  if (blue_selection == "dark blue"){
    filtered_image_tmp[filtered_image == 3] <- 1
    filtered_image_tmp[filtered_image == 1 | filtered_image == 2] <- 0
  }
  if (blue_selection == "medium blue"){
    filtered_image_tmp[filtered_image == 2] <- 1
    filtered_image_tmp[filtered_image == 1 | filtered_image == 3] <- 0
  }
  if (blue_selection == "light blue"){
    filtered_image_tmp[filtered_image == 1] <- 1
    filtered_image_tmp[filtered_image == 2 | filtered_image == 3] <- 0
  }
  data_mean <- as.data.frame(rev(1:ncol(filtered_image_tmp)))
  colnames(data_mean) <- "Depth"
  data_mean$Bluefraction <- colMeans(filtered_image_tmp)
  return(data_mean)
}


