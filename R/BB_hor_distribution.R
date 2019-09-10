#' Horizontal distribution of Brilliant Blue in Snow image
#'
#' A simple function that processes the result of BB_filter and returns a vector with fractions over the length of an image of a sprinkling experiment with Brilliant Blue.
#'
#' @param filtered_image the returned matrix of the function BB_filter with values 0, 1, 2 and 3
#' @param blue_selection the blue to consider. By default, the value for blue_selection is 'all'
#' \itemize{
#' \item 'all' (all blues)
#' \item 'dark blue' (only dark blue),
#' \item 'medium blue' (only medium blues) or
#' \item 'light blue' (only light blues).
#' }
#' @return Returns the vertical distribution of blue color as a vector.
#'
#' @author Johanna Bloecher, \email{bloecher@fzp.czu.cz}
#'
#' @examples
#' BB_h_dist <- BB_hor_distribution(filtered_image = threshold_data, blue_selections = 'all')
#'
#' @export

BB_hor_distribution <- function(filtered_image, blue_selection = 'all'){
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
  data_mean <- as.data.frame(rev(1:nrow(filtered_image_tmp)))
  colnames(data_mean) <- "Length"
  data_mean$Bluefraction <- rowMeans(filtered_image_tmp)
  return(data_mean)
}


