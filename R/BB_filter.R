#' Filters images of brilliant blue dye tracer experiments of snow
#'
#' This function uses RGB thresholds to assign one of four colors to each pixel (white, dark blue, medium blue or light blue). Grey shadows are assigned white. The filter aims to make processing easier. It only works for images of snow.
#'
#' @param rgb_image is an array with RGB values
#'
#' @return A matrix with applied threshold where 0 is white, 1 is light blue, 2 is medium blue and 3 is dark blue
#'
#' @author Johanna Bloecher, \email{bloecher@fzp.czu.cz}
#'
#' @examples
#' ## To process raw image
#' raw_image <- readJPEG('path to image')
#' ## Apply filter to raw image data
#' BB_filtered_image <- BB_filter(rgb_image = raw_image)
#' @export

BB_filter <- function(rgb_image)
{

  if(!is.numeric(rgb_image)) stop("Provided input is not numeric")
  if(length(dim(rgb_image)) != 3) stop("Make sure your raw data comprises of the row, columns and RGB arrays")
  if (dim(rgb_image)[3] != 3) stop("Make sure your raw data is in 3 arrays each for R,G, and B values")

  if (rgb_image <= 1 && rgb_image >= 0){

  } else {
    ifelse (rgb_image <= 255 && rgb_image >= 0, rgb_image <- rgb_image/255, stop("Invalid RGB Values"))
  }

  # plot image
  rgb_image_green <- t(rgb_image[,,2])#  greem channel
  rgb_image_blue <- t(rgb_image[,,3])#  blue channel
  rgb_image_red <- t(rgb_image[,,1])#  best visual contrast is 1, red channel

  filtered_image_green <- rgb_image_green
  filtered_image_blue <- rgb_image_blue
  filtered_image_red <- rgb_image_red

  # meaningless, just for dimensions
  filtered_image <- rgb_image_red

  # Apply thresholds
  log_thresh_db <- filtered_image_red < 0.15
  log_thresh1 <- filtered_image_red < 0.3 & filtered_image_red >= 0.15 & filtered_image_blue > 0.4
  log_thresh2 <- filtered_image_red < 0.4 & filtered_image_red >= 0.3 & filtered_image_blue > 0.5

  log_thresh3 <- filtered_image_red >= 0.6 & filtered_image_red < 0.65 & filtered_image_blue > 0.85
  log_thresh4 <- filtered_image_red > 0.4 & filtered_image_red < 0.6 & filtered_image_blue > 0.7
  log_thresh5 <- filtered_image_red > 0.4 & filtered_image_red < 0.6 & filtered_image_blue > 0.6 & filtered_image_green > 0.65

  for( i in 1:nrow(log_thresh1)){
    for( j in 1:ncol(log_thresh1)){
      if(log_thresh_db[i,j]){
        filtered_image[i,j] <- 3
      }else{
        if(log_thresh1[i,j]){
          filtered_image[i,j] <- 2
        }else{
          if(log_thresh2[i,j]){
            filtered_image[i,j] <- 2
          }else{
            if(log_thresh3[i,j]){
              filtered_image[i,j] <- 1
            }else{
              if(log_thresh4[i,j]){
                filtered_image[i,j] <- 1
              }else{
                if(log_thresh5[i,j]){
                  filtered_image[i,j] <- 1
                }else{
                  filtered_image[i,j] <- 0
                }
              }
            }
          }
        }
      }
    }
  }
  return(filtered_image)
}
