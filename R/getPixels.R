#' Get Pixels
#'
#' @param pictureIn a photograph
#'
#' @return a 2 element numeric vector of the width and hieght of a photo in pixels
#' @export
#'
#' @examples pictureIn = demo_dd()
#' @examples cwh= getPixels(pictureIn)
#' @examples cwh  #cwh is canvas width and height
getPixels = function(pictureIn){return(as.numeric(image_info(pictureIn)[2:3]))}
