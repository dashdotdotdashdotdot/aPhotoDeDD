#' demo_dd
#'
#' @param path of an image to load
#'
#' @return an image
#' @export
#'
#' @examples demo_dd("dove.jpg")
demo_dd <- function(path="dove.jpg") {
   magick::image_read(system.file("", path, package = "aPhotoDeDD"))
  #  image_read(paste0(".//inst//images//",path))
  #magick::image_read(path)
}

#demo_dd(path="~/dwdRstuff/testPackages/take11/inst/dove.jpg")
