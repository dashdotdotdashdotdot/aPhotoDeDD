#' demo_dd
#'
#' @param path of an image to load
#'
#' @return an image
#' @export
#'
#' @examples demo_dd("dove.jpg")
#' @examples demo_dd("mm",pixels=400)
demo_dd <- function(path0="dove.jpg",pixels=0) {
  if (path0 %in% c("dove","3wins","thescream","mm","che","bogie","monalisa") ){
    path = paste0(path0,".jpg") } else path = path0

 tmp=   magick::image_read(system.file("", path, package = "aPhotoDeDD"))
 if (pixels != 0){
   tmp = magick::image_resize(tmp,magick::geometry_size_pixels(width=pixels,height =pixels))
 }

 return(tmp)
  #  image_read(paste0(".//inst//images//",path))
  #magick::image_read(path)
}

