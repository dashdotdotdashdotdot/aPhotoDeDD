#' Title
#'
#' @param photo photograph object to input
#' @param rows number of rows in the tessalation
#' @param columns number of columns in the tessalation
#' @param decay rate at which the tesslation decays
#' @param center center of tessalation
#' @param option does a variety of options
#'
#' @return a photograph
#' @export
#'
#' @examples #EXAMPLE 1
#' @examples photo = demo_dd("3wins",pixels=300)
#' @examples  makeTessalation(photo,rows=3,columns=3,decay = -2, center=c(2,2),option=1)
#' @examples #EXAMPLE 1
#' @examples photo = demo_dd("mm",pixels=300)
#' @examples makeTessalation(photo,rows=1,columns=5,decay = -2, center=c(3,1),option=1)
#' @examples #EXAMPLE 2

#' @examples photo = demo_dd("thescream",pixels=300)
#' @examples makeTessalation(photo,rows=5,columns=3,decay = -2, center=c(2,3),option=1)
#' @examples #EXAMPLE 3

#' @examples photo = image_flip(demo_dd("monalisa",pixels=400))
#' @examples makeTessalation(photo,rows=7,columns=7,decay = -2, center=c(4,4),option=1)
#' @examples #EXAMPLE 4

#' @examples photo = image_flip(demo_dd("monalisa",pixels=200))
#' @examples photo2 = makeTessalation(photo,rows=7,columns=7,decay = 1, center=c(4,4),option=1)
#' @examples adjustPicture(photo2,pixels=600)



makeTessalation = function(photo,rows=2,columns=2,decay=0,center=c(1,1),option=1){
  wh0 = image_info(photo)[2:3]
  decayFactor = 2^decay
  if ( ( (rows == 0) * (columns == 0) ) ==TRUE) { newPhoto = photo } else {
    flip = c(0,0)
    flop = c(0,1)
    flipRow = c(0,1)
    offSet0 = 1
    negative = c(0,0)
    negativeRow = c(0,0)

    if (option == 0)    {
      flop = c(0,0)
      flipRow = c(0,0)}

    counter = 0
    for (j in 1:rows) {
      for (i in 1:columns){
        photo0 = magick::image_resize(photo,
                                magick::geometry_size_pixels(
                                width=max(decayFactor^abs(center[1]-i)*wh0[1],1),
                                height=(1^abs(center[2]-j))*wh0[2]
                                ,preserve_aspect = FALSE)
        )

        cell =   (i+j-2 + counter) %% length(flop) + 1
        print(cell)
        if (1 == flop[cell]) {photo0 = image_flop(photo0) }
        if (1 == negative[cell]) {photo0 = image_negate(photo0) }
        if (1 == flip[cell]) {photo0 = image_flip(photo0) }

        if (i ==1) {newRow = photo0} else newRow = image_append(c(newRow,photo0))
      }

      widthPixels = image_info(newRow)[[2]]
      heightPixels = image_info(newRow)[[3]]
      if (offSet0 == 1) {counter=counter+1}
      newRow = magickk::image_resize(newRow,
                              magick::geometry_size_pixels(
                              width=max(1^abs(center[1]-i)*widthPixels,1),
                              height=max(decayFactor^abs(center[2]-j)*heightPixels,1)
                              ,preserve_aspect = FALSE)
      )

      row = (j -1) %% length(flipRow) + 1
      if (flipRow[row] == 1) {newRow =image_flip(newRow)}
      if (negativeRow[row] == 1) {newRow =image_negate(newRow)}

      if (j ==1) {newPhoto = newRow} else newPhoto = image_append(c(newPhoto,newRow),stack=TRUE)
    }

  }
  return(newPhoto)}

