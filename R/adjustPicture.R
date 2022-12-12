#mainPicDesc = imageRow[2]
testFunction = FALSE


#' Title
#'
#' @param pictureIn inphoto
#' @param pixels  pixels of output photo (long end)
#' @param imageFlip Flip image upside down
#' @param imageFlop Flop image left to right
#' @param imageRotate rotate image
#' @param bsh control bsh with 3 element vector
#' @param darkGreyIn 0 to 1 control dark grey
#' @param lightGreyIn 0 to 1 control light grey
#' @param crop four element vector first two control how much of width and hieght to use second two are how much of right  and top to  chop off
#' @param return which of the 6 possiblilities to return (0  to 6) or all off them with 7.
#' @param imageNegate take negative of imageNegate
#' @param maxPixel overrides pixels to ensure image is not two big
#' @param aspect if 0 retains the input if not distorts the image (1 image 2 landscape 0.5 is portrait pixels is the smallest side)
#' @param enlarge works with make square
#' @param offCenter works with make square
#' @param offRight works with make square
#' @param backCol1 works with make square
#' @description #c(mainPic2,step1,step2,step3,step4,lessContrast,moreContrast)
#' @description #return 0 is mainpic2,
#' @description  step1 is everything lighter than dark grey
#' @description step2 is everything darker than light grey and light than dark grey,
#' @description step3 is normalizes step 2
#' @description step4 is everything darker than light grey
#' @description step5 is low contrast
#' @description step6 is high contrast
#' is  3, 4, 5, 6 is moreContrast, 7 is the stack of all

#' @return it returns a stack of images
#' @export
#'
#' @examples pictureIn = demo_dd("thescream.jpg")
#' @examples pictureIn = image_resize(pictureIn,geometry_size_pixels(width=400,heigh=400))
#' @examples stack = adjustPicture(pictureIn,400,bsh=c(100,100,100),darkGrey = 0.49, lightGrey = 0.50,crop = c(1,1,0,0),return=7,aspect=1)
#' @examples image_append(stack)
#' @examples pictureIn = demo_dd("mm.jpg")
#' @examples adjustPicture(pictureIn,400,bsh=c(100,0,100),darkGrey = 0.4, lightGrey = 0.6,crop = c(1,1,0,0),return=7,aspect=1)
#' @examples photo = demo_dd("NegWedding.jpg")
#' @examples adjustPicture(photo, pixels = 800,bsh = c(117,0,101),crop = c(0.47,0.6,0.18,0.16), return = 0, imageNegate = TRUE)

adjustPicture <- function(pictureIn, pixels = 1000, imageFlip = FALSE, imageFlop = FALSE,
                          imageRotate = 0, bsh = c(100, 100, 100), darkGreyIn = .25, lightGreyIn = .75, crop = c(1, 1, 0, 0),
                          return = 4, imageNegate = FALSE, maxPixel = 7000, aspect = 0, enlarge = 0, offCenter = 1, offRight = 1, backCol1 = 1) {
  # mainPic2

  if (enlarge != 0) {
    pictureIn <- makeSquare(pictureIn, pad = (1 + enlarge), offCenter = offCenter, offRight = offRight, backCol = backCol1)
  }
  pixels0 <- min(pixels, maxPixel)
  cwh0 <- image_info(pictureIn)[2:3]
  cwh_new <- pixels0 * cwh0 / max(cwh0)
  mainPic2 <- image_resize(
    pictureIn,
    geometry_size_pixels(width = cwh_new[1], height = cwh_new[2], preserve_aspect = "FALSE")
  )
  cwh <- image_info(mainPic2)[2:3]
  mainPic2 <- image_crop(mainPic2, geometry_area(crop[1] * cwh[[1]], crop[2] * cwh[[2]], crop[3] * cwh[[1]], crop[4] * cwh[[2]]))

  if (aspect != 0) {
    cwh <- image_info(mainPic2)[2:3]
    mainPic2 <- image_resize(mainPic2, geometry_size_pixels(width = pixels0, height = pixels0 / aspect, preserve_aspect = "FALSE"))
  }



  if (imageFlip == TRUE) {
    mainPic2 <- image_flip(mainPic2)
  }
  if (imageFlop == TRUE) {
    mainPic2 <- image_flop(mainPic2)
  }
  if (imageNegate == TRUE) {
    mainPic2 <- image_negate(mainPic2)
  }

  mainPic2 <- image_rotate(mainPic2, imageRotate)
  mainPic2 <- image_modulate(mainPic2, brightness = bsh[1], saturation = bsh[2], hue = bsh[3])

  #########

  if (return > 0) {
    cwh1 <- as.numeric(image_info(mainPic2)[2:3])

    black <- image_blank(cwh1[1], cwh1[2], col = rgb(0, 0, 0))
    white <- image_blank(cwh1[1], cwh1[2], col = rgb(1, 1, 1))

    colorNum <- darkGreyIn * rep(1, 3)
    darkGrey <- image_blank(cwh1[1], cwh1[2], col = rgb(colorNum[1], colorNum[2], colorNum[3]))

    colorNum <- lightGreyIn * rep(1, 3)
    lightGrey <- image_blank(cwh1[1], cwh1[2], col = rgb(colorNum[1], colorNum[2], colorNum[3]))

    step1 <- image_composite(mainPic2, darkGrey, operator = "lighten") # everything lighter than dark grey
    step2 <- image_composite(step1, lightGrey, operator = "darken") # everything lighter than dark grey and darker than light grey
    step3 <- image_normalize(step2) # light Grrey becomes white dark grey becomes black

    step4 <- image_composite(mainPic2, lightGrey, operator = "darken")
    lowContrast <- image_composite(step4, step2, operator = "lighten")
    highContrast <- image_normalize(lowContrast)

    lessContrast <- image_composite(mainPic2, lowContrast, operator = "multiply")
    moreContrast <- image_composite(mainPic2, highContrast, operator = "multiply")
  }

  if (return == 0) {
    return(mainPic2)
  }
  if (return == 1) {
    return(step1)
  }
  if (return == 2) {
    return(step2)
  }
  if (return == 3) {
    return(step3)
  }
  if (return == 4) {
    return(step4)
  }
  if (return == 5) {
    return(lessContrast)
  }
  if (return == 6) {
    return(moreContrast)
  }
  if (return == 7) {
    imageStack <- c(mainPic2, step1, step2, step3, step4, lessContrast, moreContrast)
    return(image_append(imageStack))
  }
}
