

#' transPics
#' Transitions between two photographs gradually
#' @param pic1 first photograph
#' @param pic2 second photograph
#' @param blackNwhite  a vector that specifies the grey used for the transition.  If c(0,0) returns first image, c(1,0) uses first image on left and second on right
#'
#'
#' @return a photograph
#' @export
#'
#' @examples pic1 = demo_dd("mm",pixels=600)
#' @examples pic2 = demo_dd("che",pixels=600)
#' @examples transPics(pic1,pic2,blackNwhite = c(1,1))
#' @examples transPics(pic1,pic2,blackNwhite = c(0,0))
#' @examples transPics(pic1,pic2,blackNwhite = c(1,0))
#' @examples transPics(pic1,pic2,blackNwhite = c(0,1))
transPics = function(pic1,pic2,blackNwhite=c(0,1)) {
  cwh=getPixels(pic1)
  grey=image_blank(500,500,pseudo_image =
                     paste0('gradient:grey',min(max(0,floor(blackNwhite[1]*100)),100),"-grey",
                            min(max(0,floor(blackNwhite[2]*100)),100)) )
  grey = adjustPicture(grey,bsh=c(100,100,100),pixels=cwh[1],imageRotate=90,aspect=(cwh[2]/cwh[1]),return=0)
  tmp1 = image_fx_sequence(c(pic1,grey)," (v * u )" )
  tmp2 = image_fx_sequence(c(pic2,image_negate(grey))," (v * u ) " )
  tmp3 = image_fx_sequence(c(tmp1,tmp2)," v+u" )
  return(tmp3)
}

pic1 = demo_dd("mm",pixels=600)
pic2 = demo_dd("che",pixels=600)
transPics(pic1,pic2,blackNwhite = c(1,1))
transPics(pic1,pic2,blackNwhite = c(0,0))
transPics(pic1,pic2,blackNwhite = c(1,0))
transPics(pic1,pic2,blackNwhite = c(0,1))
