# aPhotoDeDD

This repo will contain some basic tools for photo manipulation.

If you run this:

remotes::install_github("dashdotdotdashdotdot/aPhotoDeDD")
library(aPhotoDeDD)
che <- demo_dd(path = "che.JPG")

adjPicApp(che)


in Rstudio a shiny app should pop up and if open it in a browser you sohuld see the iconic image of Che Guevra in 200 pixels.  The app has a bunch of dials that allows you to edit the photo. The last tab of the app contians the code that produces the current version of the image -- the one you see in the app.  So if you cut and past this into an R program you can capture the image you created which allows you to work with it programatically.

library(magick)
library(shiny)
pictureIn = image_read("~//dwdRstuff//imageFolders//cylinderChess//Photos//profile.jpg")
adjPicApp(pictureIn)

remotes::install_github("dashdotdotdashdotdot/take12")
library(take11)
 


usethis::create_from_github(
  "https://github.com/dashdotdotdashdotdot/aPhotoDeDD.git",
  destdir = "~/dwdRstuff/testPackages/aPhotoDeDD"
)


che2= adjustPicture(che,
                    pixels = 200,
                    imageFlip = FALSE,
                    imageFlop = FALSE,
                    imageRotate =0,
                    bsh = c(116,100,100),
                    darkGreyIn = 0.25,
                    lightGreyIn = 0.75,
                    crop = c(1,1,0,0),
                    return = 0,
                    imageNegate = FALSE,
                    maxPixel = 7000,
                    aspect = 0,
                    enlarge = 0,
                    offCenter = 0,
                    offRight = 0,
                    backCol1 = '#000000')

image_append(c(che2,che))

