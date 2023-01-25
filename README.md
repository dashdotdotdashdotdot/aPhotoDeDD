# aPhotoDeDD

This repo will contain some basic tools for photo manipulation.

If you run this:

remotes::install_github("dashdotdotdashdotdot/aPhotoDeDD") 

library(aPhotoDeDD)

thescream <- demo_dd(path = "thescream.JPG")

adjPicApp(thescream)

in Rstudio a shiny app should pop up and if open it in a browser you sohuld see the iconic image of the scream in 200 pixels.  The app has a bunch of dials that allows you to edit the photo. The last tab of the app contians the code that produces the current version of the image -- the one you see in the app.  So if you cut and past this into an R program you can capture the image you created which allows you to work with it programatically.

