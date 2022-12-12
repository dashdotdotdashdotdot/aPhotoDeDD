library(shiny)

ui2 = fluidPage(
  titlePanel('Adjust Picture'),
  sidebarLayout(
    sidebarPanel(
      selectInput('useWhich_photo', label = 'Return Which Photo',
                  choices = list('As is' = 1, 'gt Dark Grey' = 2, 'Dark to Light' = 3,'Normalize' = 4,
                                 'lt Light Grey' = 5, 'Low Contrast' = 6, 'High Contrast' = 7, 'All'= 8),
                  selected = 1),
      fluidRow(
        column(3,checkboxInput('imageFlip', label = 'Flip', value = FALSE)),
        column(3,checkboxInput('imageFlop', label = 'Flop', value = FALSE)),
        column(6,checkboxInput('imageNegate', label = 'Negate', value = FALSE)),),
      fluidRow(

        column(6, numericInput('pixels', label = 'Pixels', value = 200)),
        column(6,numericInput('imageRotate', label = 'ImageRotate', value = 0)),),
      sliderInput('black_n_white_photo', label = 'Dark and Light Points', min = 0,
                  max = 1, value = c(.25, .75)),
      sliderInput('bPhoto', 'Photo Brightness :', min = 0, max = 300,
                  value = 116, step = 1),
      sliderInput('sPhoto', 'Photo Saturation :', min = 0, max = 200,
                  value = 100, step = 1),
      sliderInput('hPhoto', 'Photo Hue :', min = 0, max = 200,
                  value = 100, step = 1),
      sliderInput('cutHorizontal', label = 'Cut Horizontal', min = 0,
                  max = 1, value = c(.0, 1)),
      sliderInput('cutVertical', label = 'Cut Vertical', min = 0,
                  max = 1, value = c(0, 1)),
      fluidRow(
        column(6,numericInput('Aspect', label = ('Aspect'), value = 0)),
        column(6,numericInput('Enlarge', label = ('Enlarge'), value = 0))),

      fluidRow(
        column(4,numericInput('offCenter', label = ('offCenter'), value = 0)),
        column(4,numericInput('offRight', label = ('offRight'), value = 0)),
        column(4,numericInput('Grey', label = ('Grey'), value = 0)))


    ),  #ends sidebarPanel
    mainPanel(
      tabsetPanel( tabPanel( 'Plot1', imageOutput('outPhoto', height = 1)) ,
                   tabPanel('Plot 2', plotOutput('plot2')) ,
                   tabPanel('Table 1', DT::dataTableOutput('table1')),
                   tabPanel('Meta Data', verbatimTextOutput('metaData'))
      )
    ) #ends mainPanel
  ) #ends sidebarlayout
) #end fluidPage



#' Title
#'
#' @param pictureIn an input photo
#'
#' @return launch an  app that adjusts the picture
#' @export
#'
#' @examples dove = demo_DD()
#' @examples adjPicApp(dove)

adjPicApp <- function(pictureIn) {
  photoName <- substitute(pictureIn)
  shinyApp(
    ui = ui2,
    server = function(input, output) {
      library(magick)
      library(ArtMathdeDD)
      backColor <- reactive(rgb(input$Grey, input$Grey, input$Grey))
      toUse <- reactive(as.numeric(input$useWhich_photo) - 1)
      crop1 <- reactive(c(
        input$cutHorizontal[2] - input$cutHorizontal[1], input$cutVertical[2] - input$cutVertical[1],
        input$cutHorizontal[1], input$cutVertical[1]
      ))
      # photoName = substitute(pic0)
      # photoName =reactive(renderText({pictureIn}))
      output$outPhoto <- renderImage(
        {
          outPhoto <- adjustPicture(
            pictureIn,
            pixels = input$pixels,
            imageFlip = input$imageFlip,
            imageFlop = input$imageFlop,
            imageRotate = input$imageRotate,
            bsh = c(input$bPhoto, input$sPhoto, input$hPhoto),
            darkGreyIn = input$black_n_white_photo[[1]],
            lightGreyIn = input$black_n_white_photo[[2]],
            crop = crop1(),
            return = toUse(),
            imageNegate = input$imageNegate,
            maxPixel = 7000,
            aspect = input$Aspect,
            enlarge = input$Enlarge,
            offCenter = input$offCenter,
            offRight = input$offRight,
            backCol1 = backColor()
          ) # ends adjustPicture
          outfile <- tempfile(fileext = ".png")
          image_write(outPhoto, outfile)

          return(list(
            src = outfile,
            filetype = "image/jpeg",
            alt = "mainPicDesc"
          ))
        },
        deleteFile = FALSE
      )

      output$metaData <- renderPrint({
        h4(
          paste0("adjustPicture(", photoName, ","),
          paste0("pixels = ", input$pixels, ","),
          paste0("imageFlip = ", input$imageFlip, ","),
          paste0("imageFlop = ", input$imageFlop, ","),
          paste0("imageRotate =", input$imageRotate, ","),
          paste0("bsh = c(", input$bPhoto, ",", input$sPhoto, ",", input$hPhoto, "),"),
          paste0("darkGreyIn = ", input$black_n_white_photo[[1]], ","),
          paste0("lightGreyIn = ", input$black_n_white_photo[[2]], ","),
          paste0("crop = c(", crop1()[1], ",", crop1()[2], ",", crop1()[3], ",", crop1()[4], "),"),
          paste0("return = ", toUse(), ","),
          paste0("imageNegate = ", input$imageNegate, ","),
          paste0("maxPixel = 7000,"),
          paste0("aspect = ", input$Aspect, ","),
          paste0("enlarge = ", input$Enlarge, ","),
          paste0("offCenter = ", input$offCenter, ","),
          paste0("offRight = ", input$offRight, ","),
          paste0(   "backCol1 = '",backColor(),"')") # ends adjustPicture'
        )
      })
    } # ends server
  )
} # ends shinyApp

