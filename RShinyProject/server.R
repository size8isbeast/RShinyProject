#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$plot <- renderPlot({

        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
        
        
    

    })
    output$dataSet <- DT::renderDataTable({
        DT::datatable(cars)
    })
    
    output$graph<- renderPlot({
        
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)
        
        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
        
        
        
        
    })
    
    output$actor <- DT::renderDataTable({
        DT::datatable(actor)
    })
    
    output$myImage <- renderImage({
        if (is.null(input$picture))
            return(NULL)
        
        if (input$picture == "face") {
            return(list(
                src = "images/face.png",
                contentType = "image/png",
                alt = "Face"
            ))
        } else if (input$picture == "chainring") {
            return(list(
                src = "images/chainring.jpg",
                filetype = "image/jpeg",
                alt = "This is a chainring"
            ))
        }
        
    }, deleteFile = FALSE)

    output$car<- renderPlot({
        
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)
        
        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
        
        
        
        
    })
    
    output$kill<- renderPlot({
        
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)
        
        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
        
        
        
        
    })
    
    
    

})
