#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)


#installation 
library(ggplot2)
library(dplyr)
library(tidyverse)
library(scales)
library(jsonlite)
library(knitr)
library(kableExtra)
library(ggrepel)
library(gridExtra)
library(lubridate)
library(tidytext)
library(wordcloud)
library(recommenderlab)


library(ggvis)
library(plotly)

if (FALSE) {
    library(ggvis)
    library(dbplyr)
}


shinyServer(function(input, output,session) {
    
    # Filter the movies, returning a data frame
    movies <- reactive({
        # Due to dplyr issue #318, we need temp variables for input values
        rating <- input$rating
        minyear <- input$year[1]
        maxyear <- input$year[2]
        
        # award<-input$aw
        
        # Apply filters
        m <- all_movies %>%
            filter(
                vote_average >= rating,
                year >= minyear,
                year <= maxyear,
                
                # ,award==award
            ) 
        
        
        
        
        m <- as.data.frame(m)
        
        # Add column which says whether the movie won any Oscars
        # Be a little careful in case we have a zero-row data frame
        m$has_oscar <- character(nrow(m))
        # m$has_oscar[m$award == 0] <- "No"
        # m$has_oscar[m$award == 1] <- "Yes"
        m
    })
    
    # Function for generating tooltip text
    movie_tooltip <- function(x) {
        if (is.null(x)) return(NULL)
        if (is.null(x$id)) return(NULL)
        
        # Pick out the movie with this ID
        all_movies <- isolate(movies())
        movie <- all_movies[all_movies$id == x$id, ]
        
        paste0("<b>", movie$title, "</b><br>",
               movie$year, "<br>",
               "$", format(movie$revenue, big.mark = ",", scientific = FALSE),
               "<br>","$", format(movie$budget, big.mark = ",", 
                                  scientific = FALSE)
        )
    }
    
    # A reactive expression with the ggvis plot
    vis <- reactive({
        xvar_name <- names(axis_vars)[axis_vars == input$xvar]
        yvar_name <- names(axis_vars)[axis_vars == input$yvar]
        
        # Normally we could do something like props(x = ~BoxOffice, y = ~Reviews),
        # but since the inputs are strings, we need to do a little more work.
        xvar <- prop("x", as.symbol(input$xvar))
        yvar <- prop("y", as.symbol(input$yvar))
        
        movies %>% ggvis(x =xvar, y =yvar)  %>%
            layer_points(size := 50, size.hover := 200,
                         fillOpacity := 0.2, fillOpacity.hover := 0.5,
                          stroke = ~has_oscar,
                         key := ~id) %>%
            add_tooltip(movie_tooltip, "hover") %>%
            add_axis("x", title = xvar_name) %>%
            add_axis("y", title = yvar_name)%>%
            add_legend("stroke", title = "Won Oscar", values = c("Yes", "No")) %>%
            scale_nominal("stroke", domain = c("Yes", "No"),
                          range = c("orange", "#aaa")) %>%
            set_options(width = 500, height = 500)
    })
    
    vis %>% bind_shiny("plot")
    
 

        
        
    # output$dataSet <- DT::renderDataTable({
    #     DT::datatable(cars)
    # })
    # 
    # output$graph<- renderPlot({
    #     
    #     
    #     
    #     
    #     
    # })
    # 
    # output$actor <- DT::renderDataTable({
    #     DT::datatable(actor)
    # })
    # 
    # output$myImage <- renderImage({
    #     if (is.null(input$picture))
    #         return(NULL)
    #     
    #     if (input$picture == "face") {
    #         return(list(
    #             src = "images/face.png",
    #             contentType = "image/png",
    #             alt = "Face"
    #         ))
    #     } else if (input$picture == "chainring") {
    #         return(list(
    #             src = "images/chainring.jpg",
    #             filetype = "image/jpeg",
    #             alt = "This is a chainring"
    #         ))
    #     }
    #     
    # }, deleteFile = FALSE)
    # 
    # output$auto<- renderPlot({
    #     
    #     # generate bins based on input$bins from ui.R
    #     x    <- faithful[, 2]
    #     bins <- seq(min(x), max(x), length.out = input$bins + 1)
    #     
    #     # draw the histogram with the specified number of bins
    #     hist(x, breaks = bins, col = 'darkgray', border = 'white')
    #     
    #     
    #     
    #     
    # })
    # 
    # output$country<- renderPlot({
    #     
    #     # generate bins based on input$bins from ui.R
    #     x    <- faithful[, 2]
    #     bins <- seq(min(x), max(x), length.out = input$bins + 1)
    #     
    #     # draw the histogram with the specified number of bins
    #     hist(x, breaks = bins, col = 'darkgray', border = 'white')
    #     
    #     
    #     
    #     
    # })
    # 
    
    
    
})