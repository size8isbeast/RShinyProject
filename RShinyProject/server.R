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

#read csvs
movies <- read_csv("www/data/tmdb_5000_movies.csv", na="NA")
credits <- read_csv("www/data/tmdb_5000_credits.csv",  na="NA")
oscars <- read_csv("www/data/oscar.csv",  na="NA")
Coolcars <- read_csv("www/data/cars.csv")
Kills <- read_csv("www/data/Kills per Bond movie.csv",  na="NA")


S007_movie <- movies %>%filter(id %in% c(710,714,707,658,
                                         657,646,699,709,
                                         253,660,36643,668,
                                         691,698,682,681,708,
                                         12208,36669,667,700,
                                         36670,36557,10764,37724,
                                         206647)) 

S007_credit <- credits %>%filter(movie_id %in% c(710,714,707,658,
                                                 657,646,699,709,
                                                 253,660,36643,668,
                                                 691,698,682,681,
                                                 708,12208,36669,667,
                                                 700,36670,36557,10764,37724,206647)) 

#filtering cast
cast <- S007_credit %>%
    filter(nchar(cast)>2) %>%
    mutate(js = lapply(cast, fromJSON)) %>%
    unnest(js) %>%
    select(-cast, -crew, -credit_id) %>%
    rename(actor=name, movie_cast_id=cast_id, actor_id=id) %>%
    mutate_if(is.character, factor) %>%
    filter(order == 0)

#data cleaning-graph1
#join award and movie
S007_Year <-S007_movie %>% mutate(year=year(release_date))
award<- unique(oscars[,c(1,4)]) 
award<-award %>% rename(title=Film,award=winner)
S007_Year_Award<-merge(S007_Year,award, all = TRUE)
S007_Year_Award<-S007_Year_Award[complete.cases(S007_Year_Award[ , 2:21]),]
S007_Year_Award$award[is.na(S007_Year_Award$award)]<-0
mo<-S007_Year_Award %>% select(title,id,popularity,budget,revenue,vote_average,
                             year,award)
all_movies<-S007_Year_Award %>% select(title,id,popularity,budget,revenue,vote_average,
                                       year,award)


#theme_black()
##Theme

# theme_black = function(base_size = 12, base_family = "") {
#     
#     theme_grey(base_size = base_size, base_family = base_family) %+replace%
#         
#         theme(
#             # Specify axis options
#             axis.line = element_blank(),  
#             axis.text.x = element_text(size = base_size*0.8, color = "white", lineheight = 0.9),  
#             axis.text.y = element_text(size = base_size*0.8, color = "white", lineheight = 0.9),  
#             axis.ticks = element_line(color = "white", size  =  0.2),  
#             axis.title.x = element_text(size = base_size, color = "white", margin = margin(0, 10, 0, 0)),  
#             axis.title.y = element_text(size = base_size, color = "white", angle = 90, margin = margin(0, 10, 0, 0)),  
#             axis.ticks.length = unit(0.3, "lines"),   
#             # Specify legend options
#             legend.background = element_rect(color = NA, fill = NA),  
#             legend.key = element_rect(color = "white",  fill = "black"),  
#             legend.key.size = unit(1.2, "lines"),  
#             legend.key.height = NULL,  
#             legend.key.width = NULL,      
#             legend.text = element_text(size = base_size*0.8, color = "white"),  
#             legend.title = element_text(size = base_size*0.8, face = "bold", hjust = 0, color = "white"),  
#             legend.position = "right",  
#             legend.text.align = NULL,  
#             legend.title.align = NULL,  
#             legend.direction = "vertical",  
#             legend.box = NULL, 
#             # Specify panel options
#             panel.background = element_rect(fill = "black", color  =  NA),  
#             panel.border = element_rect(fill = NA, color = "white"),  
#             panel.grid.major = element_line(color = "grey35"),  
#             panel.grid.minor = element_line(color = "grey20"),  
#             panel.margin = unit(0.5, "lines"),   
#             # Specify facetting options
#             strip.background = element_rect(fill = "grey30", color = "grey10"),  
#             strip.text.x = element_text(size = base_size*0.8, color = "white"),  
#             strip.text.y = element_text(size = base_size*0.8, color = "white",angle = -90),  
#             # Specify plot options
#             plot.background = element_rect(color = "black", fill = "black"),  
#             plot.title = element_text(size = base_size*1.2, color = "white"),  
#             plot.margin = unit(rep(1, 4), "lines")
#             
#         )
#     
# }
# 





shinyServer(function(input, output,session) {
    
    # Filter the movies, returning a data frame
    movies <- reactive({
        # Due to dplyr issue #318, we need temp variables for input values
        rating <- input$rating
        minyear <- input$year[1]
        maxyear <- input$year[2]
        minbudget<-input$budget[1] 
        maxbudget<-input$budget[2]
        minrevenue <- input$revenue[1] 
        maxrevenue <- input$revenue[2] 
        #award<-input$aw
        
        # Apply filters
        m <- all_movies %>%
            filter(
                vote_average >= rating,
                year >= minyear,
                year <= maxyear,
                budget>= minbudget,
                budget <= maxbudget,
                revenue >= minrevenue,
                revenue <= maxrevenue
                #, award==award
            ) 
        
        
        
        
        m <- as.data.frame(m)
        
        # Add column which says whether the movie won any Oscars
        # Be a little careful in case we have a zero-row data frame
        m$has_oscar <- character(nrow(m))
        m$has_oscar[m$award == 0] <- "No"
        m$has_oscar[m$award == 1] <- "Yes"
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
               "$", format(movie$revenue, big.mark = ",", scientific = FALSE)
        )
    }
    
    # A reactive expression with the ggvis plot
    vis <- reactive({
        
        
        # Normally we could do something like props(x = ~BoxOffice, y = ~Reviews),
        # but since the inputs are strings, we need to do a little more work.
        b <- prop("x", as.symbol(input$budget))
        r <- prop("y", as.symbol(input$revenue))
        
        movies %>%
            ggvis(x = , y = r) %>%
            layer_points(size := 50, size.hover := 200,
                         fillOpacity := 0.2, fillOpacity.hover := 0.5,
                         stroke = ~has_oscar, key := ~id) %>%
            add_tooltip(movie_tooltip, "hover") %>%
            add_axis("x", title = budget) %>%
            add_axis("y", title = revenue) %>%
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