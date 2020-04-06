#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

library(DT)



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
               "Revenue (million $)",
               format(movie$revenue, big.mark = ",", scientific = FALSE),
               "<br>", 
               "Budget (million $)",
               format(movie$budget, big.mark = ",", scientific = FALSE)
            
        )
    }
    
    # A reactive expression with the ggvis plot
    vis <- reactive({
        xvar_name <- names(axis_vars)[axis_vars == input$xvar]
       yvar_name <- names(axis_vars)[axis_vars == input$yvar]
        # 
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
            add_axis("x", title = xvar_name,title_offset = 50,
                     properties = axis_props(
                       title = list(fontSize = 16,fill = "white"),
              labels = list(fontSize = 12, fill = "white",angle=45))) %>% 
            add_axis("y", title = yvar_name,title_offset = 50,
                     properties = axis_props(
                       title = list(fontSize = 16,
                                    fill = "white"),
                labels = list(fontSize = 12, fill = "white"))) %>% 
            add_legend("stroke", title = "Has Oscar", values = c("Yes", "No"), 
                       properties = legend_props(
              title = list(fontSize = 16,fill = "white"),
              labels = list(fontSize = 12, fill = "white")
            )) %>%
            scale_nominal("stroke", domain = c("Yes", "No"),
                          range = c("#DAA520", "#aaa")) %>%
            set_options(width = "700px", height = "400px")
    })
    
    vis %>% bind_shiny("plot")
    
    
       
    output$dataSet <- DT::renderDataTable({
      dt<-all_movies %>% 
        filter(
          vote_average>= input$rating,year >= input$year[1]&year <= input$year[2],
          budget>= input$budget[1]& budget <= input$budget[2],
          revenue >= input$revenue[1]&revenue <= input$revenue[2]
      
        ) 
      dt<-dt[,c('title','year')]
        DT::datatable(dt) %>%
        formatStyle(names(dt),  
                    color = 'white', backgroundColor = 'black', fontWeight = 'bold')
  
    })
    
    
    df<-reactive({
      tab2_bonds %>% select(input$checkbox)
    })
    
    
    output$selected_num<-renderUI({
      if(length(df())<3){
        h6(helpText(code("Please choose at least 3 Actors"),style="font-family: 'times'; font-si16pt"))
        
      }
    })
    
    
    output$radarplot<-renderPlot({
      coul <- brewer.pal(10, "RdBu")
      colors_in <- alpha(coul,0.3)
      
      
      colors_border <- coul
      par(bg = "black")
      radarchart( df(), axistype=0 , maxmin=F,
                  #custom polygon
                  pcol=colors_border , pfcol=colors_in,plwd=3, plty=2,
                  #custom the grid
                  cglcol="white", cglty=1, axislabcol="white", cglwd=0.8,
                  
                  #custom labels
                  vlcex=1) 
      
      legend(1.1, 1.1, legend = rownames(tab2_bonds), 
             col = colors_border, text.col = "white",
             seg.len = 2, border = "transparent",
             pch = 16, lty = 1)
      
     
      
      
    })
    
    output$auto<- renderPlot({
      
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2]
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
      
      
      
      
    })
    
    output$country<- renderPlot({
      
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2]
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
      
      
      
      
    })
    
    
    
    
})
