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

source("helper.R")

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
    

    observe({
      
      updateCheckboxGroupInput(
        session, 'checkbox', choices =list(
          
          'Daniel Craig (2006-2020)'='Daniel.Craig',
          'Pierce Brosnan (1995-2002)'='Pierce.Brosnan',
          'Timothy Dalton (1987-1989)'='Timothy.Dalton',
          'Roger Moore (1973-1985)'='Roger.Moore',
          'Sean Connery (1962-1971)'='Sean.Connery',
          'George Lazenby (1969)'= 'George.Lazenby'
          
        ),
        selected = if(input$all) c('Daniel.Craig',
                                   'George.Lazenby',
                                   'Pierce.Brosnan',
                                   'Roger.Moore',
                                   'Sean.Connery',
                                   'Timothy.Dalton')
      )
    }
    
    )
    
    df<-reactive({
      tab2_bonds %>% select(input$checkbox)
    })
    
    
    output$selected_num<-renderUI({
      if(length(df())<3){
        h6(helpText(code("Please choose at least 3 Actors"),style="font-family: 'times'; font-si16pt"))
        
      }
    })
    
    output$bonds<-renderImage({
      par(bg = "black")
      if(length(df())==2|length(df())==0){
        return(list(
          src = "www/combine.png",
          contentType = "image/png",
          width=600,
          heigt=600
          
        ))
      }else if(length(df())==1&input$checkbox=='Daniel.Craig'){
        
        list(src = "www/Daniel.png",
             contentType = "image/png",
             width=200,
             heigt=600)
      }
      else if(length(df())==1&input$checkbox=='Pierce.Brosnan'){
        
        list(src = "www/Pierce.png",
             contentType = "image/png",
             width=100,
             heigt=600)
      }else if(length(df())==1&input$checkbox=='Timothy.Dalton'){
        
        list(src = "www/Timothy.png",
             contentType = "image/png",
             width=150,
             heigt=700)
      }else if(length(df())==1&input$checkbox=='Roger.Moore'){
        
        list(src = "www/Roger.png",
             contentType = "image/png",
             width=200,
             heigt=600)
      }else if(length(df())==1& input$checkbox=='Sean.Connery'){
        
        list(src = "www/Sean.png",
             contentType = "image/png",
             width=100,
             heigt=500)
      }else if(length(df())==1&input$checkbox=='George.Lazenby'){
        
        list(src = "www/George.png",
             contentType = "image/png",
             width=100,
             heigt=600)
      } else if(length(df())>2){
        outfile <- tempfile(fileext='.png')
        list(
          src = outfile
        )
      }
    },deleteFile = FALSE)
    
    
    output$radarplot<-renderPlot({
      if(length(df())<3){
        
        ggplot()+
          theme(
            panel.grid = element_line(color = "#000000"),
            panel.background = element_rect(fill = "#000000", color = "#000000"),
            plot.background = element_rect(fill = "#000000", color = "#000000"))
      }else{
        
        coul <- brewer.pal(8, "RdBu")
        colors_in <- alpha(coul,0.3)
        
        
        colors_border <- coul
        par(bg = "black",col="white")
        radarchart( df(), axistype=0 , maxmin=F,
                    
                    #custom polygon
                    pcol=colors_border , pfcol=colors_in,plwd=3, plty=c(1,3,4),
                    #custom the grid
                    cglcol="white", cglty=1, axislabcol="white", cglwd=0.8,
                    
                    #custom labels
                    vlcex=1) 
        
        legend(1.1, 1.1, legend = rownames(tab2_bonds), 
               col = colors_border, text.col = "white",
               seg.len = 2, border = "transparent",
               pch = 16, lty = 1)
        
      }
      
      
    })
    
    # For the automobile part 
    output$Car <- renderD3tree2({
      
      d3tree2(p, width = "80%",valueField = "size", height ="600px", 
              rootname = "Favorite Brands")
      
      }) 
    
    # For the country part 
    output$country <- renderLeaflet({
      
      # generate bins based on input$bins from ui.R
      # ggplot()+
      #   geom_map(data=country_data,  map= country_data, 
      #            aes(x=long, y=lat, group=group, map_id=region), 
      #            fill="white", colour="#7f7f7f")+
      #   geom_map(data=country_map, map=country_data, 
      #            aes(fill=Freq, map_id=region), colour="#7f7f7f")+
      #   scale_fill_continuous(low="thistle2", high="darkred", guide="colorbar")+
      #   theme_bw()
      
      # mapState <- map("world", fill=TRUE, plot=FALSE)
      country_map %>% dataframeToPolygons("lat", "long", "group", freq) %>%
        leaflet() %>%
        addTiles(options=tileOptions(minZoom = 1, maxZoom = 8)) %>%
        leaflet::addPolygons(
          weight = 1,
          opacity = 1, 
          fillColor = ~pal2(freq),
          color = "black",
          dashArray = "3",
          fillOpacity = 0.7,
          highlight = highlightOptions(
            weight = 1,
            color = "#666",
            dashArray = "",
            fillOpacity = 0.7,
            bringToFront = TRUE),
          label = region,
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px",
            direction = "auto")
        ) %>%
        addLegend(pal = pal2, values = freq, opacity = 0.7, title = NULL,
                  position = "topleft")
    })
    v <- reactiveValues()
    
    observeEvent(input$country_shape_click, { 
      p <- input$country_shape_click
      country_name <- map.where(database="world", p$lng, p$lat)
      country_name <- str_split(country_name, ":")[[1]][1]
      v$country <- country_name
    })
    
    output$testoutput <- DT::renderDataTable({
      film <- unique(dat[dat$country == v$country, ])
      DT::datatable(film)%>%
        formatStyle(names(film),  
                    color = 'white', backgroundColor = 'black', fontWeight = 'bold')
    })
    
    
})
