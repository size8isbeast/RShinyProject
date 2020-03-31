
library(shiny)
library(png)


shinyUI(fluidPage(
    list(tags$head(HTML('<link rel="icon", href="MyIcon.png", 
                                   type="image/png" />'))),
    div(style="padding: 0px 0px; width: '100%'",
        titlePanel(
            title="", windowTitle="My Window Title"
        )),
    
    fluidPage(
    navbarPage(
        
        title=div(img(src="007Logo.png",height=50,width=100), "James Bond Performance"),
        tabPanel("Movie Comparsion ",

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            h4("Filter"),
            
            sliderInput("year", "Year released", 1940, 2014, value = c(1970, 2014),
                        sep = ""),
            sliderInput("budget", "Budget", 1, 100, 30),
            sliderInput("boxoffice", "Dollars at Box Office (millions)",
                        0, 800, c(0, 800), step = 1),
            actionButton("button", "An action button"),
            textInput(" text","Gnere", value="Action")),
       
        mainPanel(
            fluidPage(fluidRow(
                column(6,
                       plotOutput(
                           "glm", width = "700px", height = "600px"
                       )),
                column(6,
                       # DT::dataTableOutput("dataSet"))
                       img(src = "007Logo.png", height = 80 , width = 400)
              
            ))
                         
            )
        )
    )),
    tabPanel("James Bond Actors", "This panel is intentionally left blank"),
    tabPanel("Behind the 007", "This panel is intentionally left blank")
        
    ))))
