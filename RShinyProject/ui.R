
library(shiny)


shinyUI(fluidPage(
    navbarPage(
        
        "007 Movie Series Performance",
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
                       DT::dataTableOutput("dataSet"))
              
            ))
                         
            )
        )
    ),
    tabPanel("James Bond Actors", "This panel is intentionally left blank"),
    tabPanel("Behind the 007", "This panel is intentionally left blank")
        )
    ))
