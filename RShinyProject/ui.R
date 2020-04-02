
library(shiny)
library(shinyWidgets)


shinyUI(fluidPage(
    navbarPage(
        
       title="James Bond Performance",
           
        tabPanel("Movie Comparsion ",
                 
                 # Sidebar with a slider input for number of bins
                 sidebarLayout(
                     sidebarPanel(
                         h4("Filter"),
                         
                         sliderInput("year", "Year released", 1940, 2014, value = c(1970, 2014),
                                     sep = ""),
                         sliderInput("budget", "Budget (millions)", 0, 800, c(0, 800), step = 1),
                         sliderInput("boxoffice", "Dollars at Box Office (millions)",
                                     0, 800, c(0, 800), step = 1),
                         awesomeCheckbox(
                             inputId = "aw",
                             label = "Award?", 
                             value = TRUE
                         )
                        ),
                     
                     mainPanel(
                         fluidPage(fluidRow(
                             column(6,
                                    plotOutput(
                                        "plot", width = "700px", height = "600px"
                                    )),
                             column(6,
                                    DT::dataTableOutput("dataSet"),
                                    fluidRow(
                                        column(3, offset = 3,
                                               br(),
                                        ),
                                        column(3, offset = 3,
                                               img(src="007Logo.png",
                                                 height=50,width=100 )
                                        )))
                             
                         ))
                         
                     )
                 )
        ),
        tabPanel("The Bonds", 
                 sidebarLayout(
                     sidebarPanel(
                         h4("Actors"),
                         
                         selectInput(
                             "select",
                             h4("Which Era of 007?"),
                             choices = list(
                                 " "=" ",
                                 "Sean Connery" = 1,
                                 "David Niven" = 2,
                                 "George Lazenby" = 3,
                                 "Roger Moore" = 4,
                                 "Timothy Dalton" = 5,
                                 "Pierce Brosnan" = 6,
                                 "Daniel Craig" = 7
                                 
                             ),
                             selected = " "
                         )
                     ),
                     
                     mainPanel(
                         fluidPage(fluidRow(
                             column(6,
                                    plotOutput(
                                        "graph", width = "700px", height = "600px"
                                    )),
                             column(6,
                                    DT::dataTableOutput("actor"),
                                    fluidRow(
                                        column(3, offset = 3,
                                               br(),
                                        ),
                                        column(3, offset = 3,
                                               imageOutput("myImage") )
                                        )))
                             
                         ))
                         
                     )
                 ),
        tabPanel("007 Elements", fluidPage(fluidRow(
            column(6,
                   plotOutput(
                       "car", width = "700px", height = "600px"
                   )),
            column(6,
                   plotOutput(
                       "kill", width = "700px", height = "600px"
                 ))
            
        ))
    ))
)
)