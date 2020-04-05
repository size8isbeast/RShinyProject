library(ggvis)
library(shiny)
library(shinyWidgets)
#install.packages("shinythemes")
library(shinythemes)
library(DT)




shinyUI(fluidPage(theme=shinytheme("cyborg"),tagList(
    tags$head(tags$script(type="text/javascript", src ="code.js"),
              
    )),
    navbarPage(
       title="James Bond Performance",
           
        tabPanel("Movie Comparsion ",
                 
                 # Sidebar with a slider input for number of bins
                 sidebarLayout(
                     sidebarPanel(width=3,
                         h4("Filter"),
                         sliderInput("rating", "Movie average Rating",
                                     5.7, 10, 5.7, step = 0.1),
                         
                         sliderInput("year", "Year released", 1962, 2015, value = c(1962, 2015),
                                     sep = ""),
                         sliderInput("budget", "Budget (millions)", 0, 300, c(0, 300)),
                         sliderInput("revenue", "Revenue (millions)",
                                     0, 1200, c(0, 1200)),
                         wellPanel(
                             selectInput("xvar", "X-axis variable", axis_vars, selected = "budget"),
                             selectInput("yvar", "Y-axis variable", axis_vars, selected = "revenue"),
                          
                         )
                        ),
                     
                     mainPanel(
                         fluidPage(fluidRow(
                         
                                    ggvisOutput(
                                        "plot")
                             
                             
                         ),fluidRow(DT::dataTableOutput("dataSet")))
                         
                     )
                 )
        ),
        tabPanel("The Bonds", 
                 sidebarLayout(
                     sidebarPanel(
                     
            
                         checkboxGroupInput(
                             inputId = "checkGroup",
                             h4("Actors"),
                             choices = list(
                              
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
       navbarMenu("007 Elements",
                  tabPanel("Automobile",
                           fluidPage(fluidRow(
            column(6,
                   plotOutput(
                       "auto", width = "700px", height = "600px"
                   )),
            column(6,
                   p("This is text for automobile"
                       
                 ))
            
        ))
    ),tabPanel("Country",
               fluidPage(fluidRow(
                   column(6,
                          plotOutput(
                              "country", width = "700px", height = "600px"
                          )),
                   column(6,
                          p("This is text for country"
                            
                          ))
    )
)
))
)
))





