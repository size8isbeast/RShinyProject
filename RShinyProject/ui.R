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
                         ),fluidRow(DT::dataTableOutput("dataSet") ))
                         
                     )
                 )
        ),
        tabPanel("The Bonds", 
                 sidebarLayout(
                     sidebarPanel(
                         
                         
                         checkboxGroupInput(
                             inputId = "checkbox",
                             h4("Actors"),
                             choices = list(
                                 
                                 'Daniel Craig'='Daniel.Craig',
                                 'George Lazenby'= 'George.Lazenby',
                                 'Pierce Brosnan'='Pierce.Brosnan',
                                 'Roger Moore'='Roger.Moore',
                                 'Sean Connery'='Sean.Connery',
                                 'Timothy Dalton'='Timothy.Dalton'
                             ),
                             selected =  c('Daniel.Craig',
                                           'George.Lazenby',
                                           'Pierce.Brosnan',
                                           'Roger.Moore',
                                           'Sean.Connery',
                                           'Timothy.Dalton')
                         ),
                         uiOutput("selected_num")
                     ),
                     
                     mainPanel(
                         fluidPage(fluidRow(
                             
                             plotOutput(
                                 "radarplot", width = "700px", height = "600px"
                             ))
                         ))
                 )),
        
        navbarMenu("007 Elements",
                   tabPanel("Automobile",
                            fluidPage(fluidRow(
                                column(10,
                                       d3tree2Output("Car",width = "100%", height = "800px")
                                       ),
                                column(2,
                                       strong("Reminder: click the top bar to return", style = "color: white"
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