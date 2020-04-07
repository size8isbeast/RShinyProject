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
                         
                         style = "font-size: 12pt; line-height: 30pt; width = 100",
                         checkboxGroupInput(
                             inputId = "checkbox",
                             h4("Actors"),
                             choices = list(
                    
                                 
                                 'Daniel Craig (2006-2020)'='Daniel.Craig',
                                 'Pierce Brosnan (1995-2002)'='Pierce.Brosnan',
                                 'Timothy Dalton (1987-1989)'='Timothy.Dalton',
                                 'Roger Moore (1973-1985)'='Roger.Moore',
                                 'Sean Connery (1962-1971)'='Sean.Connery',
                                 'George Lazenby (1969)'= 'George.Lazenby'
                             ),
                             selected =  c('Daniel.Craig',
                                           'George.Lazenby',
                                           'Pierce.Brosnan',
                                           'Roger.Moore',
                                           'Sean.Connery',
                                           'Timothy.Dalton')
                         ),
                         checkboxInput("all","Select All",value = TRUE),
                         uiOutput("selected_num")
                     ),
                     
                     mainPanel(
                         style="position:fixed;margin-left:32vw;",
                         fluidPage(fluidRow(
                             
                             column(1,
                                    plotOutput(
                                        "radarplot", width = "700px", height = "600px"
                                    )),
                             column(11,
                                    imageOutput("bonds")))
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
                              fluidPage(
                                  fluidRow(
                                      column(
                                          width = 4, 
                                          DT::dataTableOutput("testoutput")
                                      ), 
                                      column(
                                          width = 8, 
                                          leafletOutput("country", width = "1200px", height = "1000px")
                                      )
                                  )
                              )
                   )
                   )
    )
))