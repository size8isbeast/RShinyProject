#ui
navbarMenu("007 Elements",
           tabPanel("Car",
                    fluidPage(fluidRow(
                      titlePanel("007 Favorite Car Brand"),
                      mainPanel(
                        d3tree2Output("Car",width = "100%", height = "800px")
                      )
                    ))))
#server
output$Car <- renderD3tree2({d3tree(p, width = "80%",valueField = "size", height ="600px", rootname = "Favorite Brands")})

# library
library(treemap)
library(devtools)
install.packages("devtools")
install.packages("viridisLite")
devtools::install_github("timelyportfolio/d3treeR")
library(d3treeR)
library(htmlwidgets)

#data prepare

Coolcars$Brand <- gsub("([A-Za-z]+).*", "\\1", Coolcars$Vehicle)
test_car <- Coolcars %>% group_by(Vehicle) %>% tally()
view(test_car)

Car <- left_join(Coolcars, test_car, by = c("Vehicle", "Vehicle"))
view(Car)




treemap(Car,
        index=c("Brand","Vehicle"),
        vSize = "n",
        type="value"
) 




# basic treemap
p <- treemap(Car,
             index=c("Brand","Vehicle"),
             vSize="n",
             type="value",
             palette = "Set2",
             bg.labels=c("white"),
             align.labels=list(
               c("center", "center"), 
               c("right", "bottom")
               ),
            position.legend = "none",
             lowerbound.cex.labels = 0.4,
             inflate.labels = TRUE
)            

# make it interactive ("rootname" becomes the title of the plot):
inter <- d3tree3( p , width = "80%", height ="600px", rootname = "Favorite Car") #  width = "200%", height ="600px",


inter
# save the widget
#install.packages("htmlwidgets")

saveWidget(inter, file=paste0( getwd(), "/www/HtmlWidget/interactiveTreemap.html"))
