# The code refered from  
#Movie Comparsion
#Author:Winston Chang <winston@rstudio.com>
#AuthorUrl: http://www.rstudio.com/
# License: MIT
# Type: Shiny
# Tags: ggvis 

#The Bonds
#Author:aagarw30 <aagarw30@gmail.com>
#AuthorUrl:https://github.com/aagarw30/R-Shinyapp-Tutorial/blob/master/SelectAll_None_Choices/selectall2.R
#Type: Shiny
#Tags: R-Shinyapp-Tutorial




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

#Actors
library(fmsb)
library(RColorBrewer)
library(scales)
#Car
#devtools::install_github("timelyportfolio/d3treeR")
library(d3treeR)
library(treemap)

#Countries
library(maps)
library(leaflet)
library(sf)
library(kableExtra)
library(leaflet)
library(maptools)
#install.packages("kableExtra")

#Movies
library(ggvis)
library(plotly)

if (FALSE) {
  library(ggvis)
  library(dbplyr)
}
#variable 
axis_vars <- c(
  "budget" = "budget",
  "revenue"="revenue"
  
)

#read csvs
movies <- read_csv("www/data/tmdb_5000_movies.csv", na="NA")
credits <- read_csv("www/data/tmdb_5000_credits.csv",  na="NA")
oscars <- read_csv("www/data/OScar.csv",  na="NA")
Coolcars <- read_csv("www/data/cars.csv")
Kills <- read_csv("www/data/Kills per Bond movie.csv",  na="NA")

country <- read_csv("www/data/contries.csv", na="NA")


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
cast$title<-as.character(cast$title)

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
all_movies[,"budget"]<-all_movies[,"budget"]/1000000
all_movies[,"revenue"]<-all_movies[,"revenue"]/1000000


#data cleaning radar
# kills dataframe cleaning
Kills<-Kills[-c(24,25),]
Kills[Kills$Film=="From Russia With Love",1]<-"From Russia with Love"
Kills[Kills$Film=="The World is Not Enough",1]<- "The World Is Not Enough"


#create tab2 dataframe with normalized data

tab2<-right_join(cast,S007_movie,by="title") %>% 
  left_join(Kills,by=c("title"="Film")) %>% 
  transmute(actor,
            title,
            popularity=(popularity-min(popularity))/(max(popularity)-min(popularity)),
            profit=((revenue-budget)-min((revenue-budget)))/(max((revenue-budget))-min((revenue-budget))),
            violence=(Total-min(Total,na.rm = TRUE))/(max(Total,na.rm = TRUE)-min(Total,na.rm = TRUE)))

#transpose
tab2_bonds<-group_by(tab2,actor) %>% 
  summarise(Popularity=mean(popularity),
            Profit=mean(profit),
            Violence=mean(violence,na.rm=TRUE))
row.names(tab2_bonds) <- tab2_bonds$actor
tab2_bonds<-data.frame(t(tab2_bonds))
tab2_bonds<-tab2_bonds[-1,] 

tab2_bonds[] <-lapply(tab2_bonds, function(x) as.numeric(as.character(x)))



#data cleaning car
Coolcars$Brand <- gsub("([A-Za-z]+).*", "\\1", Coolcars$Vehicle)
test_car <- Coolcars %>% group_by(Vehicle) %>% tally()
Car <- left_join(Coolcars, test_car, by = c("Vehicle", "Vehicle"))
Car <- Car %>% filter(!Brand %in% "Chinese")


# basic treemap
p <- treemap(Car,
             index=c("Brand","Vehicle"),
             vSize="n",
             type="value",
             palette =  c(
               "#004225",
               "#f2f3f4",
               "#002e63",
               "#c0c0c0"), #grey
             bg.labels=c("#0f0f0f"),
             align.labels=list(
               c("center", "center"), 
               c("right", "bottom")
             ),
            # position.legend = "none",
             lowerbound.cex.labels = 0.4,
             inflate.labels = TRUE
)       

# For map polygon coloring


dataframeToPolygons <- function(df, latCol, lngCol, idCol, var) {
  temp <- SpatialPolygons(lapply(unique(df[[idCol]]), function(id) {
    Polygons(list(Polygon(as.matrix(
      df[df[[idCol]] == id, c(lngCol, latCol)]
    ))), id)
  }))
  temp$Freq <- var
  return(temp)
}
country<-read.csv("www/data/contries.csv")
dat <- country
country <- sapply(dat$Location, function(x) str_split(x, pattern=","))
country <- sapply(country, function(x) x[length(x)])
country <- sapply(country, function(x) substr(x, start = 2, stop=str_length(x)))

country[country == 'England'] <- "UK"
country[country == 'Florida'] <- "USA"
country[country == 'he Rock of Gibraltar'] <- "UK"
country[country == 'iberia'] <- "Russia"
country[country == 'Macau'] <- "China"
country[country == 'ontenegro'] <- "Montenegro"
country[country == 'outh Africa'] <- "South Africa"
country[country == 'outh China Sea'] <- "China"
country[country == 'outh Korea'] <- "South Korea"
country[country == 'Scotland'] <- "UK"
country[country == 'Yugoslavia'] <- "Montenegro"
country[country=='adagascar'] <- 'Madagascar'
country[country=='Italy\n'] <- 'Italy'
country[country=='ustrian Alps'] <- 'Austria'

dat$country <- country

country_freq <- as.data.frame(table(country))
country_data <- map_data("world")
names(country_freq)[1] <- "region"

country_map <- left_join(country_data, country_freq, by = "region")
country_map$Freq[is.na(country_map$Freq)] <- 0
tab3_country <- country_map

tab3_country_sf <- st_as_sf(x = tab3_country,                         
                            coords = c("long", "lat"))

freq <- sapply(unique(country_map$group), 
               function(x) unique(country_map$Freq[country_map$group==x]))
region <- sapply(unique(country_map$group), 
                 function(x) unique(country_map$region[country_map$group==x]))

pal2 <- colorFactor(
  palette = c('white', 'yellow', 'red'),
  domain = freq
)




