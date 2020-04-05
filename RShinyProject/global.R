
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
# The following code refered from  
#Winston Chang <winston@rstudio.com>
#AuthorUrl: http://www.rstudio.com/
# License: MIT
# Type: Shiny
# Tags: ggvis 




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
oscars <- read_csv("www/data/oscar.csv",  na="NA")
Coolcars <- read_csv("www/data/cars.csv")
Kills <- read_csv("www/data/Kills per Bond movie.csv",  na="NA")


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
