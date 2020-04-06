


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



#read csvs
movies <- read_csv("www/data/tmdb_5000_movies.csv", na="NA")
credits <- read_csv("www/data/tmdb_5000_credits.csv",  na="NA")
oscars <- read_csv("www/data/oscar.csv",  na="NA")
Coolcars <- read_csv("www/data/cars.csv")
Kills <- read_csv("www/data/Kills per Bond movie.csv",  na="NA")




#loading data
S007_movie <- movies %>%
  filter(id %in% c(710,714,707,658,657,646,699,709,253,660,36643,668,691,698,682,681,708,12208
                   ,36669,667,700,36670,36557,10764,37724,206647)) 

S007_credit <- credits %>%
  filter(movie_id %in% c(710,714,707,658,657,646,699,709,253,660,36643,668,691,698,682,681,708,12208
                   ,36669,667,700,36670,36557,10764,37724,206647)) 

#filtering cast
cast <- S007_credit %>%
  filter(nchar(cast)>2) %>%
  mutate(js = lapply(cast, fromJSON)) %>%
  unnest(js) %>%
  select(-cast, -crew, -credit_id) %>%
  rename(actor=name, movie_cast_id=cast_id, actor_id=id) %>%
  mutate_if(is.character, factor) %>%
  filter(order == 0)





