



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




movies <- read_csv("C:/Users/onepi/Documents/NEU/IE6600/Final Project/Data/tmdb_5000_movies.csv", na="NA")
credits <- read_csv("C:/Users/onepi/Documents/NEU/IE6600/Final Project/Data/tmdb_5000_credits.csv",  na="NA")



S007_movie <- movies %>%
  filter(id %in% c(710,714,707,658,657,646,699,709,253,660,36643,668,691,698,682,681,708,12208
                   ,36669,667,700,36670,36557,10764,37724,206647)) 

S007_credit <- credits %>%
  filter(movie_id %in% c(710,714,707,658,657,646,699,709,253,660,36643,668,691,698,682,681,708,12208
                   ,36669,667,700,36670,36557,10764,37724,206647)) 

cast <- S007_credit %>%
  filter(nchar(cast)>2) %>%
  mutate(js = lapply(cast, fromJSON)) %>%
  unnest(js) %>%
  select(-cast, -crew, -credit_id) %>%
  rename(actor=name, movie_cast_id=cast_id, actor_id=id) %>%
  mutate_if(is.character, factor) %>%
  filter(order == 0)




glimpse(S007)

