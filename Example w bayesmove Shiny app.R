library(bayesmove)
library(tidyverse)


beard <- read.csv("Bearded_Seals.csv")
sese <- read.csv("Southern_Elephant_Seals.csv")


beard %>% 
  rename(id = ptt, date = date_time) %>% 
  bayesmove::shiny_tracks(., 3571)


bayesmove::shiny_tracks(sese, "+proj=laea +lat_0=-90 +lon_0=75 +ellps=WGS84 +units=m +no_defs")
