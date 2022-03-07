
library(crawl)
library(foieGras)
library(tidyverse)
library(sf)
library(bayesmove)

data("beardedSeals")
data("sese")


## Explore example datasets from foieGras and crawl
ggplot(beardedSeals, aes(longitude, latitude, color = ptt)) +
  geom_path() +
  theme_bw()

ggplot(sese, aes(lon, lat, color = id)) +
  geom_path() +
  theme_bw()


beardedSeals %>% 
  rename(x = longitude, y = latitude, date = date_time, id = ptt) %>% 
  bayesmove::shiny_tracks(., epsg = 4326)

sese %>% 
  rename(x = lon, y = lat) %>% 
  bayesmove::shiny_tracks(., epsg = 4326)



## Project coordinates for calculation of step length, turning angle, and NSD

# Bearded Seals

beard.proj <- beardedSeals %>% 
  st_as_sf(., coords = c('longitude','latitude'), crs = 4326, remove = FALSE) %>% 
  st_transform(., crs = 3571) %>% 
  mutate(x = st_coordinates(.)[,1],
         y = st_coordinates(.)[,2],
         .after = longitude) %>% 
  st_drop_geometry()

beard.proj2 <- prep_data(beard.proj, coord.names = c('x','y'), id = 'ptt')

beard.proj2 %>% 
  rename(date = date_time, id = ptt) %>% 
 shiny_tracks(., epsg = 3571)




# Southern Elephant Seals

sese.proj <- sese %>% 
  st_as_sf(., coords = c('lon','lat'), crs = 4326, remove = FALSE) %>% 
  st_transform(., crs = "+proj=laea +lat_0=-90 +lon_0=75 +ellps=WGS84 +units=m +no_defs") %>% 
  mutate(x = st_coordinates(.)[,1],
         y = st_coordinates(.)[,2],
         .after = lat) %>% 
  st_drop_geometry()

sese.proj2 <- prep_data(sese.proj, coord.names = c('x','y'), id = 'id')

shiny_tracks(sese.proj2, epsg = "+proj=laea +lat_0=-90 +lon_0=75 +ellps=WGS84 +units=m +no_defs")




## Export processed datasets

write.csv(beard.proj2, "Bearded_Seals.csv", row.names = FALSE)
write.csv(sese.proj2, "Southern_Elephant_Seals.csv", row.names = FALSE)
