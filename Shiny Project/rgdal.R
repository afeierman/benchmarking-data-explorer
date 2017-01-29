library(rgdal)
library(dplyr)
library(ggplot2)
library(broom)

zips <- readOGR("/home/andrew/Downloads/", layer = "cb_2015_us_zcta510_500k")
class(zips)
zips[1,1]
nrow(zips)
map_data_fortified <- fortify(zips, region = "BFS_ID") %>% 
  mutate(id = as.numeric(id))