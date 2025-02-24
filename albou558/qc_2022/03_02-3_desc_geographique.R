library(clessnize)
library(tidyverse)
library(sf)


shp <- sf::read_sf("_SharedFolder_datagotchi_federal_2024/data/shapefile/qc2022/Circonscription_electorale_2022_shapefile.shp") 



ggplot(shp) +
  geom_sf(color = "grey20") +
  theme_void()

