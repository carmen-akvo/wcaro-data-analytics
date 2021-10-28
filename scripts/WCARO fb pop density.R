library(here)
library(tidyverse)
library(ggplot2)

# Population data
fb_pop_sl <- read.csv2(here("ASWA/data", "population_sle_2019-07-01.csv"), sep=",")
fb_test <- fb_pop_sl %>% 
  mutate(Lat = as.numeric(Lat)) %>%
  mutate(Lon = as.numeric(Lon)) %>%
  head(10000)

# Wells sl
wells <- read.csv2(here("SL/Data", "SL_wells.csv"), sep=";") %>%
  mutate(geo.latitude = as.numeric(geo.latitude)) %>%
  mutate(geo.longitude = as.numeric(geo.longitude)) 

# size = 0.1, color = "#603f83ff", fill="#c7d3d4ff"

gg <- ggplot(data = sl.shape.data) +   
  geom_sf() + coord_sf() +
  geom_point(data=fb_test, aes(x=Lon, y=Lat), color="#ff9900", size=0.5, alpha=0.3) + 
  # coord_fixed(1) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())

gg + geom_point(data=wells, aes(y=geo.latitude, x=geo.longitude), color="#756bb1", size=0.5, alpha=0.8)


sl.shape <-read_sf(dsn = here::here("SL/Data/SIL_admin_SHP/", "SIL.shp"))
sl.shape.data <- ggplot2::fortify(sl.shape, region='NAME')

ggplot(sl.shape.data) +
  geom_sf(size = 0.1, color = "#603f83ff", fill="#c7d3d4ff") +
  theme_minimal()

