library(here)
library(tidyverse)
library(ggplot2)
library(spatialrisk)

# Population data
fb_pop_sl <- read.csv2(here("data/raw", "population_sle_2019-07-01.csv"), sep=",")
fb_test <- fb_pop_sl %>% 
  mutate(Lat = as.numeric(Lat)) %>%
  mutate(Lon = as.numeric(Lon)) 

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


### Population within water point range

# Draw circle around water points
# determine population density point in the circle
# Determine number of people in the range

# WP data 2016
wp_2016 <- read_excel(here("data/raw","wp_data_2016_national_mapping.xlsx"))
wp_2016 <- wp_2016 %>% 
  mutate(Longitude = as.numeric(Longitude)) %>%
  mutate(Latitude = as.numeric(Latitude)) %>%
  filter(Latitude < 20) %>%
  filter(Longitude < -5) %>%
  filter(Longitude > -20) %>%
  unique()

# Convert to sf, set the crs to EPSG:4326 (lat/long), 
# and transform to EPSG:3035
wp_2016_sf <- st_as_sf(wp_2016, coords = c("Longitude", "Latitude"), crs = 4326) %>% 
  st_transform(3035)

# Buffer circles by 100m
wp_2016_circles <- st_buffer(wp_2016_sf, dist = 500)

# Polygon

# Different type of shapefile? throws error for missing crs
# sl.shape <- read_sf(dsn = here::here("data/raw/SIL_admin_SHP/", "SIL.shp")) %>%
#   st_set_crs(4326) %>% st_transform(3035)

sl_sf <- getData(name = "GADM", 
                         country = "SL", 
                         level = 3) %>%
  # convert to simple features
  sf::st_as_sf() %>%
  # Filter down to Ticino
  st_transform(3035)

# Intersect the circles with the polygons
wp_2016_int_circles <- st_intersection(sl_sf, wp_2016_circles)




# UNICEF WP
waterpoints_within_distance <- NULL

for(i in 1:dim(wp_2016)[1]){
  
  # 2018 water point
  point <- wp_2016[i,]
  
  # data frame 2016 water points within distance
  df <- spatialrisk::points_in_circle(fb_test, 
                                      point$Longitude, 
                                      point$Latitude, 
                                      radius = 500, 
                                      lon = Lon, lat= Lat)
  
  # Add ID of 2018 water point to data frame
  df$wp_2018 <- point$identifier
  df$implementing_partner_2018 <- point$implementing_partner
  df$risk_assessment_2018 <- point$risk_assessment
  df$ecoli_risk_score_2018 <- point$ecoli_risk_indication
  
  waterpoints_within_distance <- rbind(waterpoints_within_distance, df)
  
}



