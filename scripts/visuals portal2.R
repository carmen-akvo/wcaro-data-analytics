library(tidyverse)
library(sf)
library(leaflet)

# afr.shape <-read_sf(dsn = here::here("Mali/data/shapefiles/afr_g2014_2013_0/", 
#                                      "afr_g2014_2013_0.shp"))

sahel.shape <-read_sf(dsn = here::here("ASWA/data/shapefiles/sah_admin0_ocha/", 
                                     "sah_admin0_ocha.shp"))

data <- data.frame("admin0Name" = c("Mali", "Burkina Faso", "Nigeria"),
                   "2019_tar" = c(100000, 200000, 20000),
                   "2019_val" = c(100000, 150000, NA),
                   "2020_tar" = c(200000, 400000, 40000),
                   "2020_val" = c(150000, 180000, NA),
                  "2021_tar" = c(300000, 600000, 80000),
                   "2021_val" = c(270000, NA, NA),
                   "2022_tar" = c(400000, 800000, 120000),
                   "2022_val" = c(NA, NA, NA))

data <- data %>% 
  mutate(current_percentage = ifelse(
    is.na(X2021_val),
    (X2020_val/X2021_tar)*100, 
    (X2021_val/X2021_tar)*100))

shape.data <- ggplot2::fortify(sahel.shape, region='admin0Name') %>%
  left_join(data) 

bins <- c(10,20,30,40,50,60,70,80,90,100)
pal <- colorBin("YlOrRd", domain = shape.data$current_percentage, bins = bins)


content <- paste(sep = "<br/>",
                 "<b>Target by 2022: </b>", data %>% filter(admin0Name == "Mali") %>% select(X2022_tar) %>% pull()
)

leaflet(shape.data, width="100%") %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(
    fillColor = ~pal(current_percentage),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7) %>% 
  addLegend(pal = pal, values = ~current_percentage, opacity = 0.7,
            title = "Nr of functional WP",
            position = "bottomright") %>%
  addPopups(13, 10, content,
            options = popupOptions(closeButton = FALSE)
  )
