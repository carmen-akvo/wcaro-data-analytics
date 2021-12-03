# MAPS for WCARO output 2

# Library
library(here)
library(tidyverse)
library(data.table)
library(readxl)
library(sf)
library(leaflet)
library(reshape2)

library(sp)
library(rgdal)
library(maps)
library(kableExtra)
library(ggplot2)

# Data
wp_2016_processed <- read.csv2(here("data/processed","wp_2016_processed.csv"))
hh_2018 <-  read_excel(here("data/processed","SL_household.xlsx"))
sl.shape <-read_sf(dsn = here::here("data/raw/SIL_admin_SHP/", "SIL.shp"))

fb_pop_sl <- read.csv2(here("data/raw", "population_sle_2019-07-01.csv"), sep=",")
fb_pop_sl <- fb_pop_sl %>%
  mutate(ID = paste0("V", rownames(fb_pop_sl))) %>%
  mutate(Lon = as.numeric(Lon)) %>%
  mutate(Lat = as.numeric(Lat)) %>%
  mutate(Population = as.numeric(Population))

FB_density_admin <- st_read(here("data/processed", "FB density with admin level.shp"))

FB_density_bonthe <- FB_density_admin %>% 
  filter(ADM2 == "Bonthe") %>%
  mutate(Population = as.numeric(Population)) %>%
  rename(ID = ID_1) %>%
  left_join(fb_pop_sl)


# Data cleaning
wp_2016_processed <- wp_2016_processed  %>%
  mutate(sdg_improved_source = recode(
    sdg_improved_source,
    "Improved" = "improved",
    "Unimproved" = "unimproved",
    .default="unimproved")) %>%
  mutate(Functionality = recode(
    Functionality,
    "No - Broken down" = "Not functional",
    "No - Still under construction" = "Not functional",
    "No - Under rehabilitation" = "Not functional",
    "Yes – Functional (but not in use)" = "Not functional",
    "Yes - But damaged" = "Not functional",
    "Yes – Functional (and in use)" = "Functional and in use"))

hh_2018 <- hh_2018 %>%
  mutate(geolocation_Latitude = as.numeric(geolocation_Latitude)) %>%
  mutate(geolocation_Longitude = as.numeric(geolocation_Longitude)) %>%
  mutate(round_trip_minutes = factor(
    round_trip_minutes, levels=c("Water on premises",
                                 "Less than 30 minutes",
                                 "More than 30 minutes but less than 1 hr",
                                 "More than 1 hr but less than 2hrs",
                                 "More than 2 hrs"))) %>%
  mutate(main_source_water = factor(
    main_source_water, levels=c( "Piped water to yard/plot",
                       "Protected dug well",
                       "Protected spring",
                       "Public tap/standpipe",
                       "Tubewell/borehole",
                       "Unprotected dug well",
                       "Unprotected spring",
                       "Surface water (river, dam, lake, pond, stream, canal, irrigation channels)"))) %>%
  mutate(wp_status = recode(main_source_water,
                         "Piped water to yard/plot" = "Improved",
                         "Protected dug well" = "Improved",
                         "Protected spring" = "Improved",
                         "Public tap/standpipe" = "Improved",
                         "Surface water (river, dam, lake, pond, stream, canal, irrigation channels)" = "Unimproved",
                         "Tubewell/borehole" = "Improved",
                         "Unprotected dug well" = "Unimproved",
                         "Unprotected spring" = "Unimproved"))


# Select Bonthe data
shape_bonthe <- ggplot2::fortify(sl.shape, region='NAME') %>%
  filter(ADM2 == "Bonthe")

wp_bonthe <- wp_2016_processed %>%
  filter(ADM2 == "Bonthe")

hh_bonthe <- hh_2018 %>%
  filter(location_District == "Bonthe")

# Shape data and WP map
ggplot(shape_bonthe) +
  geom_sf(size = 0.3, color = "#142F43", fill="#ECFCFF") +
  theme_minimal() +
  geom_point(data=wp_bonthe,
             aes(x=Longitude, y=Latitude, fill=sdg_improved_source),
             size = 2, alpha=1, shape=23) +
  scale_fill_manual(values=c("#1EAFED", "#FFCD60")) +
  labs(title="2016 Waterpoint mapping SL: Bonthe", x="", y="") +
  theme(legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(size=8),
        plot.title = element_text(size=14),
        axis.text.x = element_text(size=8),
        axis.text.y = element_text(size=8),
        axis.ticks = element_blank())


# Shape data and hh map
ggplot(shape_bonthe) +
  geom_sf(size = 0.3, color = "#142F43", fill="#ECFCFF") +
  theme_minimal() +
  geom_point(data=hh_bonthe %>% filter(Identifier != "65gg-c9k3-4f60"),
             aes(x=geolocation_Longitude, y=geolocation_Latitude, fill=round_trip_minutes),
             size = 1.5, alpha=1, shape=21, stroke=0.1) +
  scale_fill_manual(values=c("#C2FCF6","#5FF4EE","#4A9FF5","#033FFF")) +
  labs(title="Water supply househols", x="", y="") +
  theme(legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(size=8),
        plot.title = element_text(size=14),
        axis.text.x = element_text(size=8),
        axis.text.y = element_text(size=8),
        axis.ticks = element_blank())


# Shape data and WP map
ggplot(shape_bonthe) +
  geom_sf(size = 0.3, color = "#142F43", fill="#142F43") +
  theme_minimal() +
  geom_point(data=wp_bonthe,
             aes(x=Longitude, y=Latitude), fill = "#009DAE",
             size = 2, shape=23, stroke=0.25) +
  geom_point(data=hh_bonthe %>% filter(Identifier != "65gg-c9k3-4f60"),
             aes(x=geolocation_Longitude, y=geolocation_Latitude),
             size = 1.5, alpha=0.5, shape=20, stroke=0.1, color="#f2f2f2") +
  labs(title="2016 Waterpoint mapping SL: Bonthe", x="", y="") +
  theme(legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(size=8),
        plot.title = element_text(size=14),
        axis.text.x = element_text(size=8),
        axis.text.y = element_text(size=8),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill="#9D9D9D"))

# Household water point type
hh_wp_type <- hh_bonthe$main_source_water %>%
  table() %>%
  melt("wp_type")

ggplot(hh_wp_type, aes(x=wp_type, y=value, fill=status, label=value)) +
  geom_bar(stat = "identity", width=0.6) +
  scale_fill_manual(values=c("#1EAFED", "#FFCD60")) +
  theme_minimal() +
  coord_flip() +
  labs(title="Water source type in Bonthe", x="", y="") +
  scale_x_discrete(labels=c("Piped water to yard/plot",
                   "Protected dug well",
                   "Protected spring",
                   "Public tap/standpipe",
                   "Tubewell/borehole",
                   "Unprotected dug well",
                   "Unprotected spring",
                   "Surface water\n(river, dam, lake, pond,\nstream, canal, irrigation channels)")) +
  geom_text(nudge_y = -10, size=4, color="white", family="Assistant-Bold") +
  theme(plot.title = element_text(size=12, family="Roboto-Regular"),
        legend.position = "right",
        legend.text = element_text(size=10, family="Assistant-Bold", color="#252525"),
        axis.text.y = element_text(size=10, family="Assistant-Bold"),
        axis.text.x = element_text(size=10, family="Assistant-Bold"),
        legend.title = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks = element_blank())

# Household distance to WP
hh_distance <- hh_bonthe$round_trip_minutes %>%
  table() %>%
  melt("distance") %>%
  mutate(status = recode(distance,
                         "Water on premises" = "Basic",
                         "Less than 30 minutes" = "Basic",
                         "More than 30 minutes but less than 1 hr" = "Limited",
                         "More than 1 hr but less than 2hrs" = "Limited",
                         "More than 2 hrs" = "Limited"))

ggplot(hh_distance, aes(x=distance, y=value, fill=status, label=value)) +
  geom_bar(stat = "identity", width=0.6) +
  scale_fill_manual(values=c("#1EAFED", "#FFCD60")) +
  theme_minimal() +
  coord_flip() +
  scale_x_discrete(labels=c("Water on premises",
                            "Less than 30 minutes",
                            "More than 30 minutes\nbut less than 1 hr",
                            "More than 1 hr but\nless than 2hrs",
                            "More than 2 hrs")) +
  labs(title="Time taken to fetch water in Bonthe", x="", y="") +
  geom_text(nudge_y = -20, size=4, color="white", family="Assistant-Bold") +
  theme(plot.title = element_text(size=12, family="Roboto-Regular"),
        legend.position = "right",
        legend.text = element_text(size=10, family="Assistant-Bold", color="#252525"),
        axis.text.y = element_text(size=10, family="Assistant-Bold"),
        axis.text.x = element_text(size=10, family="Assistant-Bold"),
        legend.title = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks = element_blank())
