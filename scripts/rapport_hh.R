## ---- start ----
library(here)
library(tidyr)
library(dplyr)
library(ggplot2)
library(reshape2)
library(extrafont)
library(DescTools)
library(stringr)
library(gridExtra)
library(readxl)
library(zoo)
library(corrplot)
require(scales)
library(RANN)
library(tidyverse)
# Shapefiles
library(rgdal)
library(sf)
library(Imap)
## POPULATION
# Idee: totale populatie is bekend + de propotie verdeeld over de districten
# adhv deze gegevens inschatting maken van populatie per district -> huishoudens per district?
# Divide by average hh size should result in nr of households per district 

# Read processed data files of the wells
household <- read.csv2(here::here("output","SL_subset.csv"), sep=",")
wp_2017 <- read.csv2(here("Data", "wp_data_2017.csv"))

wp_2017 <- wp_2017 %>% 
  mutate(Longitude = as.numeric(as.character(Longitude))) %>%
  mutate(Latitude = as.numeric(as.character(Latitude))) %>%
  filter(Longitude <  -5 & Longitude > -20) %>%
  filter(Latitude < 10) %>%
  mutate(`national wp` = "national water point mapping 2017")

wp_2017_test <- wp_2017 %>% 
  select(Longitude, Latitude, `national wp`) %>%
  mutate(Longitude = round(Longitude,3)) %>%
  mutate(Latitude = round(Latitude,3)) %>%
  rename(geo_longitude = Longitude,
         geo_latitude = Latitude) %>%
  left_join(waterpoints %>% 
              select(geo_longitude, geo_latitude, `Unicef waterpoints`) %>%
              mutate(geo_longitude = round(geo_longitude,3)) %>%
              mutate(geo_latitude = round(geo_latitude,3)))

cnames <- aggregate(cbind(geo_longitude, geo_latitude) ~ district, 
                    data=waterpoints, 
                    FUN=function(x)mean(range(x)))

waterpoint_dist_min <- list()

# iterate through data frame placing calculated distance next to place place names
  
dist_list <- data.frame()

for(i in 1:nrow(wp_2017)){
  df <- data.frame(lat = waterpoints$geo_latitude, 
                   lon = waterpoints$geo_longitude, 
                   id = waterpoints$identifier)
  df$distance <- gdist(lon.1 = wp_2017$Longitude[i], 
                          lat.1 = wp_2017$Latitude[i], 
                          lon.2 = waterpoints$geo_longitude, 
                          lat.2 = waterpoints$geo_latitude, 
                          units="m")
  df <- df %>% filter(distance == min(distance))
  dist_list <- rbind(dist_list, df)
}

matching_wp <- cbind(dist_list, wp_2017$Geo.Code) %>% 
  group_by(id) %>%
  filter(distance == min(distance)) %>%
  filter(distance < 30) 

waterpoint_match <- matching_wp %>%
  rename(identifier = id,
         geo_code = `wp_2017$Geo.Code`) %>%
  right_join(waterpoints, by="identifier") %>%
  mutate(`Unicef waterpoints` = ifelse(is.na(geo_code), `Unicef waterpoints`, "Unicef wp mapped in 2017"))


# Shape files
# # Communities  
sl.shape <-read_sf(dsn = here::here("/Data/SIL_admin_SHP/", "SIL.shp"))

## ---- processing ----

# LOCATIE navragen bij bouzmane - ASk Bounzoumana
household <- household %>% 
  # DATA TYPES
  mutate(geolocation_latitude = as.numeric(as.character(geolocation_latitude))) %>%
  mutate(geolocation_longitude = as.numeric( as.character(geolocation_longitude))) %>%
  # FILTERS
  filter(!is.na(main_source_water)) %>%
  filter(geolocation_longitude < 0) %>%
  # RECODE FOR JMP
  mutate(sdg_improved_source = recode(
    main_source_water, 
    "unprotected dug well" = "Unimproved", 
    "piped water to yard/plot" = "Improved", 
    "protected dug well" = "Improved", 
    "unprotected spring" = "Unimproved", 
    "rainwater collection" = "Improved",
    "protected spring" = "Improved", 
    "piped water into dwelling" = "Improved", 
    "tubewell/borehole" = "Unimproved", 
    "public tap/standpipe" = "Improved", 
    "surface water" = "surface water")) %>%
  mutate(sdg_round_trip = recode(
    round_trip_minutes, 
    "Less than 30 minutes" = "Less than 30 minutes",
    "More than 30 minutes but less than 1 hr" = "More than 30 minutes", 
    "More than 1 hr but less than 2hrs" = "More than 30 minutes", 
    "Water on premises" = "Water on premises", 
    "More than 2 hrs" = "More than 30 minutes")) %>%
  mutate(sdg_hand_washing =  recode(
    hand_wash_observe,
    "Yes" = "Basic/Limited",
    "No" = "No facility"
  )) %>%
  mutate(sdg_sanitation = recode(
    toilet_facility_type,
    "bush or field" = "Open Defecation",
    "pit latrine with concrete slab" = "Improved",
    "pit latrine with other type of slab" = "Improved",
    "ventilated improved pit latrine (vip)" = "Improved",
    "composting toilet" = "Improved",
    "pit latrine without slab/open pit" = "Unimproved", 
    "flush/pour flush" = "Improved",
    "uses toilet of neighbor" = "Unimproved",
    "bucket" = "Unimproved",
  ))


household$main_source_water <- gsub(
  " \\(river, dam, lake, pond, stream, canal, irrigation channels)", "", 
  household$main_source_water)

# Read processed data files of the wells
waterpoints <- read.csv2(here("output","SL_wells.csv")) %>%
  rename_all(
    funs(
      stringr::str_replace_all(., '\\.', '_')
    )
  )

waterpoints <- waterpoints %>% 
  separate(`ecoli_risk_indication`, c("ecoli_risk_indication", "RL probability"), sep = " / ") %>%
  mutate(
    ecoli_risk_indication = factor(
      ecoli_risk_indication,
      levels=c("Very High Risk", "High Risk", "Intermediate Risk", "Low Risk"))) %>%
  mutate(geo_latitude = as.numeric(as.character(geo_latitude)))  %>%
  mutate(geo_longitude = as.numeric(as.character(geo_longitude)))

## MAP ####

## ---- sample ----

location <- as.table(apply(
  household[,c("location_region","location_district",
               "location_chiefdom")],
  2, n_distinct))
names(location) <- c("Regions", "Districts", "Chiefdoms")
location <- data.frame(location)
names(location) <- c(" ", "Sample")
location$Country <- c(5, 16, 190)
location <- location %>%
  add_row(` `="Counties", Sample="", Country= 1316) %>% 
  add_row(` `="Communities", Sample=1100, Country= "") %>% 
  add_row(` `="Households", Sample=2427, Country= 7076119) 


# Create map number of surveyed households
districts <- as.data.frame(table(household$location_district)) %>%
  mutate(Var1 = factor(Var1)) %>%
  dplyr::rename(ADM2 = Var1,
                nr_of_households_surveyed = Freq)
districts$population <- c(575478, 606544, 200781, 526379, 609891, 409372, 506100, 318588, 346461, 531435, 444270)
districts$nr_of_households <- districts$population/mean(household$number_household, na.rm=TRUE)
districts$percent_surveyed <- (districts$nr_of_households_surveyed/districts$nr_of_households)*100

sl.shape.data <- ggplot2::fortify(sl.shape, region='NAME')
sl.shape.data <- sl.shape.data %>% 
  left_join(districts) %>%
  mutate(percent_surveyed = replace_na(percent_surveyed, 0))
sl.shape.data <- unique(sl.shape.data[,c("ADM2", "geometry", "percent_surveyed")])

surveyed_hh <- ggplot(sl.shape.data) +
  
  #add a shapefile layer to the plot, set the line size and colour for the polygons, then fill them according to our data
  geom_sf(size = 0.3, color = "#808080", aes(fill=percent_surveyed)) +
  # geom_sf_label(aes(label=ADM2)) +
  scale_fill_gradient(low="#F2F2F2",high="#404898", na.value="#F2F2F2") +
  
  #choose colours for the fill gradient, and na values
  theme(panel.grid.major = element_line("white")) +
  theme_minimal() +
  guides(colour = guide_legend(override.aes = list(size=5))) +
  labs(title="Percentage of households surveyed", x ="", y = "", fill="Percentage") +
  theme(legend.title = element_text("Roboto-Regular", size=8),
        plot.title = element_text("Roboto-Regular", size=10),
        axis.title.x  = element_text(family = "Assistant-Regular", size=8),
        axis.title.y  = element_text(family = "Assistant-Regular", size=8),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())


map_data <- waterpoints %>% select(geo_latitude, geo_longitude) %>%
  mutate("data" = "UNICEF well") %>%
  mutate("size" = 3) %>%
  mutate("type" = 21) %>%
  rename(geolocation_longitude = geo_longitude,
         geolocation_latitude = geo_latitude) %>%
  rbind(household %>% select(geolocation_latitude, geolocation_longitude) %>% 
          mutate("data" = "Household") %>%
          mutate("size" = 0.5) %>%
          mutate("type" = 1) )

waterpoints$`Unicef waterpoints` <- "Unicef waterpoints"
household$`households` <- "Surveyed households"

general_map <- ggplot(sl.shape.data) +
  geom_sf(size = 0.1, color = "#603f83ff") +
  theme_minimal() +
  theme(panel.grid.major = element_line("white")) +
  # guides(colour = guide_legend(override.aes = list(size=2))) +
  labs(title="Households and wells presented in this survey", x="", y="") +
  theme(legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text("Assistant-Regular", size=8),
        plot.title = element_text("Roboto-Regular", size=8),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        legend.key.height = unit(1, "line"))

household_map <- general_map + 
  geom_point(data=household, 
             aes(x=geolocation_longitude, y=geolocation_latitude, color=households), 
             size = 0.5, alpha = 0.8) +
  scale_color_manual(values = "#4f3558")

uniwells_map <- general_map + 
  geom_point(data=waterpoints, 
             aes(x=geo_longitude, y=geo_latitude, fill=`Unicef waterpoints`), 
             size = 3, alpha=0.5, shape=21, color="#325A88") +
  scale_fill_manual(values="#96d4ed")

household_uniwells_map <- household_map + 
  geom_point(data=waterpoints, 
             aes(x=geo_longitude, y=geo_latitude, fill=`Unicef waterpoints`), 
             size = 3, alpha=0.5, shape=21, color="#03AD8C") +
  scale_fill_manual(values="#96d4ed")


waterpoints_2017 <- general_map + 
  geom_point(data=wp_2017,
             aes(x=Longitude, y=Latitude, color=`national wp`, fill=`national wp`),
             size = 2, alpha=0.5, shape=21) +
  geom_point(data=waterpoint_match,
             aes(x=geo_longitude, y=geo_latitude, color=`Unicef waterpoints`, fill=`Unicef waterpoints`),
             size = 2, alpha=1, shape=21 ) +
  scale_fill_manual(values=c("#a6cee3", "#d95f02", "#e7298a")) +
  scale_color_manual(values=c("#1f78b4", "#1f78b4", "#1f78b4")) +
  geom_text(data = cnames, 
            aes(geo_longitude, geo_latitude, label=district), 
            size=4, family="Assistant-Regular")
 

## ---- water ----####

household <- household %>% 
  mutate(
    drinking_water_jmp = 
      ifelse(
        sdg_improved_source == "Improved" & 
          sdg_round_trip == "Water on premises",
        "Safely managed", 
        ifelse(
          sdg_improved_source == "Improved" & 
            sdg_round_trip == "Less than 30 minutes",
          "Basic", 
          ifelse(
            sdg_improved_source == "Improved" & 
              !sdg_round_trip  %in% c("Water on premises","Less than 30 minutes"),
            "Limited", 
            ifelse(
              sdg_improved_source == "Unimproved",
              "Unimproved", "No service")
          )
        )
      )
  ) %>%
  mutate(drinking_water_jmp = factor(
  drinking_water_jmp, 
  levels=c("No service", 
           "Unimproved", 
           "Limited", 
           "Basic", 
           "Safely managed"))) %>%
  mutate(sdg_round_trip = factor(
  sdg_round_trip, 
  levels=c("Water on premises",
           "Less than 30 minutes", 
           "More than 30 minutes"))) %>%
  mutate(sdg_improved_source = factor(
  sdg_improved_source, 
  levels=c("Surface water", 
           "Unimproved", 
           "Improved")))

# Figures

# Drinking water service levels (JMP)
sdg_safely_managed <- ggplot(household, aes(x=drinking_water_jmp, fill=drinking_water_jmp)) +
  geom_bar(position="dodge", aes(y = (..count..)/sum(..count..)), colour="#D9D9D9", width=0.5) +
  theme_minimal() +
  coord_flip() +
  scale_fill_manual(values=c("#4F3558", "#40316B", "#28397E", "#196A91", "#04A58D")) +
  scale_y_continuous(labels = percent_format()) +
  # theme(legend.position = "none") +
  labs(title="Percentage of drinking water services levels over all communities", x ="", y = "Percentage of total", fill="Service levels") +
  theme(plot.title = element_text("Roboto-Regular", size=10),
        axis.title.y = element_text("Assistant-Regular", size=8),
        axis.title.x = element_text("Assistant-Regular", size=8),
        axis.text.y  = element_blank(),
        axis.text.x  = element_text("Assistant-Regular", size=8),
        legend.text = element_text("Assistant-Regular", size=8),  
        legend.title = element_text("Assistant-Regular", size=10),
        legend.key.size = unit(0.5,"line")) +
  guides(fill = guide_legend(reverse = TRUE))
sdg_safely_managed

# Service levels map
funct_color <- c("Safely managed" = "#03AD8C",
                 "Basic" = "#05a58d", 
                 "Limited" = "#328284",
                 "Unimproved" = "#4f3558",
                 "No service" = "red3")

service_level_map <- ggplot(sl.shape.data) +
  geom_sf(size = 0.3, color = "#808080") +
  theme(panel.grid.major = element_line("white")) +
  theme_minimal() +
  guides(colour = guide_legend(override.aes = list(size=2))) +
  labs(title="Drinking water service levels of the interviewed households", x="", y="") +
  theme(legend.title = element_blank(),
        legend.text = element_text("Assistant-Regular", size=8),
        plot.title = element_text("Roboto-Regular", size=8),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())+
  guides(fill = guide_legend(reverse = TRUE))


service_level_map_wp <- service_level_map +
  geom_point(data=waterpoints, 
             aes(x=geo_longitude, y=geo_latitude, fill=`Unicef waterpoints`), 
             size = 3, alpha=0.8, shape=21, color="#03AD8C") +
  scale_fill_manual(values = "#96d4ed")

service_level_map_hh <- service_level_map_wp +
  geom_point(data=household, 
             aes(x=geolocation_longitude, y=geolocation_latitude, 
                 color = drinking_water_jmp), size = 0.5, alpha = 0.8) +
  scale_color_manual(values=funct_color)


## ---- sanitation ----

household <- household %>%
  mutate(flush_to = as.character(flush_to)) %>%
  mutate(flush_to = replace_na(flush_to, "unknown")) %>%
  mutate(sanitation_jmp = 
           ifelse(
             sdg_sanitation == "Improved" & 
               share_facility == "No" &
               flush_to == "piped sewer system",
             "Safely managed", 
             ifelse(
               sdg_sanitation == "Improved" & 
                 share_facility == "No"&
                 flush_to != "piped sewer system",
               "Basic", 
               ifelse(
                 sdg_sanitation == "Improved" & 
                   share_facility  == "Yes",
                 "Limited", 
                 ifelse(
                   sdg_sanitation == "Unimproved",
                   "Unimproved", "Open Defecation"))))) %>%
  mutate(sanitation_jmp = factor(
    sanitation_jmp, 
    levels = c( "Open Defecation",
                "Unimproved",         
                "Limited", 
                "Basic", 
                "Safely managed")))

# Sanitation service levels (JMP)
sanitation_service_levels <- ggplot(
  household %>% filter(!is.na(sanitation_jmp)),
  aes(x=sanitation_jmp, fill=sanitation_jmp)) +
  geom_bar(position="dodge",aes(y = (..count..)/sum(..count..)), colour="#D9D9D9", width=0.5) +
  theme_minimal() +
  coord_flip() +
  guides(colour = guide_legend(override.aes = list(size=1))) +
  # theme(legend.position = "none") +
  scale_fill_manual(values=c("#4F3558", "#40316B", "#28397E", "#196A91", "#04A58D")) +
  scale_y_continuous(labels = percent_format()) + 
  labs(title="Percentage of sanitation service levels in the communities", x ="", y = "Percentage", fill="Sanitation service levels") +
  theme(plot.title = element_text("Roboto-Regular", size=10),
        axis.title.x  = element_text(family = "Assistant-Regular", size=8),
        axis.text.x  = element_text(family = "Assistant-Regular", size=8),
        axis.text.y  = element_blank(),
        legend.text = element_text("Assistant-Regular", size=8),
        legend.title = element_text("Assistant-Regular", size=10),
        legend.key.size = unit(0.5,"line"))+
  guides(fill = guide_legend(reverse = TRUE))

## ---- hygiene ----
hygiene_table <- melt(table(household$sdg_hand_washing))
hygiene_table$Percentage <- (hygiene_table$value/sum(hygiene_table$value))

hygiene_service_levels <- ggplot(hygiene_table, aes(x="", y=Percentage, fill=Var1)) +
  geom_bar(width = 1, stat="identity") +
  coord_polar("y", start=0) + 
  theme_minimal() +
  scale_fill_manual(values=c("#0D1E55", "#7f85a9")) +
  scale_y_continuous(labels = percent_format()) + 
  labs(title="Level of hand washing facilities present at the households", x ="", y = "Percentage of total", fill="") +
  theme(plot.title = element_text("Roboto-Regular", size=10),
        axis.title.x  = element_text(family = "Assistant-Regular", size=8),
        axis.text.y  = element_text("Assistant-Regular", size=8))


## ---- water_quality ----

# Quality of the water at the household

household <- household %>% 
  separate(risk.level.ecoli, c("risk_level_ecoli", "RL probability"), sep = "/")

household <- household %>% 
  mutate(
    risk_level_ecoli = factor(
      risk_level_ecoli, 
      levels=c("Very High Risk ", "High Risk ", "Intermediate Risk ", "Low Risk ")))

risk_levels <- ggplot(
  household %>% filter(!is.na(risk_level_ecoli)),
  aes(x=risk_level_ecoli, fill=risk_level_ecoli)) +
  geom_bar(position="dodge",aes(y = (..count..)/sum(..count..)), colour="#D9D9D9", width=0.5) +
  theme_minimal() +
  coord_flip() +
  # theme(legend.position = "none") +
  scale_fill_manual(values=c("#4F3558", "#40316B", "#196A91", "#04A58D")) +
  scale_y_continuous(labels = percent_format()) + 
  labs(title="Risk levels based on E.coli in the UNICEF wells", x ="", y = "Percentage", fill="E.coli risk levels") +
  theme(plot.title = element_text("Roboto-Regular", size=10),
        axis.title.x  = element_text(family = "Assistant-Regular", size=8),
        axis.text.x  = element_text(family = "Assistant-Regular", size=8),
        axis.text.y  = element_blank(),
        legend.text = element_text("Assistant-Regular", size=8),
        legend.title = element_text("Assistant-Regular", size=10)) +
  guides(fill = guide_legend(reverse = TRUE))

# MPN per 100 ml

household <- household %>%
  mutate(mpn.100ml = as.numeric(as.character(mpn.100ml)))

g <- ggplot(household, aes(mpn.100ml)) 

density_plot <- g + geom_density(aes(fill=factor(drinking_water_jmp)), alpha=0.8) +
  scale_fill_manual(values=c("#4F3558", "#40316B", "#28397E", "#196A91", "#04A58D")) +
  labs(y =" ", x = "MPN per 100 ml", fill=" ", title="The level of MPN per 100 ml of water relative to the service level of the water") +
  theme_minimal() +
  guides(colour = guide_legend(override.aes = list(size=2)))

violin_plot <- g + geom_violin(aes(fill=drinking_water_jmp)) + 
  geom_jitter(size=0.2, color="black") +
  scale_fill_manual(values=c("#4F3558", "#40316B", "#28397E", "#196A91", "#04A58D")) +
  labs(x =" ", fill=" ", title="Drinking water service level") +
  theme_minimal()

boxplot <- g + geom_boxplot(varwidth=T, fill="plum") + 
  labs(title="Box plot", 
       subtitle="City Mileage grouped by Class of vehicle",
       caption="Source: mpg",
       x="Class of Vehicle",
       y="City Mileage")

# Behavioral scoring

# Additional factors -> After..? sig. indication/correlation

# --- wells ---

# Improved versus unimproved map
waterpoints <- waterpoints %>%
  separate(water_supply_type, c("water_supply_type", "pump_type"), sep = "-|with") 

waterpoints_types <- ggplot(sl.shape.data) +
  geom_sf(size = 0.3, color = "#808080") +
  geom_point(data=waterpoints, aes(x=geo_longitude, y=geo_latitude, 
                                   fill = water_supply_type),
             size = 3, alpha=0.8, shape=21, color="#f2f2f2") +
  scale_fill_manual(values=c("#4F3558", "#40316B", "#196A91", "#04A58D","#228B22", "#FF4500")) + 
  theme(panel.grid.major = element_line("white")) +
  theme_minimal() +
  guides(colour = guide_legend(override.aes = list(size=2))) +
  labs(title="Different types of UNICEF wells present in Sierra Leone", x="", y="", fill="Types of well") +
  theme(legend.title = element_blank(),
        legend.text = element_text("Assistant-Regular", size=8),
        plot.title = element_text("Roboto-Regular", size=12),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())

# Water quality measures
boundries <- read_excel(here("Data", "Water Quality Parameter Information.xlsx"), 
                        sheet="Drinking Water Quality") %>% 
  select("Parameter", "WHO Guideline for drinking water") %>%
  mutate(Parameter = tolower(Parameter))
  
connectivity <- waterpoints %>% 
  mutate("ph" = ifelse(is.na(pH_low), pH_high, pH_low)) %>%
  select( "ph", "ammonia", "fluoride",
          "iron", "phosphate", 
          "nitrite", "potassium", 
          "nitrate", "chloride", 
          "electrical_conductivity") %>%
  gather() %>%
  rename("Parameter" = "key",
         "Observation" = "value") %>%
  mutate(Parameter = replace(Parameter, Parameter == "electrical_conductivity", "electrical conductivity")) %>%
  left_join(boundries)

caddisfly_table <- connectivity %>% 
  group_by(Parameter, `WHO Guideline for drinking water`) %>%
  summarize(`Waterpoints min (mg/l)` = min(Observation, na.rm=TRUE),
            `Waterpoints mean (mg/l)` = mean(Observation, na.rm=TRUE),
            `Waterpoints max (mg/l)` = max(Observation, na.rm=TRUE))

# PH
PH <- ggplot(connectivity %>% filter(Parameter == "ph"), aes(x=Parameter, y=Observation, color=Parameter)) +
  geom_boxplot(alpha = 0, color="black") +
  geom_jitter(alpha =0.5) +
  scale_color_manual(values ="#196A91") +
  geom_hline(aes(yintercept=7, linetype="pH neutral"), color = "#04A58D", size=0.5) +
  annotate("text",x=1.5, y = 7.1, label = "pH neutral", font = "Assistant-Regular", size=3) +
  scale_linetype_manual(name = "", values = 2) +
  theme(panel.grid.major = element_line("white")) +
  theme_minimal() +
  # guides(colour = guide_legend(override.aes = list(size=2))) +
  labs(title="Caddisfly water quality measurements: pH", x="", y="pH", fill="") +
  theme(legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text("Assistant-Regular", size=8),
        plot.title = element_text("Roboto-Regular", size=10),
        axis.text.x = element_blank(),
        axis.ticks = element_blank())

ammonia <- ggplot(connectivity %>% filter(Parameter == "ammonia"), aes(x=Parameter, y=Observation, color=Parameter)) +
  geom_boxplot(alpha = 0, color="black") +
  geom_jitter(alpha =0.5) +
  scale_color_manual(values ="#4F3558") +
  geom_hline(aes(yintercept=1.5, linetype="WHO guideline maximum ammonia mg/l (odeur)"), color = "#04A58D", size=0.5) +
  scale_linetype_manual(name = "", values = 2) +
  theme(panel.grid.major = element_line("white")) +
  theme_minimal() +
  # guides(colour = guide_legend(override.aes = list(size=2))) +
  labs(title="Caddisfly water quality measurements: Ammonia", x="", y="mg/l", fill="") +
  theme(legend.title = element_blank(),
        legend.text = element_text("Assistant-Regular", size=8),
        plot.title = element_text("Roboto-Regular", size=12),
        axis.text.x = element_blank(),
        axis.ticks = element_blank())

fluoride <- ggplot(connectivity %>% filter(Parameter == "fluoride"), aes(x=Parameter, y=Observation, color=Parameter)) +
  geom_boxplot(alpha = 0, color="black") +
  geom_jitter(alpha =0.5) +
  scale_color_manual(values ="#4F3558") +
  geom_hline(aes(yintercept=1.5, linetype="WHO guideline maximum fluoride mg/l"), color = "#04A58D", size=0.5) +
  annotate("text",x=1.3, y = 1.55, label = "WHO guideline maximum fluoride mg/l", font = "Assistant-Regular", size=3) +
  scale_linetype_manual(name = "", values = 2) +
  theme(panel.grid.major = element_line("white")) +
  theme_minimal() +
  # guides(colour = guide_legend(override.aes = list(size=2))) +
  labs(title="Caddisfly water quality measurements: Fluoride", x="", y="mg/l", fill="") +
  theme(legend.title = element_blank(),
        legend.position="none",
        legend.text = element_text("Assistant-Regular", size=8),
        plot.title = element_text("Roboto-Regular", size=12),
        axis.text.x = element_blank(),
        axis.ticks = element_blank())


