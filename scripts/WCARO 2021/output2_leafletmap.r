# Leaflet map

library(here)
library(dplyr)
library(rgdal)
library(leaflet)

library(htmlwidgets)
library(htmltools)

# Household
hh_bonthe <- hh_bonthe %>% 
  mutate(
    drinking_water_jmp = 
      ifelse(
        wp_status == "Improved" & 
          round_trip_minutes == "Water on premises",
        "Safely managed", 
        ifelse(
          wp_status == "Improved" & 
            round_trip_minutes == "Less than 30 minutes",
          "Basic", 
          ifelse(
            wp_status == "Improved" & 
              !round_trip_minutes  %in% c("Water on premises","Less than 30 minutes"),
            "Limited", 
            ifelse(
              wp_status == "Unimproved",
              "Unimproved", "No service"))))) %>% 
  mutate(sdg_hand_washing =  recode(
    hand_wash_observe,
    "Yes" = "Basic/Limited",
    "No" = "No facility"
  )) %>%
  mutate(toilet_facility_type = tolower(toilet_facility_type)) %>%
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
  )) %>%
  mutate(improved_functional = paste0())
  filter(!is.na(drinking_water_jmp)) %>%
  mutate(drinking_water_jmp = factor(drinking_water_jmp,
                                     levels=c( "Safely managed", "Basic","Limited", "Unimproved")))

# Shapefile
sierra <- readOGR(
  dsn= here("data/raw/sle_ocha_admin_1_4/") ,
  # layer="SIL",
  verbose=FALSE
)

# Water point data
wp_bonthe <- wp_bonthe %>%
  mutate(sdg_improved_source_code = recode(sdg_improved_source,
                                           improved = 2,
                                           unimproved = 1)) %>%
  mutate(status_impr_funct = paste0(sdg_improved_source, " - ", Functionality)) %>%
  mutate(status_impr_funct_code = recode(status_impr_funct,
                                         "unimproved - Not functional" = 1,
                                         "unimproved - Functional and in use" = 2,
                                         "improved - Not functional"= 3,
                                         "improved - Functional and in use" = 4 
                                         )) 


## Eerste map 
pal <- colorFactor(
  palette = colorRampPalette(c("#1EAFED", "#FFCD60"))(length(wp_bonthe$sdg_improved_source)),
  domain = wp_bonthe$sdg_improved_source)

pal_jmp <- colorFactor(
  palette = colorRampPalette(c("#033FFF", "#1EAFED", "#FFCD60", "#fa884f"))(length(hh_bonthe$drinking_water_jmp)),
  domain = hh_bonthe$drinking_water_jmp)

tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title { 
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 50%;
    text-align: center;
    padding-left: 5px; 
    padding-right: 5px; 
    background: rgba(255,255,255,0.75);
    font-weight: bold;
    font-size: 16px;
  }
"))

title_1 <- tags$div(
  tag.map.title, HTML("Bonthe water points in comparison to the population density")
)  

## Household drinking water level and water points 
leaflet() %>%
  addTiles(data=sierra) %>%
  setView( lat=7.5, lng=-12.5 , zoom=8.5) %>%
  addMarkers(data = wp_bonthe,
             lng = ~Longitude,
             lat = ~Latitude,
             icon=tap_icon,
             label = paste(
               "<b>Source:</b>",
               wp_bonthe$Type.of.water.point, "<br>",
               "<b>Coverage:</b>",
               wp_bonthe$sphere_nr_people, "<br>"
             ) %>%
               lapply(htmltools::HTML))  %>%
  addCircles(data=hh_bonthe,
             radius= 5,
             color= ~pal_jmp(drinking_water_jmp),
             lng = ~geolocation_Longitude,
             lat = ~geolocation_Latitude,
             label = paste( "<b>Household size:</b>",
                            hh_bonthe$number_household, "<br>",
                            "<b>Drinking water service level:</b>",
                          hh_bonthe$drinking_water_jmp)%>%
               lapply(htmltools::HTML)) %>%
  # add a legend
  addLegend(
    data = wp_bonthe,
    pal = pal,
    values = ~sdg_improved_source,
    position = "bottomleft",
    title = "Main water source:",
    opacity = 0.9
  ) %>%
  # add a legend
  addLegend(
    data = hh_bonthe,
    pal = pal_jmp,
    values = ~drinking_water_jmp,
    position = "bottomleft",
    title = "Drinking water service level:",
    opacity = 0.9
  ) %>%
  addControl(title_1, position = "topleft", className="map-title")

# Households + score

# Combine Water, Sanitation and Hygiene into a code

hh_bonthe <- hh_bonthe %>%
  mutate(drinking_water_jmp_code = recode(
    drinking_water_jmp,
    "Unimproved" = 1,
    "Basic" = 3,
    "Limited" = 2,
    "Safely managed" = 4)) %>%
  mutate(sdg_sanitation_code = recode(
    sdg_sanitation,
    "Improved" = 1,
    "Unimproved" = 2,
    "Open Defecation" = 3)) %>%
  mutate(sdg_hand_washing_code = recode(
    sdg_hand_washing,
    "No facility" =1,
    "Basic/Limited"=2)) %>%
  mutate(wash_score = drinking_water_jmp_code + sdg_sanitation_code + sdg_hand_washing_code)

water_jmp_chiefdom <- hh_bonthe %>% 
  group_by(location_Chiefdom, drinking_water_jmp) %>%
  summarise(count = n()) %>%
  group_by(location_Chiefdom) %>%
  filter(count == max(count)) %>%
  rename(admin3Name = location_Chiefdom) %>%
  select(-count)

sierra@data <- sierra@data %>% left_join(water_jmp_chiefdom)

pal_jmp <- colorFactor(
  palette = colorRampPalette(c("#033FFF", "#1EAFED", "#FFCD60", "#fa884f"))(length(sierra@data$drinking_water_jmp)),
  domain = sierra@data$drinking_water_jmp)

pal_wp <- colorFactor(
  palette = colorRampPalette(c("green", "orange", "red", "darkred"))(length(wp_bonthe$status_impr_funct)),
  domain = wp_bonthe$status_impr_funct)

title_2 <- tags$div(
  tag.map.title, HTML("Household water service level")
)  

getColor <- function(wp_bonthe) {
  sapply(wp_bonthe$status_impr_funct_code, function(status_impr_funct_code) {
    if(status_impr_funct_code == 4) {
      "green"
    } else if (status_impr_funct_code == 3) {
      "orange"
    }else if (status_impr_funct_code == 2) {
      "red"
    }else{
      "darkred"
    } })
}

icons <- awesomeIcons(
  icon = 'tint',
  iconColor = 'black',
  library = 'fa',
  markerColor = getColor(wp_bonthe)
)


leaflet(sierra) %>%
  addTiles() %>%
  setView( lat=7.5, lng=-12.5 , zoom=8.5) %>%
  addPolygons(
    fillColor = ~pal_jmp(sierra@data$drinking_water_jmp),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    label = paste("<b>Most Households' Drinking Water Service Level:</b>",
                   sierra@data$drinking_water_jmp, "<br>",
                  "<b>Chiefdom:</b>",
                  sierra@data$admin3Name, "<br>"
    )%>%
      lapply(htmltools::HTML)) %>%
  addAwesomeMarkers(data = wp_bonthe, 
                    ~Longitude,
                    ~Latitude, 
                    icon=icons,
                    label = paste(
                      "<b>Source:</b>",
                      wp_bonthe$sdg_improved_source, "<br>",
                      "<b>Coverage:</b>",
                      wp_bonthe$sphere_nr_people, "<br>",
                      "<b>Functionality:</b>",
                      wp_bonthe$Functionality, "<br>"
                    ) %>%
                      lapply(htmltools::HTML))  %>%
  # add a legend
  addLegend(
    data =  wp_bonthe,
    pal = pal_wp,
    values = ~status_impr_funct,
    position = "bottomleft",
    title = "Water Source Status:",
    opacity = 0.9
  ) %>%
  # add a legend
  addLegend(
    data =  sierra@data,
    pal = pal_jmp,
    values = ~drinking_water_jmp,
    position = "bottomleft",
    title = "Majority of Households Drinking water<br>service level per Chiefdom:",
    opacity = 0.9
  ) %>%
  addControl(title_2, position = "topleft", className="map-title")


# Facebook data

# Create a continuous palette function
pal_fb <- colorNumeric(c("#ffd9c7", "#ffc1a1","#ffa87c","#ff8f59","#ff7335","#ff5403"), 1:30)

leaflet() %>%
  addTiles(data=sierra) %>%
  setView( lat=7.5, lng=-12.5 , zoom=8.5) %>%
  addCircleMarkers(data=FB_density_bonthe,
             radius= ~Population/10,
             color= "#1E3163",
             lng = ~Lon,
             lat = ~Lat,
             label = paste( "<b>Population:</b>",
                            round(FB_density_bonthe$Population,0), "<br>"
             ) %>%
               lapply(htmltools::HTML))  %>%
  addMarkers(data = wp_bonthe,
             lng = ~Longitude,
             lat = ~Latitude,
             icon=tap_icon,
             label = paste(
               "<b>Source:</b>",
               wp_bonthe$Type.of.water.point, "<br>",
               "<b>Coverage:</b>",
               wp_bonthe$sphere_nr_people, "<br>"
             ) %>%
               lapply(htmltools::HTML)) %>%
  # add a legend
  addLegend(
    data = wp_bonthe,
    pal = pal,
    values = ~sdg_improved_source,
    position = "bottomleft",
    title = "Water source status:",
    opacity = 0.9
  ) %>%
  addLegend(
    data = FB_density_bonthe,
    pal = pal_fb,
    values = ~Population,
    position = "bottomleft",
    title = "Population density",
    opacity = 0.9
  ) 

### SCHOOLS
wp_bonthe_school <- wp_bonthe %>%
  filter(X4390044.Who.owns.the.water.point. %like% "5:School") %>%
  mutate(water_service_level = sample(c("No service", "Limited", "Basic", "Advanced"), 67, replace = TRUE)) %>%
  mutate(sanitation_service_level = sample(c("No service", "Limited", "Basic", "Safely Managed"), 67, replace = TRUE)) %>%
  mutate(hygiene_service_level = sample(c("No service", "Limited", "Basic"), 67, replace = TRUE))

pal_school_water <- colorFactor(
  palette = colorRampPalette(c("#033FFF", "#1EAFED", "#FFCD60", "#fa884f"))(length(wp_bonthe_school$water_service_level)),
  domain = wp_bonthe_school$water_service_level)

wp_bonthe_school <- wp_bonthe_school %>%
  mutate(water_service_level_code = recode(
    water_service_level,
    "Advanced" = 4,
    "Basic" = 3,
    "Limited" = 2,
    "No service" = 1)) %>%
  mutate(sanitation_service_level_code = recode(
    sanitation_service_level,
    "Safely Managed" = 4,
    "Basic" = 3,
    "Limited" = 2,
    "No service" = 1)) %>%
  mutate(hygiene_service_level_code = recode(
    hygiene_service_level,
    "Advanced" = 4,
    "Basic" = 3,
    "Limited" = 2,
    "No service" = 1)) %>%
  mutate(status_general_code = ifelse(
    water_service_level_code > 3 & sanitation_service_level_code > 3 & hygiene_service_level_code > 3, 4, 
    ifelse(water_service_level_code > 2 & sanitation_service_level_code > 2 & hygiene_service_level_code > 2, 3,
           ifelse(water_service_level_code > 1 & sanitation_service_level_code > 1 & hygiene_service_level_code > 1, 2, 1)
    )
  )
  ) %>%
  mutate(status_general = recode(
    status_general_code,
    "4" = "Advanced",
    "3" = "Basic",
    "2" = "Limited",
    "1" = "No service")) %>%
  mutate(status_general = factor(status_general, levels=c("Advanced",
                                                          "Basic",
                                                          "Limited",
                                                          "No service")))

getColor <- function(wp_bonthe_school) {
  sapply(wp_bonthe_school$status_general_code, function(status_general_code) {
    if(status_general_code == 4) {
      "darkgreen"
    } else if(status_general_code == 3) {
      "green"
    } else if(status_general_code == 2) {
      "red"
    } else {
      "darkred"
    } })
}

icons <- awesomeIcons(
  icon = 'graduation-cap',
  iconColor = 'black',
  library = 'fa',
  markerColor = getColor(wp_bonthe_school)
)

pal_school_general_status_2 <- 
  colorFactor(palette = c("darkgreen", "green", "#FF0000", "#8C0000"), 
              levels = c("Advanced", "Basic", "Limited","No service"))

leaflet(wp_bonthe_school) %>% addTiles() %>%
  addAwesomeMarkers(~Longitude,~Latitude, icon=icons, label= paste(
    "Water Service Level:",
    "<b>", wp_bonthe_school$water_service_level, "</b>", "<br>",
    "Sanitation Service Level:",
    "<b>",wp_bonthe_school$sanitation_service_level,"</b>", "<br>",
    "Hygiene Service Level:",
    "<b>",wp_bonthe_school$hygiene_service_level, "</b>", "<br>"
  ) %>%
    lapply(htmltools::HTML)) %>%
  # add a legend
  addLegend(
    data = wp_bonthe_school,
    pal = pal_school_general_status_2,
    values = c("Advanced", "Basic", "Limited","No service"),
    position = "bottomleft",
    title = "WaSH status of the schools:",
    opacity = 0.9
  ) 


### HEALTH FACILITIES

wp_bonthe_health <- wp_bonthe %>%
  filter(X4390044.Who.owns.the.water.point. %like% "6:Health Facility") %>%
  mutate(water_service_level = sample(c("No service", "Limited", "Basic", "Advanced"), 29, replace = TRUE)) %>%
  mutate(sanitation_service_level = sample(c("No service", "Limited", "Basic", "Safely Managed"), 29, replace = TRUE)) %>%
  mutate(hygiene_service_level = sample(c("No service", "Limited", "Basic"), 29, replace = TRUE))

wp_bonthe_health <- wp_bonthe_health %>%
  mutate(water_service_level_code = recode(
    water_service_level,
    "Advanced" = 4,
    "Basic" = 3,
    "Limited" = 2,
    "No service" = 1)) %>%
  mutate(sanitation_service_level_code = recode(
    sanitation_service_level,
    "Safely Managed" = 4,
    "Basic" = 3,
    "Limited" = 2,
    "No service" = 1)) %>%
  mutate(hygiene_service_level_code = recode(
    hygiene_service_level,
    "Advanced" = 4,
    "Basic" = 3,
    "Limited" = 2,
    "No service" = 1)) %>%
  mutate(status_general_code = ifelse(
    water_service_level_code > 3 & sanitation_service_level_code > 3 & hygiene_service_level_code > 3, 4, 
    ifelse(water_service_level_code > 2 & sanitation_service_level_code > 2 & hygiene_service_level_code > 2, 3,
           ifelse(water_service_level_code > 1 & sanitation_service_level_code > 1 & hygiene_service_level_code > 1, 2, 1)
    )
  )
  ) %>%
  mutate(status_general = recode(
    status_general_code,
    "4" = "Advanced",
    "3" = "Basic",
    "2" = "Limited",
    "1" = "No service")) %>%
  mutate(status_general = factor(status_general, levels=c("Advanced",
                                                          "Basic",
                                                          "Limited",
                                                          "No service")))

getColor <- function(wp_bonthe_health) {
  sapply(wp_bonthe_health$status_general_code, function(status_general_code) {
    if(status_general_code == 4) {
      "darkgreen"
    } else if(status_general_code == 3) {
      "green"
    } else if(status_general_code == 2) {
      "red"
    } else {
      "darkred"
    } })
}

icons <- awesomeIcons(
  icon = 'stethoscope',
  iconColor = 'black',
  library = 'fa',
  markerColor = getColor(wp_bonthe_health)
)

pal_health_general_status_2 <- 
  colorFactor(palette = c("darkgreen", "green", "#FF0000", "#8C0000"), 
              levels = c("Advanced", "Basic", "Limited","No service"))

leaflet(wp_bonthe_health) %>% addTiles() %>%
  addAwesomeMarkers(~Longitude,
                    ~Latitude, 
                    icon=icons, 
                    label= paste(
                      "Water Service Level:",
                      "<b>", wp_bonthe_health$water_service_level, "</b>", "<br>",
                      "Sanitation Service Level:",
                      "<b>",wp_bonthe_health$sanitation_service_level,"</b>", "<br>",
                      "Hygiene Service Level:",
                      "<b>",wp_bonthe_health$hygiene_service_level, "</b>", "<br>"
                    ) %>%
                      lapply(htmltools::HTML)) %>%
  # add a legend
  addLegend(
    data = wp_bonthe_health,
    pal = pal_health_general_status_2,
    values = c("Advanced", "Basic", "Limited","No service"),
    position = "bottomleft",
    title = "WaSH status of the health facilities:",
    opacity = 0.9
  ) 

## WATER ICONS
wp_bonthe <- wp_bonthe %>%
  mutate(sdg_improved_source_code = recode(sdg_improved_source,
                                           improved = 2,
                                           unimproved = 1))

getColor <- function(wp_bonthe) {
  sapply(wp_bonthe$sdg_improved_source_code, function(sdg_improved_source_code) {
    if(sdg_improved_source_code == 2) {
      "blue"
    } else {
      "orange"
    } })
}

icons <- awesomeIcons(
  icon = 'tint',
  iconColor = 'black',
  library = 'fa',
  markerColor = getColor(wp_bonthe)
)


leaflet() %>%
  addTiles(data=sierra) %>%
  setView( lat=7.5, lng=-12.5 , zoom=8.5) %>%
  addAwesomeMarkers(data = wp_bonthe, 
                    ~Longitude,
                    ~Latitude, 
                    icon=icons,
             label = paste(
               "<b>Source:</b>",
               wp_bonthe$sdg_improved_source, "<br>",
               "<b>Coverage:</b>",
               wp_bonthe$sphere_nr_people, "<br>",
               "<b>Functionality:</b>",
               wp_bonthe$Functionality, "<br>"
             ) %>%
               lapply(htmltools::HTML))   %>%
  # add a legend
  addLegend(
    data = wp_bonthe_health,
    pal =  colorFactor(palette = c("orange", "blue"), 
                       levels = c("unimproved", "improved")),
    values = c("unimproved", "improved"),
    position = "bottomleft",
    title = "Water source status:",
    opacity = 0.9
  ) 



leaflet() %>%
  addTiles(data=sierra) %>%
  setView( lat=7.5, lng=-12.5 , zoom=8.5) %>%
  addCircleMarkers(data=FB_density_bonthe,
                   radius= ~Population/10,
                   color= "#1E3163",
                   lng = ~Lon,
                   lat = ~Lat,
                   label = paste( "<b>Population:</b>",
                                  round(FB_density_bonthe$Population,0), "<br>"
                   ) %>%
                     lapply(htmltools::HTML))  %>%
  addAwesomeMarkers(data = wp_bonthe, 
                    ~Longitude,
                    ~Latitude, 
                    icon=icons,
                    label = paste(
                      "<b>Source:</b>",
                      wp_bonthe$sdg_improved_source, "<br>",
                      "<b>Coverage:</b>",
                      wp_bonthe$sphere_nr_people, "<br>",
                      "<b>Functionality:</b>",
                      wp_bonthe$Functionality, "<br>"
                    ) %>%
                      lapply(htmltools::HTML))   %>%
  # add a legend
  addLegend(
    data = wp_bonthe_health,
    pal =  colorFactor(palette = c("orange", "blue"), 
                       levels = c("unimproved", "improved")),
    values = c("unimproved", "improved"),
    position = "bottomleft",
    title = "Water source status:",
    opacity = 0.9
  ) 