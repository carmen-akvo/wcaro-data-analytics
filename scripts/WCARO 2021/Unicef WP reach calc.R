# WCARO 2021 

# libraries
library(here)
library(readxl)
library(data.table)
library(spatialrisk)
library(sf)
library(tidyverse)
library(ggplot2)
library(reshape2)
library(splitstackshape)


### DATA

# 2016 national wp mapping
wp_2016 <- read_excel(here("SL/Data","wp_data_2016_national_mapping.xlsx"))
wp_2016 <- wp_2016 %>% 
  # mutate(district = str_to_title(district)) %>%
  # mutate(chiefdom = str_to_title(chiefdom)) %>%
  # mutate(section = str_to_title(section)) %>%
  # mutate(community = str_to_title(community)) %>%
  # mutate(water_point_name = str_to_title(water_point_name))
  mutate(Longitude = as.numeric(Longitude)) %>%
  mutate(Latitude = as.numeric(Latitude)) %>%
  filter(Latitude < 20) %>%
  filter(Longitude < -5) %>%
  filter(Longitude > -20) %>%
  unique()

wp_2016 <- wp_2016 %>%
  mutate(Partner = ifelse(`6400047|Installer / implementing agency` == "UNICEF", "UNICEF", "Other")) %>%
  mutate(Partner = ifelse(is.na(Partner), "Other", Partner)) %>%
  separate(`7430035|Water point Functionality`, c("Functionality score", "Functionality"), sep=":") %>%
  separate(`Type of water point`, c("Type of water point score", "Type of water point"), sep=":") %>%
  mutate(`Type of water point` = tolower(`Type of water point`)) %>%
  mutate(sdg_improved_source = recode(
    `Type of water point`, 
    "unprotected dug well" = "Unimproved", 
    "piped water to yard/plot" = "Improved", 
    "protected dug well" = "Improved", 
    "unprotected spring" = "Unimproved", 
    "rainwater collection" = "Improved",
    "protected spring" = "Improved", 
    "piped water into dwelling" = "Improved", 
    "tubewell/borehole" = "Unimproved", 
    "public tap/standpipe" = "Improved", 
    "surface water" = "surface water",
    "surface water (lake/river/stream)" = "surface water",
    "sand/sub-surface dam (with well or standpipe)" = "Improved", 
    "piped water into dwelling/plot/yard"  = "Improved",
    "tube well or borehole" = "Unimproved", 
    "public tap/standpipe (stand-alone or water kiosk" = "Improved",
    "unequipped borehole" = "Unimproved", 
    "hand pump" = "Improved", 
    "rainwater (harvesting)" = "Improved",
    "handpump" = "Improved")) %>%
  rename(ADM2 = District) %>% 
  mutate(ADM2 = recode(ADM2, 
                       "East" = "Eastern Region",
                       "West" = "Western Region",
                       "North" = "Northern Region",
                       "South" = "Southern Region"))

# Classification of WP reach
sphere_wp_reach <- data.frame(
  "sphere_nr_people" = c(250, 500, 400, 1000 ),
  "sphere_wp_type" = c("tap", "hand pump", "open hand well", "mechanized well"),
  "sphere_cat" = c("A","B","C","D"))

sphere_mapping <- read.table(here("SL/Data","sphere_category_mapping.csv"), sep=",", header=TRUE) %>% 
  left_join(sphere_wp_reach) %>%
  rename("Type of water point" = Type.of.water.point) %>%
  mutate(`Type of water point` = tolower(`Type of water point`))

# Shape file data
sl.shape <-read_sf(dsn = here::here("SL/Data/SIL_admin_SHP/", "SIL.shp"))
sl.shape.data <- ggplot2::fortify(sl.shape, region='NAME')

# Implementing partner
ggplot(sl.shape.data) +
  geom_sf(size = 0.1, color = "#603f83ff", fill="#c7d3d4ff") +
  theme_minimal() +
  geom_point(data=wp_2016, 
             aes(x=Longitude, y=Latitude, color=Partner), 
             size = 2, alpha=1, shape=20) +
  scale_color_manual(values=c("#404898", "#E04D95")) +
  theme(panel.grid.major = element_line("white")) +
  labs(title="2016 Waterpoint mapping SL", x="", y="") +
  theme(legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text("Assistant-Regular", size=8),
        plot.title = element_text("Roboto-Regular", size=8),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())

# Functionality
ggplot(sl.shape.data) +
  geom_sf(size = 0.1, color = "#603f83ff", fill="#c7d3d4ff") +
  theme_minimal() +
  geom_point(data=wp_2016, 
             aes(x=Longitude, y=Latitude, color=Functionality), 
             size = 2, alpha=1, shape=20) +
  # scale_color_manual(values=c("#404898", "#E04D95")) +
  theme(panel.grid.major = element_line("white")) +
  labs(title="2016 Waterpoint mapping SL", x="", y="") +
  theme(legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text("Assistant-Regular", size=8),
        plot.title = element_text("Roboto-Regular", size=8),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())

### REACH
wp_2016 <- wp_2016 %>% 
  left_join(sphere_mapping)

# Determine reach per district for all water points
reach_2016_sphere_district <- wp_2016 %>% 
  group_by(ADM2) %>% 
  summarise(reach = sum(sphere_nr_people, na.rm=TRUE))

# Determine reach per district for all functional water points
reach_2016_sphere_functional <- wp_2016 %>% 
  filter(Functionality %in% c("Yes – Functional (and in use)", "Yes - But damaged" )) %>% 
  group_by(ADM2) %>% 
  summarise(reach_functional_wp = sum(sphere_nr_people, na.rm=TRUE))

reach_2016_sphere_district <- reach_2016_sphere_district %>%
  left_join(reach_2016_sphere_functional)

# Add to shape file
sl.shape.data.2016 <- sl.shape.data %>% 
  left_join(reach_2016_sphere_district) %>%
  mutate(reach = replace_na(reach, 0)) %>%
  mutate(reach_functional_wp = replace_na(reach_functional_wp, 0))

# Remove redundant lines
sl.shape.data.2016.reach <- unique(
  sl.shape.data.2016[,c("ADM2", "geometry", "reach", "reach_functional_wp")]) %>%
  group_by(ADM2) %>%
  summarise(reach = unique(reach),
            reach_functional_wp = unique(reach_functional_wp))

# Create choropleth map with reach numbers for 2016 data
ggplot(sl.shape.data.2016.reach) +
  
  #add a shapefile layer to the plot, set the line size and colour for the polygons, then fill them according to our data
  geom_sf(size = 0.3, color = "#808080", aes(fill=reach)) +
  geom_sf_text(aes(label=paste0(ADM2, "\n", reach)), check_overlap = TRUE) +
  scale_fill_gradient(low="#F2F2F2",high="#404898", na.value="#F2F2F2")

## 2018 WQ mapping

# Unicef WQ mapping
wp_uni_2018 <- read.csv2(here("SL/output","SL_wells.csv")) %>%
  rename_all(funs(stringr::str_replace_all(., '\\.', '_')))

wp_uni_2018 <- wp_uni_2018 %>%
  mutate(province = str_to_title(province)) %>%
  mutate(district = str_to_title(district)) %>%
  mutate(chiefdom = str_to_title(chiefdom)) %>%
  mutate(community = str_to_title(community)) %>%
  mutate(water_point_name = str_to_title(water_point_name)) %>%
  mutate(geo_longitude = as.numeric(geo_longitude)) %>%
  mutate(geo_latitude = as.numeric(geo_latitude)) %>%
  rename("Type of water point" = water_supply_type) %>% 
  mutate(`Type of water point` = tolower(`Type of water point`)) %>%
  left_join(sphere_mapping) %>%
  rename(ADM2 = district)

# Determine reach per admin level 2:
reach_2018_sphere <- wp_uni_2018 %>% 
  group_by(ADM2) %>% 
  summarise(reach = sum(sphere_nr_people, na.rm=TRUE))

# Add to shape file
sl.shape.data.2018 <- sl.shape.data %>% 
  left_join(reach_2018_sphere) %>%
  mutate(reach = replace_na(reach, 0))

# Remove redundant lines
sl.shape.data.2018.reach <- unique(
  sl.shape.data.2018[,c("ADM2", "geometry", "reach")]) %>%
  group_by(ADM2) %>%
  summarise(reach = unique(reach))

# Create choropleth map with reach numbers for 2016 data
ggplot(sl.shape.data.2018.reach) +
  
  # add a shapefile layer to the plot, set the line size and colour for the polygons, then fill them according to our data
  geom_sf(size = 0.3, color = "#808080", aes(fill=reach)) +
  geom_sf_text(aes(label=paste0(ADM2, "\n", reach)), check_overlap = TRUE) +
  scale_fill_gradient(low="#F2F2F2",high="#404898", na.value="#F2F2F2")


# Example points within circle
wp_2016_test <- wp_2016 %>% 
  # filter(District == "Bo") %>%
  select("Submission Date","ADM2",                                                                           
         "Chiefdom","Section","Community","Water point Name",
         "Community Name","Latitude","Longitude",
         "Type of water point score","Type of water point",
         "Extraction system type","Pump type",
         "Number of taps at this point", 
         "6400047|Installer / implementing agency",
         "6430041|Others Installer / implementing agency",
         "Functionality score","Functionality")

# UNICEF WP
waterpoints_within_distance <- NULL

for(i in 1:dim(wp_uni_2018)[1]){

  # 2018 water point
  point <- wp_uni_2018[i,]
  
  # dataframe 2016 water points within distance
  df <- spatialrisk::points_in_circle(wp_2016_test, 
                                      point$geo_longitude, 
                                      point$geo_latitude, 
                                radius = 10, 
                                lon = Longitude, lat= Latitude)
  # Add ID of 2018 water point to data frame
  df$wp_2018 <- point$identifier
  df$implementing_partner_2018 <- point$implementing_partner
  df$risk_assessment_2018 <- point$risk_assessment
  df$ecoli_risk_score_2018 <- point$ecoli_risk_indication
  
  waterpoints_within_distance <- rbind(waterpoints_within_distance, df)
  
}

# Determine reach 2018
wp_uni_2018 <- wp_uni_2018 %>% 
  left_join(waterpoints_within_distance %>% 
              select(wp_2018, Functionality, distance_m), 
            by=c("identifier"="wp_2018")) 

# Reach of "new" water points in 2018
reach_2018_new_wp <- wp_uni_2018 %>%
  filter(is.na(distance_m))  %>% 
  group_by(ADM2) %>% 
  summarise(added_reach_2018 = sum(sphere_nr_people, na.rm=TRUE)) 

reach_2016_sphere_district %>%
  rename(reach_2016 = reach) %>% 
  left_join(reach_2018_new_wp) #%>% write.table(here("SL/output", "Coverage_2018_test2.csv"))

# Reach of "new" and possibly rehabilitated water points in 2018
reach_2018_new_and_broken_wp <- wp_uni_2018 %>%
  filter(!Functionality %like% "Yes")  %>% 
  group_by(ADM2) %>% 
  summarise(added_reach_2018 = sum(sphere_nr_people, na.rm=TRUE)) 

reach_2016_sphere_district %>%
  rename(reach_2016 = reach) %>% 
  left_join(reach_2018_new_and_broken_wp) #%>% write.table(here("SL/output", "Coverage_2018_test2.csv"))

# Population per district

# population projections of the government 
pop_projections_city <- read.table(here("SL/Data","pop_projections.csv"), sep=",", header=TRUE)
names(pop_projections_city) <- c("ADM2", "2016", "2018", "2020", "2022", "2024", "2026", "2028", "2030")

pop_projections <- pop_projections_city %>%
  mutate(across(c("2016":"2030"), ~ gsub(",", "", .))) %>%
  mutate(across(c("2016":"2030"), ~ as.numeric(.))) %>%
  mutate(City = ifelse(ADM2 %like% "City", "yes", "no")) %>%
  mutate(ADM2 = gsub(" District| City| Municipal", "", ADM2)) %>%
  group_by(ADM2) %>%
  summarise(across(c("2016":"2030"), ~ sum(.)))

percentage_reach_2016_sphere_district <- reach_2016_sphere_district %>% 
  mutate(ADM2 = recode(ADM2, 
                      "East" = "Eastern Region",
                      "West" = "Western Region",
                      "North" = "Northern Region",
                      "South" = "Southern Region")) %>% 
  full_join(pop_projections %>% select(ADM2, `2018`)) %>%
  mutate(Percentage_coverage = round(reach/`2018`,2)*100) %>%
  mutate(Percentage_coverage_functional_2016 = round(reach_functional_wp/`2018`,2)*100)

# Create choropleth map with reach numbers for 2016 data and reach

# Add to shape file
sl.shape.data.percentage.reach <- sl.shape.data %>% 
  left_join(percentage_reach_2016_sphere_district) %>%
  mutate(Percentage_coverage = replace_na(Percentage_coverage, 0))  %>%
  mutate(Percentage_coverage_functional_2016 = replace_na(Percentage_coverage_functional_2016, 0)) 

sl.shape.data.percentage.reach <- unique(
    sl.shape.data.percentage.reach[,c("ADM2", "geometry", 
                                      "Percentage_coverage", 
                                      "Percentage_coverage_functional_2016")]) %>%
  group_by(ADM2) %>%
  summarise(Percentage_coverage = unique(Percentage_coverage),
            Percentage_coverage_functional_2016 = unique(Percentage_coverage_functional_2016))

ggplot(sl.shape.data.percentage.reach) +
  
  #add a shapefile layer to the plot, set the line size and colour for the polygons, then fill them according to our data
  geom_sf(size = 0.3, color = "#808080", aes(fill=Percentage_coverage)) +
  geom_sf_text(aes(label=paste0(ADM2, "\n", Percentage_coverage, "%"))) + #, check_overlap = TRUE) +
  scale_fill_gradient(low="#F2F2F2",high="#404898", na.value="#F2F2F2")


ggplot(sl.shape.data.percentage.reach) +
  
  #add a shapefile layer to the plot, set the line size and colour for the polygons, then fill them according to our data
  geom_sf(size = 0.3, color = "#808080", aes(fill=Percentage_coverage_functional_2016)) +
  geom_sf_text(aes(label=paste0(ADM2, "\n", Percentage_coverage_functional_2016, "%"))) + #, check_overlap = TRUE) +
  scale_fill_gradient(low="#F2F2F2",high="#404898", na.value="#F2F2F2")


# ADD 2018 to population data
percentage_reach_2018_sphere_district <- percentage_reach_2016_sphere_district %>% 
  full_join(reach_2018_new_wp) %>% 
  mutate(Percentage_coverage_new_2018 = round((reach_functional_wp + added_reach_2018)/`2018`,2)*100) %>% 
  select(-added_reach_2018) %>%
  full_join(reach_2018_new_and_broken_wp) %>% 
  mutate(Percentage_coverage_new_and_broken_2018 = round((reach_functional_wp + added_reach_2018)/`2018`,2)*100)

sl.shape.data.percentage.reach <- sl.shape.data %>% 
  left_join(percentage_reach_2018_sphere_district) %>%
  mutate(Percentage_coverage_new_2018 = replace_na(Percentage_coverage_new_2018, 0))  %>%
  mutate(Percentage_coverage_new_and_broken_2018 = replace_na(Percentage_coverage_new_and_broken_2018, 0)) 

sl.shape.data.percentage.reach <- unique(
  sl.shape.data.percentage.reach[,c("ADM2", "geometry", "Percentage_coverage_new_2018", "Percentage_coverage_new_and_broken_2018")]) %>%
  group_by(ADM2) %>%
  summarise(Percentage_coverage_new_2018 = unique(Percentage_coverage_new_2018),
            Percentage_coverage_new_and_broken_2018 = unique(Percentage_coverage_new_and_broken_2018))

ggplot(sl.shape.data.percentage.reach) +
  
  #add a shapefile layer to the plot, set the line size and colour for the polygons, then fill them according to our data
  geom_sf(size = 0.3, color = "#808080", aes(fill=Percentage_coverage_new_2018)) +
  geom_sf_text(aes(label=paste0(ADM2, "\n", Percentage_coverage_new_2018, "%"))) + #, check_overlap = TRUE) +
  scale_fill_gradient(low="#F2F2F2",high="#404898", na.value="#F2F2F2")


# Show improved sources 2016 data
districtwise_improved_wp <- wp_2016 %>% 
  filter(sdg_improved_source %in% c("Unimproved", "Improved", "surface water")) %>% 
  mutate(sdg_improved_source = factor(sdg_improved_source, levels=c("Unimproved", "Improved", "surface water"))) %>%
  group_by(ADM2, sdg_improved_source) %>% 
  summarise(waterpoints = n(),
            sphere_nr_people_total = sum(sphere_nr_people, na.rm=TRUE),
            sphere_nr_people_average = mean(sphere_nr_people, na.rm=TRUE))

# districtwise_improved_wp <- districtwise_improved_wp %>%
#   mutate(percentage_wp = round(waterpoints / sum(waterpoints),2)*100) %>%
#   mutate(percentage_people = round(sphere_nr_people / sum(sphere_nr_people),2)*100)

total_improved_wp <- districtwise_improved_wp %>% 
  group_by(sdg_improved_source) %>%
  summarise(waterpoints = sum(waterpoints, na.rm=TRUE),
            sphere_nr_people_total = sum(sphere_nr_people_total, na.rm=TRUE),
            sphere_nr_people_average = round(mean(sphere_nr_people_average, na.rm=TRUE),2)) %>%
  mutate(percentage_wp = round(waterpoints / sum(waterpoints),2)*100) %>%
  mutate(percentage_people = round(sphere_nr_people_total / sum(sphere_nr_people_total),2)*100)

# improved versus unimproved water points
total_improved_wp %>%
  mutate(percentage_people = round(sphere_nr_people / sum(sphere_nr_people),2)*100) %>%
  ggplot(aes(x=sdg_improved_source, y=percentage_wp, fill = sdg_improved_source, label=paste0(percentage_wp, "% = ", waterpoints))) + 
  geom_col() + theme_light() +
  geom_text(data = total_improved_wp %>% filter(sdg_improved_source == "Unimproved"),
            nudge_x = -0.1, nudge_y = 5, family="Assistant-Regular", size=4) +
  geom_text(data = total_improved_wp %>% filter(sdg_improved_source == "Improved"),
            nudge_x = 0, nudge_y = 5, family="Assistant-Regular", size=4) +
  geom_text(data = total_improved_wp %>% filter(sdg_improved_source == "surface water"),
            nudge_x = 0.1, nudge_y = 5, family="Assistant-Regular", size=4) + 
  theme(legend.position = "None",
        axis.title.x = element_blank(),
        axis.text.x = element_text(family="Assistant-Regular", size=12))

# People served by improved versus unimproved water points
total_improved_wp %>%
  ggplot(aes(x=sdg_improved_source, y=percentage_people, fill = sdg_improved_source, label=paste0(percentage_people, "% = ", sphere_nr_people))) + 
  geom_col() + theme_light() +
  geom_text(data = total_improved_wp %>% filter(sdg_improved_source == "Unimproved"),
            nudge_x = -0.1, nudge_y = 5, family="Assistant-Regular", size=4) +
  geom_text(data = total_improved_wp %>% filter(sdg_improved_source == "Improved"),
            nudge_x = 0, nudge_y = 5, family="Assistant-Regular", size=4) +
  geom_text(data = total_improved_wp %>% filter(sdg_improved_source == "surface water"),
            nudge_x = 0.1, nudge_y = 5, family="Assistant-Regular", size=4) + 
  theme(legend.position = "None",
        axis.title.x = element_blank(),
        axis.text.x = element_text(family="Assistant-Regular", size=12))

# water point coverage - average
total_improved_wp %>%
  filter(!is.na(sphere_nr_people_average)) %>%
  ggplot(aes(x=sdg_improved_source, y=sphere_nr_people_average, fill = sdg_improved_source, label=paste0(sphere_nr_people_average))) + 
  geom_col() + theme_light() +
  geom_text(data = total_improved_wp %>% filter(sdg_improved_source == "Unimproved"),
            nudge_x = -0.05, nudge_y = 15, family="Assistant-Regular", size=4) +
  geom_text(data = total_improved_wp %>% filter(sdg_improved_source == "Improved"),
            nudge_x = 0, nudge_y = 15, family="Assistant-Regular", size=4) +
  theme(legend.position = "None",
        axis.title.x = element_blank(),
        axis.text.x = element_text(family="Assistant-Regular", size=12))

# Water points per district
districtwise_improved_wp %>%
  filter(sdg_improved_source != "surface water") %>%
  filter(!is.na(ADM2)) %>%
  ggplot(aes(fill=sdg_improved_source, y=waterpoints, x = ADM2, label=paste0(waterpoints))) + 
  geom_col(position = "dodge") + theme_light() + 
  coord_flip() + 
  theme(axis.text.x = element_text(angle=45))

# people per district
districtwise_improved_wp %>%
  filter(sdg_improved_source != "surface water") %>%
  filter(!is.na(ADM2)) %>%
  ggplot(aes(fill=sdg_improved_source, y=sphere_nr_people_total, x = ADM2, label=paste0(sphere_nr_people_total))) + 
  geom_col(position = "dodge") + theme_light() + 
  coord_flip() + 
  theme(axis.text.x = element_text(angle=45))

# People served by improved water points compared to the population
districtwise_improved_wp %>%
  left_join(pop_projections %>% select(ADM2, `2018`)) %>%
  mutate(coverage_wp_type = round(sphere_nr_people_total / `2018`,2)*100)

# Water point functionality of improved water points
# Show improved sources 2016 data
districtwise_wp_functionality <- wp_2016 %>% 
  filter(sdg_improved_source %in% c("Unimproved", "Improved", "surface water")) %>% 
  mutate(sdg_improved_source = factor(sdg_improved_source, levels=c("Unimproved", "Improved", "surface water"))) %>%
  group_by(ADM2, sdg_improved_source, Functionality) %>% 
  summarise(waterpoints = n(),
            sphere_nr_people_total = sum(sphere_nr_people, na.rm=TRUE),
            sphere_nr_people_average = mean(sphere_nr_people, na.rm=TRUE))

districtwise_improved_wp_functionality <- districtwise_wp_functionality %>%
  filter(sdg_improved_source == "Improved") %>% 
  filter(!is.na(ADM2)) %>%
  mutate(Functionality = factor(Functionality, 
                                levels = c("No - Broken down",
                                           "No - Still under construction",
                                           "No - Under rehabilitation",
                                           "Yes – Functional (but not in use)",
                                           "Yes - But damaged",
                                           "Yes – Functional (and in use)")))

districtwise_improved_wp_functionality %>%
  group_by(Functionality) %>%
  summarise(waterpoints = sum(waterpoints)) %>%
  ggplot(aes(fill=Functionality, y=waterpoints, x = Functionality, label=paste0(waterpoints))) + 
  geom_col(position = "dodge") + theme_light() + 
  # coord_flip() + 
  # theme(legend.position = element_blank()) +
  scale_x_discrete(labels=c("No\nBroken down",
                            "No\nStill under\nconstruction",
                            "No\nUnder\nrehabilitation",
                            "Yes\nFunctional\n(but not in use)",
                            "Yes\nBut damaged",
                            "Yes\nFunctional\n(and in use)" ))

# Functionality per District 


districtwise_improved_wp_functionality_percentage <- districtwise_improved_wp_functionality %>% 
  mutate(Functionality_simple = recode(Functionality,
                                       "No - Broken down" = "No",
                                       "No - Still under construction" = "No",
                                       "No - Under rehabilitation" = "No",
                                       "Yes – Functional (but not in use)" = "Yes",
                                       "Yes - But damaged" = "Yes",
                                       "Yes – Functional (and in use)" = "Yes")) %>%
  group_by(ADM2, sdg_improved_source, Functionality_simple) %>%
  summarise(waterpoints = sum(waterpoints),
            sphere_nr_people_total = sum(sphere_nr_people_total)) %>% 
  left_join(districtwise_wp_functionality %>% 
              group_by(ADM2) %>%
              summarise(total_waterpoints = sum(waterpoints),
                        sphere_nr_people_total_per_district = sum(sphere_nr_people_total)) %>%
              full_join(pop_projections %>% select(ADM2, `2018`))) %>%
  mutate(waterpoint_percentage = round(waterpoints/total_waterpoints, 2)*100) %>%
  mutate(sphere_coverage_percentage = round(sphere_nr_people_total/`2018`, 2)*100)

# Percentage of water points
districtwise_improved_wp_functionality_percentage %>%
  ggplot(aes(fill=Functionality_simple, y=waterpoint_percentage, x = ADM2, label=paste0(waterpoint_percentage, "%"))) + 
  geom_col() + theme_light() + 
  coord_flip() +
  geom_text(data = districtwise_improved_wp_functionality_percentage, 
            position="stack", check_overlap = TRUE)

# Percentage of people
districtwise_improved_wp_functionality_percentage %>%
  ggplot(aes(fill=Functionality_simple, y=sphere_coverage_percentage, x = ADM2, label=paste0(sphere_coverage_percentage, "%"))) + 
  geom_col() + theme_light() + 
  coord_flip() +
  geom_text(data = districtwise_improved_wp_functionality_percentage, 
            position="stack", check_overlap = TRUE)

# Water quality - no water quality measurement, but parameters

# Seasonality / reliability
wp_2016 %>% 
  separate(`9410052|How reliable is the water point?`, c("Reliability score", "Reliability"), sep=":") %>%
  select(sphere_wp_type, Reliability)  %>%
  mutate(Reliability = factor(Reliability, levels=c("Good, practically always",
                                                    "Reasonable (< 7 days per month no supply)",
                                                    "Insufficient (> 7 days per month no supply)"))) %>% 
  table() %>% melt() %>% group_by(sphere_wp_type) %>%
  mutate(percentage = round(value/sum(value), 2)*100) %>%
  ggplot(aes(fill=Reliability, y=percentage, x = sphere_wp_type, label=paste0(percentage))) + 
  geom_col(position = "dodge") + theme_light() + #coord_flip() +
  scale_x_discrete(labels=c( "hand pump",
                             "mechanized\nwell",
                             "open hand\nwell",
                             "tap")) + 
  scale_fill_manual(values = c( "#99d594","#ffffbf", "#fc8d59"))







## Household level improved versus unimproved WP
hh_service_levels <- read.csv2(here::here("SL/output","household_with_service_levels.csv"), sep=";")

district_service_level <- hh_service_levels %>% 
  group_by(location_district, sdg_improved_source) %>%
  summarise(households = n(),
            nr_of_people = sum(number_household, na.rm = TRUE))

