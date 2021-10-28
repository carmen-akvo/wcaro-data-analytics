library("easypackages")
library(tidyverse)
libraries("tidyverse","inspectdf","dlookr","RColorBrewer","scales","reshape2","scales")
minimal <- theme_set(theme_minimal(base_size = 16))
minimal

# Function to Round off  ####
round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame 
  # digits: number of digits to round
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}

# Household ####
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

write.csv(household, "2020_Aug_Report/households.csv")
View(household)


## Drinking Water JPM ####

# Ordered Factor for the JMP Level
household$drinking_water_jmp <- factor(household$drinking_water_jmp, levels = c("No service","Unimproved","Limited","Basic","Safely managed"), ordered = T)

# Table for kableExtra 
hh_drinkingWater <- table(household$location_district, household$drinking_water_jmp,dnn=c("District", "JMP Drinking Water Ladder"))  
hh_drinkingWater <- round(prop.table(hh_drinkingWater, margin = 2)*100,3)
hh_drinkingWater <- addmargins(hh_drinkingWater)
hh_drinkingWater <- as.data.frame.matrix(hh_drinkingWater)

color_hex <- c("#195675","#008790","#974458","#f6b059","#f9f871")
# Plot for Drinking Water
household %>%
  group_by(location_district) %>%
  count(drinking_water_jmp) %>%
  mutate(prop = n/sum(n)) %>%
  mutate_if(is.numeric, round, digits=2) %>%
  ggplot(aes(x = location_district, y =  prop)) +
  geom_col(aes(fill = drinking_water_jmp), position = "fill") +
  labs(fill = "key", subtitle = "JMP Drinking Water Level in Sierra Leone") +
  theme(legend.position = "top", legend.title = element_blank()) +
  scale_y_continuous(labels = scales::percent) +
  geom_text(aes(label = scales::percent(prop),
                y = prop,
                group = drinking_water_jmp),
            position = position_fill(),
            vjust = 1.5) +
  labs(x="District", y="Percentage") + 
  scale_fill_manual(values =  color_hex)


### Sanitation ####
household$sanitation_jmp <- factor(household$sanitation_jmp, levels = c("Open Defecation","Unimproved","Limited","Basic","Safely managed"), ordered = T)
  
# Table for Sanitation Per District
hh_sanitation <- table(household$location_district, household$sanitation_jmp)
hh_sanitation <- round(prop.table(hh_sanitation, margin = 2)*100,1)
# hh_sanitation <- addmargins(hh_sanitation)
hh_sanitation <- as.data.frame.matrix(hh_sanitation)
hh_sanitation %>%
  rename(`Open Defecation (%)` == `Open Defecation`,
         `Unimproved (%)` ==`Unimproved`,
         `Unimproved (%)` ==`Limited`,
         `Basic (%)` ==`Safely managed`)
hh_sanitation

## GGplot of Sanitation Per District
household %>%
  filter(!is.na(sanitation_jmp)) %>%
  group_by(location_district) %>%
  count(sanitation_jmp) %>%
  mutate(prop = n/sum(n)) %>%
  mutate_if(is.numeric, round, digits=1) %>%
  ggplot(aes(x = location_district, y =  prop)) +
  geom_col(aes(fill = sanitation_jmp), position = "fill") +
  labs(fill = "key", subtitle = "JMP Drinking Water Level in Sierra Leone") +
  theme(legend.position = "top", legend.title = element_blank()) +
  scale_y_continuous(labels = scales::percent) +
  geom_text(aes(label = scales::percent(prop),
                y = prop,
                group = sanitation_jmp),
            position = position_fill(),
            vjust = 1.5) +
  labs(x="District", y="Percentage") + 
  scale_fill_manual(values =  color_hex)


## Hand Washing ####
household %>% 
  group_by(location_district) %>% 
  count(sdg_hand_washing) %>% 
  mutate(prop = n/sum(n)) %>% 
  ggplot(aes(x = location_district, y = prop)) +
  geom_col(aes(fill = sdg_hand_washing), position = "dodge") +
  # scale_fill_manual("legend", values = c("Yes" = "#00c739", "No" = "#ff6263")) +
  labs(fill = "key", subtitle = "") +
  theme(legend.position = "top")+
  geom_text(aes(label = scales::percent(prop),
                y = prop, 
                group = sdg_hand_washing),
            position = position_dodge(width = 0.9),
            vjust = 1.8) +
  labs(x="District", y="Percentage") + scale_y_continuous(labels = scales::percent_format())


household %>%
  group_by(location_district) %>%
  count(sdg_hand_washing) %>%
  mutate(prop = n/sum(n)) %>%
  mutate_if(is.numeric, round, digits=2) %>%
  ggplot(aes(x = location_district, y =  prop)) +
  geom_col(aes(fill = sdg_hand_washing), position = "fill") +
  labs(fill = "key", subtitle = "") +
  theme(legend.position = "top", legend.title = element_blank(), axis.text = element_text(size = 25), legend.text = element_text(size = 27)) +
  scale_y_continuous(labels = scales::percent) +
  geom_text(aes(label = scales::percent(prop),
                y = prop,
                group = sdg_hand_washing),
            position = position_fill(), size=8,
            vjust = 1.5) +
  labs(x="", y="") + 
  scale_fill_manual(values =  color_hex)




