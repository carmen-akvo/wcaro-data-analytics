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

# leaflet
library(rjson)
library(jsonlite)
library(leaflet)=-0
library(RCurl)


# Read processed data files of the wells
waterpoints <- read.csv2(here("output","SL_wells.csv")) %>%
  rename_all(
    funs(
        stringr::str_replace_all(., '\\.', '_')
    )
  )


# SURVEY ADJUSTMENTS

# Read file with survey structure
survey <- read_excel(
  here::here("surveys", "water facility assessment form.xls"),
  sheet ="Full Survey", skip=2)

survey_questions <- survey %>%
  # Select right columns
  select("Text","Dependent", "Question", "Answer(s)", 
         "Question type", "Options",
         "Allow multiple") %>%
  # Trim trailing spaces
  mutate_if(is.character, str_trim) %>%
  # Rename columns
  rename("question" = "Text",
         "dependent" = "Dependent",
         "dep question" = "Question",
         "dep answer" = "Answer(s)",
         "type" = "Question type", 
         "options" = "Options",
         "multiple" = "Allow multiple") %>%
  # All variables to lowercase
  mutate_all(tolower)

waterpoints <- waterpoints %>% 
  separate(`ecoli_risk_indication`, c("ecoli_risk_indication", "RL probability"), sep = " / ") %>%
  mutate(
    ecoli_risk_indication = factor(
      ecoli_risk_indication,
      levels=c("Very High Risk", "High Risk", "Intermediate Risk", "Low Risk"))) %>%
  mutate(geo_latitude = as.numeric(as.character(geo_latitude)))  %>%
  mutate(geo_longitude = as.numeric(as.character(geo_longitude)))
  
## MAP

# Service levels map
funct_color <- c("Safely managed" = "#03AD8C",
                 "Basic" = "#05a58d", 
                 "Limited" = "#328284",
                 "Unimproved" = "#4f3558",
                 "No service" = "#E04D95")

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

ggplot(sl.shape.data) +
  geom_sf(size = 0.1, color = "#603f83ff", fill="#c7d3d4ff") +
  theme_minimal() +
  geom_point(data=waterpoints, 
             aes(x=geo_longitude, y=geo_latitude), 
             size = 3, alpha=0.8, shape=21, color="#03AD8C", fill="#f2f2f2") +
  geom_point(data=household, 
             aes(x=geolocation_longitude, y=geolocation_latitude), 
             size = 0.5, alpha = 0.5, color="#4f3558") +
  theme(panel.grid.major = element_line("white")) +
  # guides(colour = guide_legend(override.aes = list(size=2))) +
  labs(title="Households and wells presented in this survey", x="", y="") +
  theme(legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text("Assistant-Regular", size=8),
        plot.title = element_text("Roboto-Regular", size=8),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())


## ---- Hardware ----

water_type_col = c("#4D5262", "#C73E39","#5BCB93", "#D8D8D8", "#FFB55F", "#15607A")
pump_type_col = c("#15607A","#5BCB93", "#D8D8D8", "#FFD8BB")

# WP supply type
waterpoints <- waterpoints %>% 
  mutate(water_supply_type = as.character(water_supply_type)) %>%
  separate(water_supply_type, c("water_supply_type", "pump_type"), sep=" with | - ") %>%
  mutate(pump_type = str_replace(pump_type, "submersive", "submersible")) %>% 
  mutate(pump_type = replace_na(pump_type, "no pump")) %>%
  mutate(water_supply_type = factor(
    water_supply_type, 
    levels = c("Rehabilitated hand dug well",
               "New borehole", "New hand dug well", 
               "Rehabilitated borehole",
               "Tap stands from gravity fed water scheme",
               "Rehabilitate spring box"))) %>%
  mutate(pump_type = factor(
    pump_type, 
    levels = c("handpump","submersible pump","mechanised pump","no pump")))

levels(waterpoints$pump_type) <- c("Hand pump", "Submersible pump", "Mechinased pump", "No pump")

st_col <- colorRampPalette(c("#404898", "#E04D95"))

wp_supply_type <- ggplot(waterpoints) +
  geom_bar(mapping= aes(x=water_supply_type, fill=pump_type), colour="#D9D9D9", position = "dodge") +
  theme_minimal() +
  scale_fill_manual(values=pump_type_col) +
  labs(title="", x ="", y = "Amount of waterpoints", fill="") +
  theme(plot.title = element_text(family = "Assistant-Regular", size=14),
        axis.text.y = element_text(family = "Assistant-Regular", size=11, hjust=0), 
        axis.title.y = element_text(family = "Assistant-Regular", size=14), 
        axis.text.x = element_text(family = "Assistant-Regular", size=14),
        legend.text = element_text(family = "Assistant-Regular", size=14),
        legend.position = c(0.85, 0.5),
        legend.background = element_rect(fill="white", linetype = "blank")) + 
  # guides(fill = guide_legend(reverse = TRUE)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))

ecoli_col <- colorRampPalette(c("lightblue", "red"))

wp_supply_basic <- ggplot(waterpoints) +
  geom_bar(mapping= aes(x=water_supply_type, fill=`ecoli risk indication`), position = position_dodge(), colour="white") +
  theme_minimal() +
  scale_fill_manual(values=ecoli_col(6)) +
  labs(title="", x ="", y = "Amount of waterpoints", fill="") +
  theme(plot.title = element_text(family = "Assistant-Regular", size=14),
        axis.text.y = element_text(family = "Assistant-Regular", size=11, hjust=0), 
        axis.title.y = element_text(family = "Assistant-Regular", size=14), 
        axis.text.x = element_text(family = "Assistant-Regular", size=14),
        legend.text = element_text(family = "Assistant-Regular", size=12),
        legend.position = c(0.8, 0.5),
        legend.background = element_rect(fill="white", linetype = "blank")) + 
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))

ggplot(waterpoints) +
  geom_bar(aes(x=water_supply_type, y=risk_assessment, fill=water_supply_type), stat = "summary", fun.y = "mean") +
  theme_minimal() +
  scale_fill_manual(values=water_type_col) +
  labs(title="", x ="", y = "Amount of waterpoints", fill="") +
  theme(plot.title = element_text(family = "Assistant-Regular", size=14),
        axis.text.y = element_text(family = "Assistant-Regular", size=11, hjust=0), 
        axis.title.y = element_text(family = "Assistant-Regular", size=14), 
        axis.text.x = element_text(family = "Assistant-Regular", size=14),
        legend.text = element_text(family = "Assistant-Regular", size=12),
        legend.position = c(0.25, 0.8),
        legend.background = element_rect(fill="white", linetype = "blank")) + 
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))


waterpoints <- waterpoints %>%
  mutate(MPN_100ml = str_replace(MPN_100ml, ">100", "100")) %>% 
  mutate(MPN_100ml = as.numeric(MPN_100ml)) 

waterpoints %>% 
  ggplot(aes(x=risk_assessment, y=MPN_100ml, size = MPN_100ml, color=water_supply_type)) +
  geom_point(alpha=0.5) +
  scale_size(range = c(.1, 24), name="Population (M)")


# Correlations

grid.arrange(ggplot(waterpoints, aes(x=risk_assessment, y=pH.high)) + geom_point(),
ggplot(waterpoints, aes(x=risk_assessment, y=pH.low)) + geom_point(),
ggplot(waterpoints, aes(x=risk_assessment, y=fluoride)) + geom_point(),
ggplot(waterpoints, aes(x=risk_assessment, y=phosphate)) + geom_point(),
ggplot(waterpoints, aes(x=risk_assessment, y=chloride)) + geom_point(),
ggplot(waterpoints, aes(x=risk_assessment, y=electrical.conductivity)) + geom_point(), nrow=2)

grid.arrange(w_latrine_10m_pie, w_latrine_uphill_pie, 
             w_pollution_pie, w_drainage_faulty_pie, 
             w_drainage_pie, w_fence_pie, nrow=2)

corr_mat <- waterpoints %>% 
  select_if(is.numeric) %>%
  select(-c(nitrite, turbidity, barcode)) %>%
  rename(`elect. cond.` = electrical.conductivity,
         `risk score` = risk_assessment) %>%
  cor(use="complete.obs") 

col1 <- colorRampPalette(c("#D9D9D9", "white","#03AD8C"))
corrplot(corr_mat, 
         method="circle", 
         type="upper", 
         order="hclust",
         tl.col = "black",
         col=col1(10),
         family="Assistant-Regular")

corr_df <- as.data.frame(corr_mat) %>% mutate_all(funs(round(.,3)))
ggtexttable(corr_df[,14], rows = NULL, theme = ttheme("classic"))

waterpoints <- waterpoints %>%
  mutate_at(vars(geo.latitude, geo.longitude), function(x) as.numeric(as.character(x)))

# Risk assessment
col2 <-  colorRampPalette(c("#03AD8C", "red"))
pal = colorNumeric(col2(20), domain = waterpoints$risk_assessment)
map <- leaflet(waterpoints) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addCircleMarkers(
    color = ~pal(risk_assessment),
    stroke = FALSE, fillOpacity = 0.9,
    lng = ~geo.longitude, lat = ~geo.latitude,
    label = ~as.character(risk_assessment),
    radius=7
  ) %>%
  addLegend(pal=pal, values=~risk_assessment)

risk_assessment_wpt <- waterpoints %>% 
  group_by(water_supply_type) %>% 
  summarise(`Risk Assessment` = mean(risk_assessment, na.rm=TRUE))

risk_assessment_wpt$water_supply_type<- factor(
  risk_assessment_wpt$water_supply_type, 
  levels = risk_assessment_wpt$water_supply_type[order(risk_assessment_wpt$`Risk Assessment`)])

wt_col <- colorRampPalette(c("#F2F2F2","#F2F2F2", "#03AD8C"))
  
ggplot(risk_assessment_wpt, aes(x=water_supply_type, y=`Risk Assessment`, fill=water_supply_type)) +
  geom_bar(stat="identity") +
  theme_minimal() +
  scale_fill_manual(values=wt_col(15))+
  coord_flip() + theme(legend.position = "none")  + 
  labs(title="Risk assessment score", x ="", y = "") +
  theme(plot.title = element_text("Roboto-Regular", size=14),
        axis.title.x  = element_text(family = "Assistant-Regular", size=11),
        axis.text.y  = element_text("Assistant-Regular", size=12))

# water_data_df
waterpoints$`ecoli risk indication` <- factor(waterpoints$`ecoli risk indication`, 
                                 levels=c( "Low Risk / Safe", 
                                           "Intermediate Risk / Probably Safe", 
                                           "Intermediate Risk / Possibly Safe", 
                                           "High Risk / Possibly Unsafe",
                                           "High Risk / Probably Unsafe", 
                                           "Very High Risk / Unsafe"))
# Ecoli risk
col1 <- colorRampPalette(c("#03AD8C", "#FFA500"))
pal <- colorFactor(col1(6),
                   domain = unique(waterpoints$`ecoli risk indication`))

map <- leaflet(waterpoints) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addCircleMarkers(
    color = ~pal(`ecoli risk indication`),
    stroke = FALSE, fillOpacity = 0.5,
    lng = ~geo.longitude, lat = ~geo.latitude,
    label = ~as.character(`ecoli risk indication`)
  ) %>%
  addLegend(pal=pal, values=~`ecoli risk indication`)


# Borehole handpump
boreholes <- waterpoints %>%
  filter(waterpoints$water_supply_type %in% 
      c("New borehole - handpump",
        "Rehabilitated borehole - handpump")) %>%
  dplyr::select(c("identifier", "province", "district", 
                  "chiefdom", "community", "water.point.name", 
                  "implementing.partner", "facility", "geo.latitude", 
                  "geo.longitude", "geo.elevation", "water_supply_type", 
                  starts_with("borehole"))) %>%
  mutate_at(vars(starts_with("borehole")), function(x) ifelse(x=="Yes", 1,0)) %>%
  mutate(risk_assessment = rowSums(.[grep("borehole", names(.))], na.rm = TRUE))


# submersible

waterpoints[
  waterpoints$water_supply_type %in% c("New borehole with submersible pump",
                                       "Rehabilitated borehole with submersible pump",
                                       "New hand dug well with submersible pump",
                                       "Rehabilitated hand dug well with submersible pump"),]


# hand dug well - hand pump
waterpoints[
  waterpoints$water_supply_type %in% c("New hand dug well - handpump",
                                       "Rehabilitated hand dug well - handpump"),]
# (rehabilitated) spring box
waterpoints[
  waterpoints$water_supply_type %in% "Rehabilitate spring box",]

# Tap stands from gravity fed water scheme
waterpoints[
  waterpoints$water_supply_type %in% "Tap stands from gravity fed water scheme",]

# Rainwater harvested system
waterpoints[
  waterpoints$water_supply_type %in% "Rainwater harvested system",]



### DARLINGS #####

# Figures
latrine_10m <- melt(table(boreholes$borehole...handpump.latrine.distance.10m),
                    varnames="Latrine within 10m")
latrine_uphill <- melt(table(boreholes$borehole...handpump.latrine.uphill),
                       varnames="Latrine uphill")
pollution <- melt(table(boreholes$borehole...handpump.pollution.10m),
                  varnames="Pollution within 10m")
drainage_faulty <- melt(table(boreholes$borehole...handpump.drainage.faulty.ponding),
                        varnames="Failty drainage")
drainage <- melt(table(boreholes$borehole...handpump.drainage.channel),
                 varnames="Drainage channel")
fence <- melt(table(boreholes$borehole...handpump.fence.missing),
              varnames="Missing fence")

latrine_10m_pie <- ggplot(latrine_10m, aes(x="", y=value, fill=`Latrine within 10m`))+
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start=0) + scale_fill_manual(values=c("#F2F2F2", "#03AD8C")) +
  theme_minimal() +
  labs(title="Latrine within 10 m", x ="", y = "", fill="") +
  theme(title = element_text(family = "Assistant-Regular", size=11),
        legend.text = element_text(family = "Assistant-Regular", size=14),
        axis.text.x = element_text("Assistant-Regular", size=11))

latrine_uphill_pie <- ggplot(latrine_uphill, aes(x="", y=value, fill=`Latrine uphill`))+
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start=0) + scale_fill_manual(values=c("#F2F2F2", "#03AD8C")) +
  theme_minimal() + 
  labs(title="Latrine uphill", x ="", y = "", fill="") +
  theme(title = element_text(family = "Assistant-Regular", size=11),
        legend.text = element_text(family = "Assistant-Regular", size=14),
        axis.text.x = element_text("Assistant-Regular", size=11))


pollution_pie <- ggplot(pollution, aes(x="", y=value, fill=`Pollution within 10m`))+
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start=0) + scale_fill_manual(values=c("#F2F2F2", "#03AD8C")) +
  theme_minimal() + 
  labs(title="Pollution source within 10 m", x ="", y = "", fill="") +
  theme(title = element_text(family = "Assistant-Regular", size=11),
        legend.text = element_text(family = "Assistant-Regular", size=14),
        axis.text.x = element_text("Assistant-Regular", size=11))

drainage_faulty_pie <- ggplot(drainage_faulty, aes(x="", y=value, fill=`Failty drainage`))+
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start=0) + scale_fill_manual(values=c("#F2F2F2", "#03AD8C")) +
  theme_minimal() + 
  labs(title="Drainage faulty", x ="", y = "", fill="") +
  theme(title = element_text(family = "Assistant-Regular", size=11),
        legend.text = element_text(family = "Assistant-Regular", size=14),
        axis.text.x = element_text("Assistant-Regular", size=11))

drainage_pie <- ggplot(drainage, aes(x="", y=value, fill=`Drainage channel`))+
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start=0) + scale_fill_manual(values=c("#F2F2F2", "#03AD8C")) +
  theme_minimal() + 
  labs(title="Drainage channel", x ="", y = "", fill="") +
  theme(title = element_text(family = "Assistant-Regular", size=11),
        legend.text = element_text(family = "Assistant-Regular", size=14),
        axis.text.x = element_text("Assistant-Regular", size=11))

fence_pie <- ggplot(fence, aes(x="", y=value, fill=`Missing fence`))+
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start=0) + scale_fill_manual(values=c("#F2F2F2", "#03AD8C")) +
  theme_minimal() + 
  labs(title="Missing fence", x ="", y = "", fill="") +
  theme(title = element_text(family = "Assistant-Regular", size=11),
        legend.text = element_text(family = "Assistant-Regular", size=14),
        axis.text.x = element_text("Assistant-Regular", size=11))


# Save figures
grid.arrange(latrine_10m_pie, latrine_uphill_pie, 
             pollution_pie, drainage_faulty_pie, 
             drainage_pie, fence_pie, nrow=3)


# Wells
wells <- waterpoints[waterpoints$water_supply_type.1 == "Hand dug well", 
                   c(names(waterpoints)[names(waterpoints) %like% "well%"],"ecoli_risk")]

# Wells latrine 10 m
w_latrine_10m <- melt(table(wells$well...handpump.latrine.distance.10m),
                    varnames="Latrine within 10m")
w_latrine_10m_pie <- ggplot(w_latrine_10m, aes(x="", y=value, fill=`Latrine within 10m`))+
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start=0) + scale_fill_manual(values=c("#F2F2F2", "#404898")) +
  theme_minimal() + 
  labs(title="Latrine within 10m", x ="", y = "", fill="") +
  theme(title = element_text(family = "Assistant-Regular", size=11),
        legend.text = element_text(family = "Assistant-Regular", size=14),
        axis.text.x = element_text("Assistant-Regular", size=11))

# Wells latrine uphill
w_latrine_uphill <- melt(table(wells$well...handpump.latrine.uphill),
                       varnames="Latrine uphill")
w_latrine_uphill_pie <- ggplot(w_latrine_uphill, aes(x="", y=value, fill=`Latrine uphill`))+
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start=0) + scale_fill_manual(values=c("#F2F2F2", "#404898")) +
  theme_minimal() + 
  labs(title="Latrine uphill", x ="", y = "", fill="") +
  theme(title = element_text(family = "Assistant-Regular", size=11),
        legend.text = element_text(family = "Assistant-Regular", size=14),
        axis.text.x = element_text("Assistant-Regular", size=11))

# Wells pollution
w_pollution <- melt(table(wells$well...handpump.pollution.10m),
                  varnames="Pollution within 10m")
w_pollution_pie <- ggplot(w_pollution, aes(x="", y=value, fill=`Pollution within 10m`))+
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start=0) + scale_fill_manual(values=c("#F2F2F2", "#404898")) +
  theme_minimal() + 
  labs(title="Pollution within 10m", x ="", y = "", fill="") +
  theme(title = element_text(family = "Assistant-Regular", size=11),
        legend.text = element_text(family = "Assistant-Regular", size=14),
        axis.text.x = element_text("Assistant-Regular", size=11))

# Wells drainage faulty
w_drainage_faulty <- melt(table(wells$well...handpump.drainage.faulty.ponding),
                        varnames="Failty drainage")
w_drainage_faulty_pie <- ggplot(w_drainage_faulty, aes(x="", y=value, fill=`Failty drainage`))+
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start=0) + scale_fill_manual(values=c("#F2F2F2", "#404898")) +
  theme_minimal() + 
  labs(title="Failty drainage", x ="", y = "", fill="") +
  theme(title = element_text(family = "Assistant-Regular", size=11),
        legend.text = element_text(family = "Assistant-Regular", size=14),
        axis.text.x = element_text("Assistant-Regular", size=11))

# wells drainage channel
w_drainage <- melt(table(wells$well...handpump.drainage.channel),
                 varnames="Drainage Channel")
w_drainage_pie <- ggplot(w_drainage, aes(x="", y=value, fill=`Drainage Channel`))+
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start=0) + scale_fill_manual(values=c("#F2F2F2", "#404898")) +
  theme_minimal() + 
  labs(title="Drainage Channel", x ="", y = "", fill="") +
  theme(title = element_text(family = "Assistant-Regular", size=11),
        legend.text = element_text(family = "Assistant-Regular", size=14),
        axis.text.x = element_text("Assistant-Regular", size=11))

# Wells Fence
w_fence <- melt(table(wells$well...handpump.fence.missing),
              varnames="Missing fence")
w_fence_pie <- ggplot(w_fence, aes(x="", y=value, fill=`Missing fence`))+
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start=0) + scale_fill_manual(values=c("#F2F2F2", "#404898")) +
  theme_minimal() + 
  labs(title="Missing fence", x ="", y = "", fill="") +
  theme(title = element_text(family = "Assistant-Regular", size=11),
        legend.text = element_text(family = "Assistant-Regular", size=14),
        axis.text.x = element_text("Assistant-Regular", size=11))

# Cement radius
w_cement_radius <- melt(table(wells$well...handpump.cement.radius),
              varnames="Cement radius")
w_cement_radius_pie <- ggplot(w_cement_radius, aes(x="", y=value, fill=`Cement radius`))+
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start=0) + scale_fill_manual(values=c("#F2F2F2", "#404898")) +
  theme_minimal() + 
  labs(title="Cement radius", x ="", y = "", fill="") +
  theme(title = element_text(family = "Assistant-Regular", size=11),
        legend.text = element_text(family = "Assistant-Regular", size=14),
        axis.text.x = element_text("Assistant-Regular", size=11))

# Water in apron area
w_water_apron_area <- melt(table(wells$well...handpump.water.in.apron.area),
              varnames="Water in the apron area")
w_water_apron_area_pie <- ggplot(w_water_apron_area, aes(x="", y=value, fill=`Water in the apron area`))+
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start=0) + scale_fill_manual(values=c("#F2F2F2", "#404898")) +
  theme_minimal() + 
  labs(title="Water in the apron area", x ="", y = "", fill="") +
  theme(title = element_text(family = "Assistant-Regular", size=11),
        legend.text = element_text(family = "Assistant-Regular", size=14),
        axis.text.x = element_text("Assistant-Regular", size=11))

# Cement damage
w_cement_damage <- melt(table(wells$well...handpump.cement.damage),
              varnames="Cement damaged")
w_cement_damage_pie <- ggplot(w_cement_damage, aes(x="", y=value, fill=`Cement damaged`))+
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start=0) + scale_fill_manual(values=c("#F2F2F2", "#404898")) +
  theme_minimal() + 
  labs(title="Cement damaged", x ="", y = "", fill="") +
  theme(title = element_text(family = "Assistant-Regular", size=11),
        legend.text = element_text(family = "Assistant-Regular", size=14),
        axis.text.x = element_text("Assistant-Regular", size=11))


# Hand pump loose
w_handpump_loose <- melt(table(wells$well...handpump.handpump.loose),
              varnames="Handpump is loose")
w_handpump_loose_pie <- ggplot(w_handpump_loose, aes(x="", y=value, fill=`Handpump is loose`))+
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start=0) + scale_fill_manual(values=c("#F2F2F2", "#404898")) +
  theme_minimal() + 
  labs(title="Handpump is loose", x ="", y = "", fill="") +
  theme(title = element_text(family = "Assistant-Regular", size=11),
        legend.text = element_text(family = "Assistant-Regular", size=14),
        axis.text.x = element_text("Assistant-Regular", size=11))

# Well covered 
w_well_cover <- melt(table(wells$well...handpump.well.cover),
              varnames="Well covered")
w_well_cover_pie <- ggplot(w_well_cover, aes(x="", y=value, fill=`Well covered`))+
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start=0) + scale_fill_manual(values=c("#F2F2F2", "#404898")) +
  theme_minimal() + 
  labs(title="Well covered", x ="", y = "", fill="") +
  theme(title = element_text(family = "Assistant-Regular", size=11),
        legend.text = element_text(family = "Assistant-Regular", size=14),
        axis.text.x = element_text("Assistant-Regular", size=11))

grid.arrange(w_latrine_10m_pie, w_latrine_uphill_pie, 
             w_pollution_pie, w_drainage_faulty_pie, 
             w_drainage_pie, w_fence_pie, nrow=2)

grid.arrange(w_cement_radius_pie, w_water_apron_area_pie, 
             w_cement_damage_pie, w_handpump_loose_pie, 
             w_well_cover_pie, nrow=2)

# Rehabilitated Spring box

spring_box <- waterpoints[
  waterpoints$water_supply_type.1 == "Spring box", 
  c(names(waterpoints)[names(waterpoints) %like% "Rehabilitate.spring.box%"],"ecoli_risk")]

tap_stands <- waterpoints[
  waterpoints$water_supply_type.1 == "Tap stands", 
  c(names(waterpoints)[names(waterpoints) %like% "Tap.stands.from.gravity%"],"ecoli_risk")]



