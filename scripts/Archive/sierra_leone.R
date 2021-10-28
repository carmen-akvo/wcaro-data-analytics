library(openxlsx)
library(here)
library(extrafont)
library(tidyr)
library(dplyr)
library(data.table)
library(reshape)

library(ggplot2)
library(ggthemes)
library(raster)
library(rgeos)


## Akvo colour scheme
# Akvo Orange red:	    HEX #EA5547 rgb(234,85,71)
# Akvo Blue:      	    HEX #404898 rgb(64,72,152)
# Akvo Turquoise green:	HEX #03AD8C rgb(3,173,140)
# Akvo Pink:          	HEX #E04D95 rgb(224,77,149)

## Akvo fonts
# Roboto Condensed
# Assistant
# AIzaSyDqjAH-quvoQKLAHar43l-4z10EVNB--yw

# Householde data
household_data <- read.xlsx(here("Data/Household Questionaire.xlsx"))
names(household_data)[c(1, 43, 44, 45, 90, 91, 92)] <- c("ID", "photo_storage_lat",
                                                         "photo_storage_lon",
                                                         "photo_storage_acc",
                                                         "photo_hand_wash_lat",
                                                         "photo_hand_wash_lon",
                                                         "photo_hand_wash_acc")
household_data$geolocation_Latitude <- as.numeric(household_data$geolocation_Latitude)
household_data$geolocation_Longitude <- as.numeric(household_data$geolocation_Longitude)

# Water sources
source_types <- c("Piped water into dwelling", "Rainwater collection", "Unprotected dug well","Protected dug well",
                  "Surface water (river, dam, lake, pond, stream, canal, irrigation channels)", "Tubewell/borehole",
                  "Unprotected spring", "Protected spring", "Piped water to yard/plot", "Public tap/standpipe")

sources_other <- c("Piped water into dwelling", "Rainwater collection", "Unprotected dug well",
                   "Surface water (river, dam, lake, pond, stream, canal, irrigation channels)",
                   "Unprotected spring", "Protected spring", "Piped water to yard/plot", "Public tap/standpipe")

sources_unicef <- c("Protected dug well", "Tubewell/borehole")

# Household locations based on water source
household_locations <- household_data[household_data$main_source_water %in% sources_unicef ,
                                      c("ID","main_source_water", "geolocation_Latitude", "geolocation_Longitude")]
household_locations <- household_locations[!is.na(household_locations$geolocation_Latitude), ]
household_locations$geolocation_Latitude <- as.numeric(household_locations$geolocation_Latitude)
household_locations$geolocation_Longitude <- as.numeric(household_locations$geolocation_Longitude)


# Household data Caddisfly
hh_caddisfly <- read.xlsx(here("Data/Household Questionaire Caddisfly.xlsx"))
hh_caddisfly <- hh_caddisfly[
  ,c("Identifier",
     "ecoli_result_incubation|E.coli.test.result.AFTER.incubation.|Health.Risk.Category.(Based.on.MPN.and.Confidence.Interval)",
     "--CADDISFLY--|MPN(MPN/100ml)",
     "--CADDISFLY--|Upper.95%.Confidence.Interval",
     "--CADDISFLY--|E.coli.test.result.AFTER.incubation.--Image")]
names(hh_caddisfly) <- c("ID", "result", "MPN", "CI", "image")

# Waterput data
waterput_data <- read.xlsx(here("Data/Water facility risk assessment form.xlsx"))
waterput_locations <- waterput_data[
  ,c("Identifier", "Location.-.Province","Location.-.District",
     "Location.-.Chiefdom", "Type.of.facility.where.water.point.is.located",
     "Geolocation.-.Latitude", "Geolocation.-.Longitude")
  ]

waterput_locations$`Geolocation.-.Latitude` <- as.numeric(waterput_locations$`Geolocation.-.Latitude`)
waterput_locations$`Geolocation.-.Longitude` <- as.numeric(waterput_locations$`Geolocation.-.Longitude`)

# Waterput data Caddisfly
wp_caddisfly <- read.xlsx(here("Data/Water facility risk assessment form Caddisfly.xlsx"))

wp_caddisfly <- wp_caddisfly[,c("Identifier",
                          "135780018|Fill.in.CBT.E.coli.(Escherichia.coli).result|Health.Risk.Category.(Based.on.MPN.and.Confidence.Interval)",
                          "--CADDISFLY--|MPN(MPN/100ml)",
                          "--CADDISFLY--|Upper.95%.Confidence.Interval",
                          "--CADDISFLY--|Fill.in.CBT.E.coli.(Escherichia.coli).result--Image")]
names(wp_caddisfly) <- c("ID", "result", "MPN", "CI", "image")

# data cleaning
household_data$geolocation_Longitude <- ifelse(
  household_data$geolocation_Longitude > 0, 
  NA, 
  household_data$geolocation_Longitude
  )

household_data$main_source_water <- gsub(
  "lake, pond, stream, canal, irrigation channels)", "etc.)",
  household_data$main_source_water
)

household_data$unicefwell <- ifelse(
  household_data$main_source_water %in% sources_unicef,
  "Possible UNICEF well","Other water source")

# data checks 
trip_water_source <- as.data.frame(table(household_data$main_source_water, household_data$round_trip_minutes))
g <- ggplot(trip_water_source, aes(x=Var1, y=Freq, fill=Var2))
g +
  geom_bar(stat="identity", position='dodge') +
  theme(axis.text.x = element_text(family='Roboto')) 

# Combine data sources
SL_data <- left_join(household_data, hh_caddisfly, by="ID")
SL_data <- SL_data %>% separate(result, c("risk", "outcome"), sep=" / ")

# descriptives

# Main water source versus the walking distance
distance_source <- cast(SL_data, round_trip_minutes ~ main_source_water)

# Person fetching the water compared to the source
water_fetchers <- SL_data[,c(
  'main_source_water',
  '--OPTION--|Adult.woman',
  '--OPTION--|Adult.man',
  '--OPTION--|Female.child.(under.15.years)',
  '--OPTION--|Male.child.(under.15.years)')
  ]
names(water_fetchers) <- c('Source', 'Woman', 'Man', 'Female child (<15)', 'Male child (<15)')
water_fetchers <- melt(water_fetchers, id="Source")

# water_fetchers <- water_fetchers %>% group_by(Source) %>% summarise_each(sum)

# Main water source and risk
water_risk <- cast(SL_data, main_source_water ~ risk)
water_risk_long <- melt(water_risk, id="main_source_water")
water_risk_long$value <- as.numeric(as.character(water_risk_long$value))
water_risk_long$main_source_water <- gsub("lake, pond, stream, canal, irrigation channels)", "etc.)",
                                          water_risk_long$main_source_water)

water_risk_long <- within(
  water_risk_long,
  risk <- factor(risk, levels = c("Very High Risk", "High Risk", "Intermediate Risk", "Low Risk")
  )
)

# Plots

g <- ggplot(water_risk_long, aes(x=main_source_water, y=value, fill=risk))
g +
  geom_bar(stat="identity", position='fill') +
  theme(axis.text.x = element_text(family='Roboto')) +
  coord_flip() +
  scale_fill_manual(
    "legend",
    values = c("Very High Risk"="#00e3b7", "High Risk"="#03AD8C",
               "Intermediate Risk"="#5c68db", "Low Risk"="#404898")
    )


# Main water source and test outcome
water_outcome <- cast(SL_data, main_source_water ~ outcome)
water_outcome_long <- melt(water_outcome, id="main_source_water")
water_outcome_long$value <- as.numeric(as.character(water_outcome_long$value))
water_outcome_long$main_source_water <- gsub(
  "lake, pond, stream, canal, irrigation channels)", "etc.)",
  water_outcome_long$main_source_water)


water_outcome_long <- within(
  water_outcome_long,
  outcome <- factor(
    outcome,levels = c("Unsafe", "Probably Unsafe", "Possibly Unsafe",
                       "Possibly Safe", "Probably Safe", "Safe")
  )
)

# Plots

g <- ggplot(water_outcome_long, aes(x=main_source_water, y=value, fill=outcome))
g +
  geom_bar(stat="identity", position='fill') +
  theme(axis.text.x = element_text(family='Roboto')) +
  coord_flip() +
  scale_fill_manual("legend",
                    values = c("Unsafe"="#404898",
                               "Probably Unsafe"="#5c68db", 
                               "Possibly Unsafe"="#6a77fc", 
                               "Possibly Safe"="#03AD8C",
                               "Probably Safe"="#00e3b7",
                               "Safe"="#00ffce"))

######################
## Maps
## More Q&A - https://github.com/dkahle/ggmap/issues/51

#Get the latest Install Google maps API:
# if(!requireNamespace("devtools")) install.packages("devtools")
# devtools::install_github("dkahle/ggmap", ref = "tidyup", force=TRUE)

# GGmap --> Needs Google API
library(mapproj)
library(ggmap)
library(rworldmap)
library(CartoDB)
library(maps)

#Set your API Key --> Google platform account via Oriol
ggmap::register_google(key = "AIzaSyDqjAH-quvoQKLAHar43l-4z10EVNB--yw")

p <- ggmap(get_googlemap(center = c(lon = -12, lat = 8),
                         zoom = 8, scale = 2,
                         maptype ='terrain',
                         color = 'color'))

p + geom_point(
  aes(
    x = `Geolocation.-.Longitude`,
    y = `Geolocation.-.Latitude`,
    color=Type.of.facility.where.water.point.is.located),
               data = waterput_locations, size = 0.5) + 
  theme(legend.position="bottom")


# Rwolrdmap
newmap <- getMap(resolution="low")
plot(newmap, xlim=c(-14,-8), ylim=c(7.5,9.5), asp=1)

points(waterput_locations$`Geolocation.-.Longitude`,
       waterput_locations$`Geolocation.-.Latitude`, col = "red", cex = .6)

points(household_locations$geolocation_Longitude,
       household_locations$geolocation_Latitude, col = "blue", cex = .6)


## Carto
account <- "carmen-akvo"
key <- "352f5e1357f4b29bcdeed8273b701de000b205fb"

cartodb(account, api.key=key)
cartodb.test()

map(regions="sierra leone", fill=TRUE, col="#ffc0a8")
points(waterput_data$`Geolocation.-.Longitude`,
       waterput_data$`Geolocation.-.Latitude`,
       color='#404898', fill='#fcfbfa', shape=22)

## Locatie putten en huishoudens
library(rgeos)
library(raster)

SL <- getData("GADM", country="SL", level=1)
SL <- gSimplify(SL, 0.01)

SLmap <- fortify(SL)

gg <- ggplot(household_data) + 
  geom_map(map=SLmap, data=SLmap,
           aes(x=long, y=lat, map_id=id),
           fill="#ffc0a8", color="black", size=0.25)+ 
  coord_map() + 
  theme_map() + 
  theme(legend.position = c(-0.5,0.2)) +
  geom_point(data=waterput_locations, aes(`Geolocation.-.Longitude`, `Geolocation.-.Latitude`),
             color='#404898', fill='#fcfbfa', shape=22, size=2) +
  geom_point(aes(`geolocation_Longitude`, `geolocation_Latitude`,
                 color=unicefwell), size=1) +
  scale_color_manual("Main water source", values = c("#03AD8C", "#404898"))

gg

distance <- as.data.frame(
  pointDistance(
    waterput_locations[,c("Geolocation.-.Longitude","Geolocation.-.Latitude")],
    household_data[,c("geolocation_Longitude", "geolocation_Latitude")],
    lonlat=TRUE
  )
)

rownames(distance) <- waterput_locations$Identifier
colnames(distance) <- household_data$ID
household_data$distance_well <- apply(distance, 2, min, na.rm=TRUE)



