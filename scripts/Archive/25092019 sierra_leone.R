library(openxlsx)
library(here)
library(extrafont)
library(tidyr)
library(dplyr)

## Akvo colour scheme
# Akvo Orange red:	    HEX #EA5547 rgb(234,85,71)
# Akvo Blue:      	    HEX #404898 rgb(64,72,152)
# Akvo Turquoise green:	HEX #03AD8C rgb(3,173,140)
# Akvo Pink:          	HEX #E04D95 rgb(224,77,149)

## Akvo fonts
# Roboto Condensed
# Assistant

# Householde data
household_data <- read.xlsx("Household Questionaire.xlsx")
names(household_data)[c(1, 43, 44, 45, 90, 91, 92)] <- c("ID", "photo_storage_lat",
                                                         "photo_storage_lon",
                                                         "photo_storage_acc",
                                                         "photo_hand_wash_lat",
                                                         "photo_hand_wash_lon",
                                                         "photo_hand_wash_acc")

# Caddisfly data
caddisfly <- read.xlsx("Household Questionaire Caddisfly.xlsx")
caddisfly <- caddisfly[,c("Identifier",
                          "ecoli_result_incubation|E.coli.test.result.AFTER.incubation.|Health.Risk.Category.(Based.on.MPN.and.Confidence.Interval)",
                          "--CADDISFLY--|MPN(MPN/100ml)",
                          "--CADDISFLY--|Upper.95%.Confidence.Interval",
                          "--CADDISFLY--|E.coli.test.result.AFTER.incubation.--Image")]
names(caddisfly) <- c("ID", "result", "MPN", "CI", "image")

# Combine data sources
SL_data <- left_join(household_data, caddisfly, by="ID")
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
water_risk_long$main_source_water <- gsub("lake, pond, stream, canal, irrigation channels)", "etc.)", water_risk_long$main_source_water)

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
  scale_fill_manual("legend",
                    values = c("Very High Risk"="#00e3b7", "High Risk"="#03AD8C", "Intermediate Risk"="#5c68db", "Low Risk"="#404898"))


# Main water source and test outcome
water_outcome <- cast(SL_data, main_source_water ~ outcome)
water_outcome_long <- melt(water_outcome, id="main_source_water")
water_outcome_long$value <- as.numeric(as.character(water_outcome_long$value))
water_outcome_long$main_source_water <- gsub(
  "lake, pond, stream, canal, irrigation channels)", "etc.)",
  water_outcome_long$main_source_water)


water_outcome_long <- within(
  water_outcome_long,
  outcome <- factor(outcome, levels = c("Unsafe", "Probably Unsafe", "Possibly Unsafe", "Possibly Safe","Probably Safe", "Safe")
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

# Practice data Veerle
SL_data_practice <- SL_data[
  ,c("ID", "location_Region", "location_District",
     "location_Chiefdom", "location_Sections", "geolocation_Latitude",
     "geolocation_Longitude", "geolocation_Elevation",
     "implementing_partner_Implementing_partner", "community_name",
     "number_household", "main_source_water",
     "main_source_water--OTHER--", "round_trip_minutes",
     "fetch_water", "--OPTION--|Adult.woman", "--OPTION--|Adult.man",
     "--OPTION--|Female.child.(under.15.years)",
     "--OPTION--|Male.child.(under.15.years)",
     "point_name", "container_type", "treatment_drinking_water",
     "flush_to", "share_facility", "number_households",
     "diarrhea_two_weeks", "barcode_preparation",
     "risk", "outcome", "MPN", "CI", "image")
  ]
