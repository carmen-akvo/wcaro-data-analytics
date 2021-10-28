library(dplyr)
library(data.table)
library(readxl)
library(ggplot2) 
library(here) 
library(stringr)
library(tidyr)
library(DescTools)

## Read data
well <- read_excel(here("SL/Data", "SL_wells.xlsx")) 
well_monitoring <- read_excel(here("SL/Data", "SL_wells_monitoring.xlsx")) 

# well <- well %>% 
#   rename("identifier" = "Identifier",
#          "province" = "Location - Province" ,
#          "district" = "Location - District",
#          "chiefdom" = "Location - Chiefdom",
#          "community" = "Name of community (TYPE NAME IN CAPITAL LETTERS)",
#          "water point name" = "Name of water point (TYPE NAME IN CAPITAL LETTERS)",
#          "implementing partner (UNICEF)" =  "Name of UNICEF implementing partner that constructed the water facility - Implementing partner",
#          "facility" = "Type of facility where water point is located"    ,
#          "geo latitude" = "Geolocation - Latitude" ,
#          "geo longitude" = "Geolocation - Longitude",
#          "geo elevation" = "Geolocation - Elevation",
#          "pH high" = "114460027|pH (High Range)|pH",                                
#          "pH low" = "151820540|pH (Low Range)|pH",                                      
#          "ammonia" = "120350044|Ammonia |Ammonia(mg/l)",                    
#          "fluoride" = "106580042|Fluoride|Fluoride(mg/l)",
#          "iron" = "131740018|Iron |Iron(mg/l)",
#          "nitrate" = "104510038|Nitrate |Nitrate(mg/l)",
#          "phosphate" = "100880015|Phosphate|Phosphate (ortho)(mg/l)",
#          "potassium" = "112740025|Potassium |Potassium(mg/l)",
#          "sulfate" = "124050054|Sulfate|Sulfate(mg/l)",
#          "nitrite" = "135520044|Nitrite |Nitrite(mg/l)",
#          "chloride" = "129780050|Chloride (liquid reagent)|Chloride(mg/l)",
#          "turbidity" = "102670024|Turbidity|Turbidity(FAU)",                        
#          "electrical conductivity" = "120390030|Electrical Conductivity |Electrical Conductivity(μS/cm)",
#          "temperature" = "What is the temperature of the water sample?",         
#          "barcode" = "Scan barcode label on Compartment Bag"
#   )

names(well) <- c(
  "identifier", "province", "district", "chiefdom", "community", "water point name",
  "implementing partner", "facility", "geo latitude", "geo longitude",
  "geo elevation", "water_supply_type", "borehole - handpump-latrine distance 10m",
  "borehole - handpump-latrine uphill", "borehole - handpump-pollution 10m", "borehole - handpump-drainage faulty ponding",
  "borehole - handpump-drainage channel", "borehole - handpump-fence missing", "borehole - handpump-apron radius",
  "borehole - handpump-water in apron area", "borehole - handpump-apron damaged", "borehole - handpump-handpump loose",
  "submersible pump-latrine distance 100m", "submersible pump-pit latrine", "submersible pump-pollution 50m",
  "submersible pump-unapped well 100m", "submersible pump-drainage faulty", "submersible pump-fence damaged",
  "submersible pump-borehole floor", "submersible pump-pools", "submersible pump-well seal",
  "well - handpump-latrine distance 10m", "well - handpump-latrine uphill", "well - handpump-pollution 10m",
  "well - handpump-drainage faulty ponding", "well - handpump-drainage channel", "well - handpump-fence missing",
  "well - handpump-cement radius", "well - handpump-water in apron area", "well - handpump-cement damage",
  "well - handpump-handpump loose", "well - handpump-well cover", "Rehabilitate spring box-spring protected",
  "Rehabilitate spring box-masory damage", "Rehabilitate spring box-backfill area",
  "Rehabilitate spring box-water flood", "Rehabilitate spring box-fence missing",
  "Rehabilitate spring box-animal access", "Rehabilitate spring box-latrine uphill",
  "Rehabilitate spring box-surface water collection", "Rehabilitate spring box-diversion ditch",
  "Rehabilitate spring box-pollution", "Tap stands from gravity fed water scheme-tapstand leak",
  "Tap stands from gravity fed water scheme-surface water collection", "Tap stands from gravity fed water scheme-area eroded",
  "Tap stands from gravity fed water scheme-pipes exposed", "Tap stands from gravity fed water scheme-human excreta", "Tap stands from gravity fed water scheme-sewer distance",
  "Tap stands from gravity fed water scheme-discontinuity", "Tap stands from gravity fed water scheme-pipe leaks",
  "Tap stands from gravity fed water scheme-pipe breaks","Tap stands from gravity fed water scheme-pipe exposure",
  "Tap stands from gravity fed water scheme-pipe leaks storage", "Tap stands from gravity fed water scheme-storage damage",
  "Tap stands from gravity fed water scheme-air vents", "Rainwater harvested system-open container",
  "Rainwater harvested system-contamination roof", "Rainwater harvested system-guttering blocked",
  "Rainwater harvested system-tank damaged", "Rainwater harvested system-tank collection",
  "Rainwater harvested system-bucket", "Rainwater harvested system-tap damaged",
  "Rainwater harvested system-floor damaged", "Rainwater harvested system-pollution",
  "Rainwater harvested system-tank clear", "pH high", "pH low", "ammonia",
  "fluoride", "iron", "nitrate", "phosphate", "potassium", "sulfate", "nitrite",
  "chloride", "turbidity", "electrical conductivity", "temperature", "barcode")

names(well_monitoring) <- tolower(names(well_monitoring))
well_monitoring <- well_monitoring %>%
  rename(barcode = `scan barcode label on compartment bag`)

## data type date omzetten
# well$`Submission Date` <- as.Date(household$`Submission Date` , format="%d-%m-%Y %H:%M:%S")
well_monitoring$`submission date` <- as.Date(
  well_monitoring$`submission date`, format="%d-%m-%Y %H:%M:%S")

## Merge datasets on survey households and ecoli results on ID and barcode
wells <- inner_join(well, well_monitoring, by=c("identifier", "barcode"))

# Data changes
names(wells)[99] <- "ecoli_risk"

wells <- wells %>%
  dplyr::select(-c("repeat no", "display name",
                   "device identifier", "instance",
                   "submission date", "submitter",
                   "duration", "form version", 
                   "--caddisfly--|fill in cbt e.coli (escherichia coli) result--image")) %>%
  mutate_at(vars(
    starts_with("borehole"), 
    starts_with("submersible"),
    starts_with("well"),
    starts_with("Rehabilitate"),
    starts_with("Tap"),
    starts_with("Rainwater")), 
    function(x) ifelse(x=="Yes", 1, 0)) %>%
  mutate(
    risk_assessment = rowSums(
      .[grep("borehole|submersible|well|Rehabilitate|Tap|Rainwater", names(.))], na.rm = TRUE)) %>%
  mutate_at(vars(`pH high`:temperature), function(x) gsub(">|<", "", x)) %>%
  mutate_at(vars(`pH high`:temperature), as.numeric) %>%
  dplyr::select(-starts_with("borehole"), 
                -starts_with("submersible"),
                -starts_with("well"),
                -starts_with("Rehabilitate"),
                -starts_with("Tap"),
                -starts_with("Rainwater")) %>%
  dplyr::rename(MPN_100ml = "--caddisfly--|mpn(mpn/100ml)",                                     
                conf_inter_95 = "--caddisfly--|upper 95% confidence interval",
                `ecoli risk indication` = ecoli_risk) %>%
  distinct(`identifier`, .keep_all = TRUE)

write.csv2(wells, here("output","SL_wells.csv"), row.names=FALSE)

