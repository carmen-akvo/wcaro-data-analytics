library(dplyr)
library(data.table)
library(readxl)
library(ggplot2) 
library(here) 
library(stringr)
library(tidyr)

## Read data
household <- read_excel(here("data", "SL_household.xlsx")) 
hh_monitoring <- read_excel(here("data", "SL_household_monitoring.xlsx")) 

## data type date omzetten
household$`Submission Date` <- as.Date(household$`Submission Date` , format="%d-%m-%Y %H:%M:%S")
hh_monitoring$`Submission Date` <- as.Date(hh_monitoring$`Submission Date` , format="%d-%m-%Y %H:%M:%S")

## Filter training dates out of the set --> SEE NOTES
household <- household %>%
  filter(!(`Submission Date` >= "2018-11-15" & `Submission Date` <= "2018-11-19")) %>%
  filter(!(`Submission Date` >= "2018-11-20"  & `Submission Date` <= "2018-11-24" & location_Region == "South"))

## Rename
household <- household %>% rename(barcode = barcode_preparation)
hh_monitoring <- hh_monitoring %>% rename(barcode = scan_barcode_after)

## Select necessary columns and rename columns both data frames
household <- household %>%
  select(-c(`Repeat no`, `Display Name`, `Device identifier`, Instance, `Form version`)) %>%
  rename(`duration registration` = Duration) %>%
  rename(`submitter registration` = Submitter) %>%
  rename(`submission date registration` = `Submission Date`)

hh_monitoring <- hh_monitoring %>%
  select(-c(`Repeat no`, `Display Name`, `Device identifier`, Instance, `Form version`)) %>%
  rename(`duration monitoring` = Duration) %>%
  rename(`submitter monitoring` = Submitter) %>%
  rename(`submission date monitoring` = `Submission Date`) %>%
  rename(`risk level ecoli` = `ecoli_result_incubation|E.coli test result AFTER incubation |Health Risk Category (Based on MPN and Confidence Interval)`)

## Remove duplicates from monitoring form
hh_monitoring <- hh_monitoring %>%
  group_by(`Identifier`, barcode, `risk level ecoli`, `--CADDISFLY--|MPN(MPN/100ml)`) %>%
  filter(n() == 1)

## Merge datasets on survey households and ecoli results on ID and barcode
SL_household <- inner_join(household, hh_monitoring, by=c("Identifier", "barcode"))

## change order of variables
SL_household <- SL_household %>% 
  select(Identifier, barcode,
         `duration registration`, `duration monitoring`, 
         `submitter registration`, `submitter monitoring`,
         `submission date registration`, `submission date monitoring`,
         everything())

## change all column names to lower
SL_household <- SL_household %>% rename_all(tolower)

## Remove "OPTION" part of dummy variables --> DOESNT WORK YET
names(SL_household) <- names(SL_household) %>% 
  str_replace("\\|+", "") 
names(SL_household) <- names(SL_household) %>% 
  str_replace("--option--", "op: ") 
names(SL_household) <- names(SL_household) %>% 
  str_replace("--photo--", "photo: ") 
names(SL_household) <- names(SL_household) %>% 
  str_replace("--caddisfly--", "caddisfly: ") 

## Check voor duplicate ecoli results
## 100 households have >1 ecoli result
## Of which 65 have > test, but with the same Risk Level result
ecoli_dupli <- SL_household %>%
  group_by(barcode) %>%
  filter(n() >1) %>%
  select(barcode, `risk level ecoli`)%>%
  group_by(barcode) %>%
  mutate(sum_levels = n_distinct(`risk level ecoli`)) 

## For 35 households there are multiple ecoli results with different risk levels 
ecoli_error <- ecoli_dupli %>%
  filter(sum_levels >1) 

## These households need to be removed from the dataset 
## Remove duplicates of households that have >1 ecoli result, but same outcome
SL_household <- SL_household %>%
  subset(!(barcode %in% ecoli_error$barcode)) %>%
  distinct(barcode, .keep_all = TRUE)

## Create a subset that you'll use for further analyses 
SL_sub <- SL_household %>% 
  select(-contains("op:"))  

SL_sub <- rename(SL_sub, "mpn/100ml" = `caddisfly: mpn(mpn/100ml)`) 
SL_sub$`mpn/100ml` <- gsub(">", "", SL_sub$`mpn/100ml`) 
SL_sub$`mpn/100ml` <- as.numeric(SL_sub$`mpn/100ml`)

## Other columns

## Water containers

# Split the container type column
SL_sub <- SL_sub %>% 
  separate(container_type, c("container amount", "container measurement","container type"), " ") %>%
  unite("container amount", `container amount`:`container measurement`, sep = " ", remove=TRUE, na.rm=TRUE)

# Do the same for the other column, lower, remove redundant strings filter to the ones containing liter indication
SL_sub_other_container <- SL_sub %>%
  mutate(`container_type--other--` = tolower(`container_type--other--`)) %>%
  filter(grepl("litre|liter", `container_type--other--`)) %>%
  mutate(`container_type--other--` = gsub("rubber |local |clay |big ", "", `container_type--other--`)) %>%
  separate(`container_type--other--`, c("container amount other", "container measurement other", "container type other"), " ") %>%
  unite("container amount other", `container amount other`:`container measurement other`, sep = " ", remove=TRUE, na.rm=TRUE) %>%
  select(identifier, barcode, `container amount other`,`container type other`)

# join filtered set back to original, lower, remove redundant strings
SL_sub <- SL_sub %>% 
  left_join(SL_sub_other_container) %>%
  mutate(`container_type--other--` = tolower(`container_type--other--`)) %>%
  mutate(
    `container_type--other--` = gsub("in a |\\.|rubber|local|clay|big|native|drinking|open", "", `container_type--other--`)) %>%
  mutate(`container_type--other--` = str_trim(`container_type--other--`)) %>%
  mutate(
    `container type other` = ifelse(is.na(`container type other`), `container_type--other--`, `container type other`))

# combine columns and delete redundant ones
SL_sub <- SL_sub %>%
  mutate(`container type` = gsub("^$", NA, `container type`)) %>%
  mutate(`container amount` = gsub("^$", NA, `container amount`)) %>%
  mutate(`container type` = coalesce(`container type`, `container type other`)) %>%
  mutate(`container amount` = coalesce(`container amount`, `container amount other`)) %>%
  select(-c(`container_type--other--`,`container amount other`,`container type other`))

## Main & other water source

# lowercase
SL_sub <- SL_sub %>%
  mutate(`water_source_other_purpose--other--` = tolower(`water_source_other_purpose--other--`)) %>%
  mutate(`main_source_water--other--` = tolower(`main_source_water--other--`))  %>%
  mutate(`water_source_other_purpose` = tolower(`water_source_other_purpose`)) %>%
  mutate(`main_source_water` = tolower(`main_source_water`))

# options
other_surface_water <- c("wolanwolan water point","wulawolan stream water point","wolawolan stream water point",
                         "kasubuyen","bally river","bankoh","stream water",
                         "stream","syream","woromquie stream","swamp")
other_protected_spring <- c("possible gravity source","possible  gravity spring","possible gravity spring")

# Change in set
SL_sub <- SL_sub %>% 
  mutate(`main_source_water--other--` = 
           replace(`main_source_water--other--`,
                   `main_source_water--other--` %in% other_surface_water, 
                   "surface water (river, dam, lake, pond, stream, canal, irrigation channels)")) %>% 
  mutate(`main_source_water--other--` = 
           replace(`main_source_water--other--`,
                   `main_source_water--other--` %in% other_protected_spring,
                   "protected spring")) %>%
  mutate(`main_source_water--other--` = 
           replace(`main_source_water--other--`,
                   `main_source_water--other--` == "hand dug pump",
                   NA)) %>%
  mutate(`water_source_other_purpose--other--` = 
           replace(`water_source_other_purpose--other--`,
                   `water_source_other_purpose--other--` %in% other_surface_water,
                   "surface water (river, dam, lake, pond, stream, canal, irrigation channels)")) %>% 
  mutate(`water_source_other_purpose--other--` = 
           replace(`water_source_other_purpose--other--`,
                   `water_source_other_purpose--other--` %in% other_protected_spring,
                   "protected spring")) %>% 
  mutate(`water_source_other_purpose--other--` = 
           replace(`water_source_other_purpose--other--`,
                   `water_source_other_purpose--other--` == "rain water",
                   "rainwater collection")) 

## Stool

# lowercase
SL_sub <- SL_sub %>%
  mutate(`done_to_stool--other--` = tolower(`done_to_stool--other--`))  %>%
  mutate(`done_to_stool` = tolower(`done_to_stool`)) 

# options
left_in_open <- c("bush","bush or field", "throwing in brush")
latrine <- "throw in to neighbor's latrine"
na <- "no child with me"

# Change in set
SL_sub <- SL_sub %>% 
  mutate(`done_to_stool--other--` = 
           replace(`done_to_stool--other--`,`done_to_stool--other--` %in% left_in_open, "left in the open")) %>% 
  mutate(`done_to_stool--other--` = 
           replace(`done_to_stool--other--`,`done_to_stool--other--` %in% latrine, "put/rinsed into toilet or latrine")) %>% 
  mutate(`done_to_stool--other--` = 
           replace(`done_to_stool--other--`,`done_to_stool--other--` %in% na, NA))


## Toilet facilities

# lowercase
SL_sub <- SL_sub %>%
  mutate(`toilet_facility_type--other--` = tolower(`toilet_facility_type--other--`))  %>%
  mutate(`toilet_facility_type` = tolower(`toilet_facility_type`)) 

# options
toilets <- c("share with neighborhood","using neighbouring latrine",
             "use neibouring latrine","in a neiboring latrine","in a neibouring latrine",
             "no latrine uses neighborhood","nabour house","neighbor house","neighborhood",
             "neighbor","niebuhr's")
latrines_with_other_slab <- c("pit latrine with locally made slab",
                              "unimproved pit latrine made with local materials",
                              "pit latrine with local made material","pit latrine with local material",
                              "clts latrine built with stick","clts latrine","pit latrines with local made material",
                              "pit  latrines with local made material")
latrines_without_slab <- c("pit latrine with locally made materials without slab",
                           "pit latrine with locally made materials without concrete slab",
                           "pit latrine with locally made materials but without concrete slab")
unidentified <- c("board slab with multiple openings", "uncovered board slab",
                  "community toilet" , "river", "community centre", "no latrine", "no", "school")

# Change in set
SL_sub <- SL_sub %>% 
  mutate(`toilet_facility_type--other--` = 
           replace(`toilet_facility_type--other--`,
                   `toilet_facility_type--other--` %in% toilets, 
                           "uses toilet of neighbor")) %>% 
  mutate(`toilet_facility_type--other--` = 
           replace(`toilet_facility_type--other--`,
                   `toilet_facility_type--other--` %in% latrines_with_other_slab, 
                           "pit latrine with other type of slab")) %>% 
  mutate(`toilet_facility_type--other--` = 
           replace(`toilet_facility_type--other--`,
                   `toilet_facility_type--other--` %in% latrines_without_slab, 
                           "pit latrine without slab/open pit"))  %>%
  mutate(`toilet_facility_type--other--` = 
           replace(`toilet_facility_type--other--`,
                   `toilet_facility_type--other--` %in% unidentified, 
                   NA))


# Water storage

# lowercase
SL_sub <- SL_sub %>%
  mutate(`store_drinking_water--other--` = tolower(`store_drinking_water--other--`))  %>%
  mutate(`store_drinking_water` = tolower(`store_drinking_water`)) 

# options
item_no_lid <- c("country pot", "30 liter clay pot",
            "25 litres rubber bucket","drinking bucket","in a local clay pot",  "rubber bucket",
            "rubber kettle", "store in a local clay pot", "uncovered rubber")
item_with_lid <- c("in a contain with lid", "container with lid", "drinking bucket with fitted lid cover",
                   "in a big bowl covered with a pan", "in a big bowl covered with another bowl",
                   "in a big bowl with cover", "in a big pot with cover", "in a big pot with pan cover",
                   "in a bowl covered with winnower", "in a bowl with cover",             
                   "in a clay pot with cover", "In a contain with lid",
                   "in a container with lid", "in a local clay pot with cover",
                   "In a local pot with cover", "in a local pot with cover",
                   "in a pot completely cover with lid", "in a pot covered with plate",
                   "in a pot with cover", "rubber bucket with fitted cover",
                   "rubber bucket with fitted lid cover","rubber bucket with lid cover")
item_narrow_opening <- c("in a bowl with narrow mouthed cover", "in a local pot with narrow opening",
                         "jerry  can","60 litres tank", "60 litres water tank")
unidentified <- c("batter", "drinking rubber", "hug foot rubber kettle",
                  "one gallon rubber","quarse rubber kettle","quarse rubber","rubber baff")

# Change in set
SL_sub <- SL_sub %>% 
  mutate(`store_drinking_water--other--` = 
           replace(`store_drinking_water--other--`,
                   `store_drinking_water--other--` %in% item_no_lid, 
                   "in container with no lid or cover")) %>% 
  mutate(`store_drinking_water--other--` = 
           replace(`store_drinking_water--other--`,
                   `store_drinking_water--other--` %in% item_with_lid, 
                   "in container with lid but no spigot/tap")) %>% 
  mutate(`store_drinking_water--other--` = 
           replace(`store_drinking_water--other--`,
                   `store_drinking_water--other--` %in% item_narrow_opening, 
                   "in narrow-mouthed container")) %>%
  mutate(`store_drinking_water--other--` = 
           replace(`store_drinking_water--other--`,
                   `store_drinking_water--other--` %in% unidentified, 
                   NA)) 

## Observed storage

# lowercase
SL_sub <- SL_sub %>%
  mutate(`stored_observed--other--` = tolower(`stored_observed--other--`))  %>%
  mutate(`stored_observed` = tolower(`stored_observed`)) 

# options
covered_with_lid <- c("completely covered with fitted lid",
                      "rubber bucket is completely covered with fitted lid",
                      "140 litres rubber completely covered with lid")
covered_with_lid_dirty <- c("drinking bucket completely covered but lid very dirty",
                            "dirty cover",
                            "covered but not clean")

covered_with_opening <- c("use plate to cover",
                          "covered with slight openings under cover",
                          "water stored in a big rubber bowl with ci sheet as a cover in an unprotected area")
unidentified <- c("not too clean", "rubber kettle")

# Change in set
SL_sub <- SL_sub %>% 
  mutate(`stored_observed--other--` = 
           replace(`stored_observed--other--`,
                   `stored_observed--other--` %in% covered_with_opening, 
                   "narrow opening")) %>% 
  mutate(`stored_observed--other--` = 
           replace(`stored_observed--other--`,
                   `stored_observed--other--` %in% covered_with_lid, 
                   "completely covered with lid")) %>%
  mutate(`stored_observed--other--` = 
           replace(`stored_observed--other--`,
                   `stored_observed--other--` %in% covered_with_lid_dirty, 
                   "completely covered with lid|dirty")) %>%
  mutate(`stored_observed--other--` = 
           replace(`stored_observed--other--`,
                   `stored_observed--other--` %in% unidentified, 
                   NA)) 

## treatment drinking water

# lowercase
SL_sub <- SL_sub %>%
  mutate(`treatment_drinking_water--other--` = tolower(`treatment_drinking_water--other--`))  %>%
  mutate(`treatment_drinking_water` = tolower(`treatment_drinking_water`)) 

# options
cloth <- c("a white piece of cloth", "white cloth to filter water")
canfor <- c("add canfor", "canfor", "kanfor")
undefined <- "wash drinking bucket with clean water and soap"                                      

# Change in set
SL_sub <- SL_sub %>% 
  mutate(`treatment_drinking_water--other--` = 
           replace(`treatment_drinking_water--other--`,
                   `treatment_drinking_water--other--` %in% cloth, 
                   "strain through cloth")) %>% 
  mutate(`treatment_drinking_water--other--` = 
           replace(`treatment_drinking_water--other--`,
                   `treatment_drinking_water--other--` %in% canfor, 
                   "bleach / jik")) %>% 
  mutate(`treatment_drinking_water--other--` = 
           replace(`treatment_drinking_water--other--`,
                   `treatment_drinking_water--other--` %in% undefined, 
                   NA))

## treatment observed

# lowercase
SL_sub <- SL_sub %>%
  mutate(`treatment_observe--other--` = tolower(`treatment_observe--other--`))  %>%
  mutate(`treatment_observe` = tolower(`treatment_observe`)) 

# options
cloth_observed <- c("a white piece of cloth to filter the water", "white cloth to filter drinking water",
                    "cloth use to strain water appears unclean")
chlorine_observed <-c("chlorine is normally put into the well quarterly", 
                      "sometimes using aqua tabs", "sometimes using aquatab")
undefined <- c("rubber is completely clean", "not applicable")

# Change in set
SL_sub <- SL_sub %>% 
  mutate(`treatment_observe--other--` = 
           replace(`treatment_observe--other--`,
                   `treatment_observe--other--` %in% cloth_observed, 
                   "observed cloth, and if it appears intact")) %>% 
  mutate(`treatment_observe--other--` = 
           replace(`treatment_observe--other--`,
                   `treatment_observe--other--` %in% chlorine_observed, 
                   "observed chlorine bottle/tablets, test")) %>% 
  mutate(`treatment_observe--other--` = 
           replace(`treatment_observe--other--`,
                   `treatment_observe--other--` %in% undefined, 
                   NA))    

## merge option and other columns
SL_sub <- SL_sub %>%
  mutate(main_source_water = coalesce(`main_source_water--other--`, `main_source_water`)) %>%
  mutate(water_source_other_purpose = coalesce(`water_source_other_purpose--other--`, water_source_other_purpose))  %>%
  mutate(store_drinking_water = coalesce(`store_drinking_water--other--`, store_drinking_water)) %>%
  mutate(stored_observed = coalesce(`stored_observed--other--`, stored_observed)) %>%
  mutate(treatment_drinking_water = coalesce(`treatment_drinking_water--other--`, treatment_drinking_water)) %>%
  mutate(treatment_observe = coalesce(`treatment_observe--other--`, treatment_observe)) %>%
  mutate(toilet_facility_type = coalesce( `toilet_facility_type--other--`, toilet_facility_type)) %>%
  mutate(done_to_stool = coalesce( `done_to_stool--other--`, done_to_stool)) %>%
  select(-c(`main_source_water--other--`, `water_source_other_purpose--other--`,`store_drinking_water--other--`,
            `stored_observed--other--`, `treatment_drinking_water--other--`,`treatment_observe--other--`,
            `toilet_facility_type--other--`, `done_to_stool--other--`))


# test <- SL_household[,c("identifier","main_source_water--other--", "water_source_other_purpose--other--","store_drinking_water--other--",
#                         "stored_observed--other--", "treatment_drinking_water--other--","treatment_observe--other--",
#                         "toilet_facility_type--other--", "done_to_stool--other--")] %>%
#   left_join(SL_sub[,c("identifier","main_source_water", "water_source_other_purpose","store_drinking_water",
#                       "stored_observed", "treatment_drinking_water","treatment_observe",
#                       "toilet_facility_type", "done_to_stool")])
# 
# write.csv(test, here("output", "SL_test.csv"))

# write.csv(SL_sub, here("output", "SL_subset.csv"))



