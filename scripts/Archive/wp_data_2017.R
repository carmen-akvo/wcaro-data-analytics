library(dplyr)
library(data.table)
library(readxl)
library(ggplot2) 
library(here) 
library(stringr)
library(tidyr)
library(DescTools)

## Read data
wp_data <- read_excel(here("data", "Water-point-data-SL.xlsx"))

names(wp_data) <- strsplit(names(wp_data), "\\|") %>% 
  as.data.frame() %>% 
  t %>% 
  data.frame(stringsAsFactors = F) %>% 
  pull(2)

wp_data <- wp_data %>% 
  filter(!is.na(Latitude)) %>%
  select("Location",             
         "Latitude",                                                                      
         "Longitude",
         "Geo Code",
         "Type of water point",
         "Extraction system type",
         "Pump type",
         "Water point Functionality") %>%
  separate(Location, c("Regions", "Districts", "Chiefdoms", "Communities"), sep = "\\|") %>%
  separate(`Type of water point`, c("Type of water point (num)","Type of water point"), sep = ":") %>%
  separate(`Extraction system type`, c("Extraction system type (num)","Extraction system type"), sep = ":") %>%
  separate(`Pump type`, c("Pump type (num)","Pump type"), sep = ":") %>%
  separate(`Water point Functionality`, c("Water point Functionality (num)","Water point Functionality"), sep = ":")

write.csv2(wp_data, here("data", "wp_data_2017.csv"), row.names = FALSE)
