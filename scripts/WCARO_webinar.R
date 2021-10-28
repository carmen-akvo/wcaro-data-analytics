library(dplyr)
library(data.table)
library(readxl)
library(ggplot2) 
library(here) 
library(stringr)
library(tidyr)

## Akvo colour scheme
# Akvo Orange red:	    HEX #EA5547 rgb(234,85,71)
# Akvo Blue:      	    HEX #404898 rgb(64,72,152)
# Akvo Turquoise green:	HEX #03AD8C rgb(3,173,140)
# Akvo Pink:          	HEX #E04D95 rgb(224,77,149)

# Blue to red
# RGB gradient: "#EA5547","#C85257","#A64F67","#844D77","#624A87","#404898"
# HSV gradient: "#EA5547","#D9477A","#C946AD","#9F45B8","#6542A8","#404898"

## Akvo fonts
# Roboto Condensed
# Assistant

## Read data
household <- read.csv(here("output", "SL_subset.csv")) 
wells <- read.csv(here("output", "SL_wells.csv"), sep=';')

## 2017 - water source type
SL_2017_type <- melt(read_excel(here("data", "SL_2017_main_findings.xlsx"), sheet="Water source"))
names(SL_2017_type) <- c("Location", "Source type", "Percentage")
SL_2017_type$Improved <- c(rep("Improved", 24), rep("Unimproved", 8))

ggplot(data=SL_2017_type, aes(x=Improved, y=Percentage, fill=Location)) +
  geom_bar(stat="identity", position=position_dodge()) +
  coord_flip() + theme_minimal() + 
  scale_fill_manual(values=c('#404898','#03AD8C')) + 
  labs(title="Improved sources", x ="", y = "Frequency (%)") +
  theme(plot.title = element_text("Roboto"))

## 2017 - Time to source
SL_2017_time <- melt(read_excel(here("data", "SL_2017_main_findings.xlsx"), sheet="Time to source"))
names(SL_2017_time) <- c("Location", "Time to source", "Percentage")

ggplot(data=SL_2017_time, aes(x=`Time to source`, y=Percentage, fill=Location)) +
  geom_bar(stat="identity", position=position_dodge()) +
  coord_flip() + theme_minimal() + 
  scale_fill_manual(values=c('#404898','#03AD8C')) + 
  labs(title="distance between HH and source", x ="", y = "Frequency (%)") +
  theme(plot.title = element_text("Roboto"))


## 2017 - Unsufficient water
SL_2017_amount <- melt(read_excel(here("data", "SL_2017_main_findings.xlsx"), sheet="Sufficient water"))
names(SL_2017_amount) <- c("Location", "Unsufficient water", "Percentage")
SL_2017_amount <- SL_2017_amount[!SL_2017_amount$`Unsufficient water` %in% 
                                   c("Total nr of HH", "HH unable to find sufficient water"),]

ggplot(data=SL_2017_amount, aes(x=`Unsufficient water`, y=Percentage, fill=Location)) +
  geom_bar(stat="identity", position=position_dodge()) +
  coord_flip() + theme_minimal() + 
  scale_fill_manual(values=c('#404898','#03AD8C')) + 
  labs(title="HH with unsufficient water", x ="", y = "Frequency (%)") +
  theme(plot.title = element_text("Roboto"))


## 2017 - E. Coli contamination
SL_2017_ecoli <- melt(read_excel(here("data", "SL_2017_main_findings.xlsx"), sheet="E. Coli"))
names(SL_2017_ecoli) <- c("Location", "E.Coli risk", "Percentage")

ggplot(data=SL_2017_ecoli, aes(x=`E.Coli risk`, y=Percentage, fill=Location)) +
  geom_bar(stat="identity", position=position_dodge()) +
  coord_flip() + theme_minimal() + 
  scale_fill_manual(values=c('#404898','#03AD8C')) + 
  labs(title="E. Coli contamination", x ="", y = "Frequency (%)") +
  theme(plot.title = element_text("Roboto", size=12),
        axis.text.y = element_text("Roboto", size=10),
        axis.title.x = element_text("Roboto", size=10))


# 2019 data

# E coli household
hh_ecoli <- melt(table(household$risk.level.ecoli), varnames="risk level E.coli")
hh_ecoli$Percentage <- hh_ecoli$value/sum(hh_ecoli$value)*100

hh_ecoli$`risk level E.coli` <- factor(
  hh_ecoli$`risk level E.coli`, levels=
    c("Very High Risk / Unsafe",
      "High Risk / Possibly Unsafe", "High Risk / Probably Unsafe",
      "Intermediate Risk / Possibly Safe", "Intermediate Risk / Probably Safe", 
      "Low Risk / Safe" ))

ggplot(data=hh_ecoli, aes(x=`risk level E.coli`, y=Percentage, fill=`risk level E.coli`)) +
  scale_fill_manual(values=c("#404898","#335C95","#277093","#1B8490","#0F988E","#03AD8C")) +
  coord_flip() + theme_minimal() +
  geom_bar(stat="identity") +
  labs(title="", x ="", y = "Frequency (%)") +
  theme(plot.title = element_text("Roboto"),
        axis.title.x  = element_text("Roboto", size=11),
        axis.text.y  = element_text("Roboto", size=11),
        # axis.text.y  = element_blank()
        ) +
  theme(legend.position = "none")


# E coli wells
wells <- wells %>% rename("risk.level.ecoli" = names(wells)[100])
  
wells_ecoli <- melt(table(wells$risk.level.ecoli), varnames="risk level E.coli")
wells_ecoli$Percentage <- wells_ecoli$value/sum(wells_ecoli$value)*100

wells_ecoli$`risk level E.coli` <- factor(
  wells_ecoli$`risk level E.coli`, levels=c("Very High Risk / Unsafe",
    "High Risk / Possibly Unsafe", "High Risk / Probably Unsafe",
    "Intermediate Risk / Possibly Safe", "Intermediate Risk / Probably Safe", 
    "Low Risk / Safe" ))

ggplot(data=wells_ecoli, aes(x=`risk level E.coli`, y=Percentage, fill=`risk level E.coli`)) +
  scale_fill_manual(values=c("#404898","#335C95","#277093","#1B8490","#0F988E","#03AD8C")) +
  coord_flip() + theme_minimal() +
  geom_bar(stat="identity") +
  labs(title="", x ="", y = "Frequency (%)") +
  theme(plot.title = element_text("Roboto"),
        axis.title.x  = element_text("Roboto", size=11),
        axis.text.y  = element_text("Roboto", size=11), 
        axis.text.x  = element_text("Roboto", size=11)) +
  theme(legend.position = "none")

# 404898, 335C95, 277093, 1B8490, 0F988E, 03AD8C

# E coli and lids
ecoli_lids <- melt(table(household[,c("store_drinking_water","risk.level.ecoli")]))
ecoli_lids <- ecoli_lids[ecoli_lids$value > 10,]

ecoli_lids$`risk.level.ecoli` <- factor(
  ecoli_lids$`risk.level.ecoli`, 
  levels=c("Very High Risk / Unsafe",
           "High Risk / Possibly Unsafe", "High Risk / Probably Unsafe",
           "Intermediate Risk / Possibly Safe", "Intermediate Risk / Probably Safe", 
           "Low Risk / Safe" ))

ecoli_lids$store_drinking_water <- factor(
  ecoli_lids$store_drinking_water, 
  levels=c("in container with lid and spigot", "in container with lid but no spigot/tap",
       "in narrow-mouthed container","in container with no lid or cover"))

ggplot(data=ecoli_lids, aes(x=store_drinking_water, y=value, fill=risk.level.ecoli)) +
  # scale_fill_manual(values=c("#404898","#335C95","#0F988E","#03AD8C")) +
  coord_flip() + theme_minimal() +
  geom_bar(stat="identity", position = "fill") +
  labs(title="", x ="", y = "Frequency (%)", legend = "Drinking water storage") +
  theme(plot.title = element_text("Roboto"),
        axis.title.x  = element_text("Roboto", size=11),
        axis.text.y  = element_text("Roboto", size=11), 
        axis.text.x  = element_text("Roboto", size=11)) +
  theme(legend.title = "Drinking water storage")


## hand wash observed
library(splitstackshape) 

hand_wash_ob <- melt(table(household[,c("hand_wash_observe","risk.level.ecoli")]))
diarrhea <- melt(table(household[,c("diarrhea_two_weeks","risk.level.ecoli")]))
stool <- household %>% cSplit("done_to_stool","|") %>%
  dplyr::select(risk.level.ecoli, starts_with("done_to_stool"))

stool <- rbind(
  melt(table(stool[,c("risk.level.ecoli", "done_to_stool..other..")]), varnames=c("E.Coli", "Handling of stool")),
  melt(table(stool[,c("risk.level.ecoli", "done_to_stool_1")]), varnames=c("E.Coli", "Handling of stool")),
  melt(table(stool[,c("risk.level.ecoli", "done_to_stool_2")]), varnames=c("E.Coli", "Handling of stool")),
  melt(table(stool[,c("risk.level.ecoli", "done_to_stool_2")]), varnames=c("E.Coli", "Handling of stool"))
)

stool <- stool %>% 
  group_by(E.Coli, `Handling of stool`) %>%
  summarise(frequency = sum(value))

stool$`Handling of stool` <- factor(
  stool$`Handling of stool`, 
  levels=c("child used toilet/latrine", 
           "put/rinsed into toilet or latrine", 
           "put/rinsed into drain or ditch",
           "thrown into garbage","buried",    
           "left in the open"
  ))

stool$E.Coli <- factor(
  stool$E.Coli, 
  levels=c("Very High Risk / Unsafe",
           "High Risk / Possibly Unsafe", 
           "High Risk / Probably Unsafe",
           "Intermediate Risk / Possibly Safe" , 
           "Intermediate Risk / Probably Safe",
           "Low Risk / Safe" ))

ggplot(data=stool, aes(x=`Handling of stool`, y=frequency, fill=E.Coli )) +
  scale_fill_manual(values=c("#EA5547","#C85257","#A64F67","#844D77","#624A87","#404898")) +
  # scale_fill_manual(values=c("#404898","#624A87","#844D77","#A64F67","#C85257","#EA5547")) +
  coord_flip() + theme_minimal() +
  geom_bar(stat="identity", position = "fill") +
  labs(title="", x ="", y = "Proportion", legend = "Stool handling") +
  theme(plot.title = element_text("Roboto"),
        axis.title.x  = element_text("Roboto", size=11),
        axis.text.y  = element_text("Roboto", size=12), 
        axis.text.x  = element_text("Roboto", size=11)) 


source <- melt(table(household[,c("main_source_water","risk.level.ecoli")]))
source$main_source_water <- as.character(source$main_source_water)
source <- source %>% 
  mutate(
    main_source_water = replace(
      main_source_water, 
      main_source_water == "surface water (river, dam, lake, pond, stream, canal, irrigation channels)",
      "surface water"))

source$main_source_water <- factor(
  source$main_source_water, 
  levels=c("protected dug well", "protected spring",
           "piped water to yard/plot", "piped water into dwelling", 
           "rainwater collection","public tab/standipe", "tubewell/borehole",
           "unprotected dug well", "unprotected spring", "surface water"
  ))

source$risk.level.ecoli <- factor(
  source$risk.level.ecoli, 
  levels=c("Low Risk / Safe" , "Intermediate Risk / Probably Safe",
           "Intermediate Risk / Possibly Safe" ,
           "High Risk / Possibly Unsafe", "High Risk / Probably Unsafe",
           "Very High Risk / Unsafe"
  )
)

source$risk.level.ecoli <- factor(
  source$risk.level.ecoli, 
  levels=c("Very High Risk / Unsafe",
           "High Risk / Possibly Unsafe", 
           "High Risk / Probably Unsafe",
           "Intermediate Risk / Possibly Safe" , 
           "Intermediate Risk / Probably Safe",
           "Low Risk / Safe" )
)

source <- source[!is.na(source$main_source_water),]

ggplot(data=source, aes(x=main_source_water, y=value, fill=risk.level.ecoli )) +
  scale_fill_manual(values=c("#EA5547","#C85257","#A64F67","#844D77","#624A87","#404898")) +
  # scale_fill_manual(values=c("#404898","#624A87","#844D77","#A64F67","#C85257","#EA5547")) +
  coord_flip() + theme_minimal() +
  geom_bar(stat="identity", position = "fill") +
  labs(title="", x ="", y = "Proportion of tested households", fill = "E. Coli risk level") +
  theme(plot.title = element_text("Roboto"),
        axis.title.x  = element_text("Roboto", size=12),
        axis.text.y  = element_text("Roboto", size=14), 
        axis.text.x  = element_text("Roboto", size=12)) 


