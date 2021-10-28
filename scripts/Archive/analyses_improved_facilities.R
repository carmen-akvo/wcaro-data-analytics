library(ggplot2)
library(dplyr) 
library(here)

## Based on UNICEF Standardized WASH Survey, create two new columns that
## refer to Improved/Unimproved water/sanitation facilities 

## WATER FACILITIES

SL_sub$main_source_water <- as.factor(SL_sub$main_source_water)

SL_sub$drinking_facilities_status <- "improved" 
levels(SL_sub$drinking_facilities_status) <- c(levels(SL_sub$drinking_facilities_status), "unimproved") 

SL_sub$drinking_facilities_status[SL_sub$main_source_water == "piped water into dwelling" |
                                    SL_sub$main_source_water == "piped water to yard/plot" |
                                    SL_sub$main_source_water == "protected dug well" |
                                    SL_sub$main_source_water == "protected spring" |
                                    SL_sub$main_source_water == "public tap/standpipe" |
                                    SL_sub$main_source_water == "rainwater collection" |
                                    SL_sub$main_source_water == "tubewell/borehole" ] <- "improved"

SL_sub$drinking_facilities_status[SL_sub$main_source_water == "unprotected dug well" | 
                                    SL_sub$main_source_water == "unprotected spring" |
                                    SL_sub$main_source_water == "surface water (river, dam, lake, 
                                  pond, stream, canal, irrigation channels)"] <- "unimproved"
## Cross table
ct_drinking_facilities <- as.data.frame(table(SL_sub$drinking_facilities_status, SL_sub$`risk level ecoli`)) %>%
  rename("Drinking facilities" = Var1, "Risk level ecoli" = Var2) %>%
  group_by(`Risk level ecoli`) %>% 
  mutate(Perc = round(Freq/sum(Freq)*100, 2))

## Bar plot
ggplot(ct_drinking_facilities, aes(x=`Risk level ecoli`,
                                 y = Perc, fill = `Drinking facilities`)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#C4EE82", "#EE6650")) +
  geom_text(aes(label= Freq, vjust=2)) +
  labs(title="Distribution of water facilities per risk level",
       x="Risk level E.coli", y="Percentage", fill = "Category") +
  coord_flip()

ggsave(here("figures", "water_improved.pdf"))

## SANITATION FACILITIES

SL_sub$toilet_facilities_status <- "improved" 
levels(SL_sub$toilet_facilities_status) <- c(levels(SL_sub$dtoilet_facilities_status), "unimproved")

SL_sub$toilet_facility_type <- as.factor(SL_sub$toilet_facility_type) 
SL_sub$flush_to <- as.factor(SL_sub$flush_to) 

SL_sub$toilet_facilities_status[SL_sub$toilet_facility_type == "ventilated improved pit latrine (VIP)" |
                                  SL_sub$toilet_facility_type == "pit latrine with concrete slab" |
                                  SL_sub$toilet_facility_type == "pit latrine with other type of slab" |
                                  SL_sub$toilet_facility_type == "composting toilet" |
                                  SL_sub$flush_to == "piped sewer system" |
                                  SL_sub$flush_to == "pit latrine" |
                                  SL_sub$flush_to == "septic tank"] <- "improved"

SL_sub$toilet_facilities_status[SL_sub$toilet_facility_type == "pit latrine without slab/open pit" |
                                  SL_sub$toilet_facility_type == "bucket" |
                                  SL_sub$toilet_facility_type == "bush or field"] <- "unimproved"

## Cross table 
ct_toilet_facilities <- as.data.frame(table(SL_sub$toilet_facilities_status, SL_sub$`risk level ecoli`)) %>%
  rename("Toilet facilities" = Var1, "Risk level ecoli" = Var2) %>%
  group_by(`Risk level ecoli`) %>% 
  mutate(Perc = round(Freq/sum(Freq)*100, 2))

## Bar plot
ggplot(ct_toilet_facilities, aes(x=`Risk level ecoli`,
                                 y = Perc, fill = `Toilet facilities`)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#C4EE82", "#EE6650")) + 
  geom_text(aes(label= Freq, vjust=2)) +
  labs(title="Distribution of toilet facilities per risk level",
       x="Risk level E.coli", y="Percentage", fill = "Category") +
  coord_flip()

ggsave(here("figures", "sanitation_improved.pdf"))
