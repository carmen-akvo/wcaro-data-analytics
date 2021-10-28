## Load required packages 
library(forcats)
library(vcd)
library(sjPlot)

## Create copy of dataset for mosaic plots 
SL_mosaic <- SL_sub 

## Merge catagories with frequency < 5% into "Other" 

## WATER
SL_mosaic$main_source_water_merged <- fct_lump(SL_mosaic$main_source_water, prop= 0.05)
SL_mosaic$round_trip_minutes_merged <- fct_lump(SL_mosaic$round_trip_minutes, prop= 0.05)
SL_mosaic$fetch_water_merged <- fct_lump(SL_mosaic$fetch_water, prop= 0.05) 
SL_mosaic$container_type_merged <- fct_lump(SL_mosaic$`container type`, prop= 0.05) 
SL_mosaic$store_drinking_water_merged <- fct_lump(SL_mosaic$store_drinking_water, prop= 0.05) 
SL_mosaic$treatment_drinking_water_merged <- fct_lump(SL_mosaic$treatment_drinking_water, prop= 0.05)

## SANITATION
SL_mosaic$toilet_facility_type_merged <- fct_lump(SL_mosaic$toilet_facility_type, prop= 0.05)
SL_mosaic$share_facility_merged <- fct_lump(SL_mosaic$share_facility, prop= 0.05)
SL_mosaic$done_to_stool_merged <- fct_lump(SL_mosaic$done_to_stool, prop= 0.05)

## Rename factor level for Surface water 
levels(SL_mosaic$main_source_water_merged)[levels(SL_mosaic$main_source_water_merged) == "Surface water (river, dam, lake, pond, stream, canal, irrigation channels)"] <- "Surface water" 

## MOSAIC PLOTS

## WATER

## Main source water

## Create a contingency table 
SL_mosaic %>%
  sjtab(fun = "xtab", var.labels=c("risk level ecoli", "main_source_water_merged"),
        show.row.prc=T, show.col.prc=T, show.summary=T, show.exp=T, show.legend=T)

## The abbreviation table helps to understand the mosaic plot
abbrev_source <- data.frame("Abbreviation" = c("Pdw", "Sw", "Udw", "Us", "O"),
                                "Category" = c("Protected dug well",
                                               "Surface water", "Unprotected dug well",
                                               "Unprotected spring", "Other")) 
## Save the mosaic plot into a PDF 
pdf(here("figures", "mosaic_watersource.pdf")) 

mosaic(~ main_source_water_merged + risk_level_ecoli, data = SL_mosaic, 
       labeling_args=list(set_varnames = c(risk_level_ecoli = "Risk level E.coli",
                                           main_source_water_merged = "Water source"),
                          gp_labels=gpar(fontsize=8, fontface=3), 
                          gp_varnames=gpar(fontsize=9, fontface=2, col="grey50")),
       shade=TRUE, direction="v", rot_labels=c(0,90,0,0), 
       legend=legend_resbased(fontsize=9),
       abbreviate=TRUE)

dev.off() 

## Round trip in minutes
 
abbrev_trip <- data.frame("Abbreviation" = c("L30", "M30", "Wop", "O"),
                            "Category" = c("Less than 30 minutes",
                                           "More than 30 minutes but less than 1 hr", "Water on premises",
                                           "Other")) 


pdf(here("figs", "mosaic_trip.pdf")) 

mosaic(~ round_trip_minutes_merged + "risk level ecoli", data = SL_mosaic, color=1:5,
       labeling_args=list(set_varnames = c("risk level ecoli" = "Risk level E.coli",
                                           round_trip_minutes_merged = "Trip in minutes"),
                          set_labels=list(round_trip_minutes_merged=c("L30", "M30", "Wop", 
                                                                        "O")), 
                          gp_labels=gpar(fontsize=8, fontface=3), 
                          gp_varnames=gpar(fontsize=9, fontface=2, col="grey50")),
       shade=TRUE, direction="v", rot_labels=c(0,90,0,0), 
       legend=legend_resbased(fontsize=9),
       abbreviate=TRUE)

dev.off() 

## Container type

abbrev_container <- data.frame("Abbreviation" = c("1lb", "1lj", "2lb", "2lj", "5lj"),
                               "Category" = c("10 liter buckets", "10 liter jerrycans",
                                         "20 liter buckets", "20 liter jerrycans",
                                         "5 liter jerrycans")) 

pdf(here("figs", "mosaic_container.pdf")) 

mosaic(~ container_type_merged + risk_level_ecoli, data = SL_mosaic, 
       labeling_args=list(set_varnames = c(risk_level_ecoli = "Risk level E.coli",
                                           container_type_merged = "Container type"),
                          gp_labels=gpar(fontsize=8, fontface=3), 
                          gp_varnames=gpar(fontsize=9, fontface=2, col="grey50")),
       shade=TRUE, direction="v", rot_labels=c(0,90,0,0), 
       legend=legend_resbased(fontsize=9),
       abbreviate=TRUE)

dev.off() 

## Storage type
abbrev_storage <- data.frame("Abbreviation" = c("CLS", "CL", "C", "O"),
                               "Category" = c("In container with lid and spigot", "In container with lid but no spigot/tap",
                                              "In container with no lid or cover",
                                              "Other")) 

pdf(here("figs", "mosaic_storage.pdf")) 

mosaic(~ store_drinking_water_merged + risk_level_ecoli, data = SL_mosaic, 
       labeling_args=list(set_varnames = c(risk_level_ecoli = "Risk level E.coli",
                                           store_drinking_water_merged = "Storage water"),
                          gp_labels=gpar(fontsize=8, fontface=3), 
                          gp_varnames=gpar(fontsize=9, fontface=2, col="grey50")),
       shade=TRUE, direction="v", rot_labels=c(0,90,0,0), 
       legend=legend_resbased(fontsize=9),
       abbreviate=TRUE)

dev.off() 

## Treatment

abbrev_treatment <- data.frame("Abbreviation" = c("Nt", "SaS", "O"),
                             "Category" = c("No treatment", "Stand and settle",
                                            "Other")) 

pdf(here("figs", "mosaic_treatment.pdf")) 

mosaic(~ treatment_drinking_water_merged + risk_level_ecoli, data = SL_mosaic, 
       labeling_args=list(set_varnames = c(risk_level_ecoli = "Risk level E.coli",
                                           treatment_drinking_water_merged = "Treatment water"),
                          gp_labels=gpar(fontsize=8, fontface=3), 
                          gp_varnames=gpar(fontsize=9, fontface=2, col="grey50")),
       shade=TRUE, direction="v", rot_labels=c(0,90,0,0), 
       legend=legend_resbased(fontsize=9),
       abbreviate=TRUE)

dev.off() 

## SANITATION

## Toilet type
abbrev_toilet <- data.frame("Abbreviation" = c("BF", "PLC", "PLO", "PLW",
                                              "O"),
                            "Category" = c("Bush or field", "Pit latrine with concrete slab", 
                                           "Pit latrine with other type of slab", "Pit latrine without slab/open pit",
                                           "Other")) 

pdf(here("figs", "mosaic_toilet_type.pdf")) 

mosaic(~ toilet_facility_type_merged + risk_level_ecoli, data = SL_mosaic, 
       labeling_args=list(set_varnames = c(risk_level_ecoli = "Risk level E.coli",
                                           toilet_facility_type_merged = "Toilet type"),
                          set_labels=list(toilet_facility_type_merged=c("BF", "PLC", "PLO", "PLW", 
                                                                        "O")), 
                          gp_labels=gpar(fontsize=8, fontface=3), 
                          gp_varnames=gpar(fontsize=9, fontface=2, col="grey50")),
       shade=TRUE, direction="v", rot_labels=c(0,90,0,0), 
       legend=legend_resbased(fontsize=9),
       abbreviate=TRUE)

dev.off() 

## Toilet shared
abbrev_toilet_shared <- data.frame("Abbreviation" = c("Y", "N"),
                            "Category" = c("Yes", "No"))

pdf(here("figs", "mosaic_toilet_shared.pdf")) 

mosaic(~ share_facility_merged + risk_level_ecoli, data = SL_mosaic, 
       labeling_args=list(set_varnames = c(risk_level_ecoli = "Risk level E.coli",
                                           share_facility_merged = "Share toilet"),
                          gp_labels=gpar(fontsize=8, fontface=3), 
                          gp_varnames=gpar(fontsize=9, fontface=2, col="grey50")),
       shade=TRUE, direction="v", rot_labels=c(0,90,0,0), 
       legend=legend_resbased(fontsize=9),
       abbreviate=TRUE)

dev.off() 

## Done to stool

abbrev_stool <- data.frame("Abbreviation" = c("TiG", "O"),
                                   "Category" = c("Thrown into garbage", "Other"))

pdf(here("figs", "mosaic_stool.pdf")) 

mosaic(~ done_to_stool_merged + risk_level_ecoli, data = SL_mosaic, 
       labeling_args=list(set_varnames = c(risk_level_ecoli = "Risk level E.coli",
                                           done_to_stool_merged = "Done to stool"),
                          gp_labels=gpar(fontsize=8, fontface=3), 
                          gp_varnames=gpar(fontsize=9, fontface=2, col="grey50")),
       shade=TRUE, direction="v", rot_labels=c(0,90,0,0), 
       legend=legend_resbased(fontsize=9),
       abbreviate=TRUE)

dev.off() 
                                                
## Tutorial:
## http://www.datavis.ca/courses/VCD/vcd-tutorial.pdf 
