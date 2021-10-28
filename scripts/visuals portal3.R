library(here)
library(tidyverse)
library(jsonlite)
library(ggplot2)

# Data
rsrdata <- fromJSON("https://rsr2.akvotest.org/rest/v1/project/8810/results_framework/?format=json")

# [1] "New water users (Repaired/Rehabilitated water points, new water points (boreholes fitted with HP and Small scale water supply networks)"
ind_1 <- rsrdata[["results"]][['indicators']][[1]][['periods']][[1]] %>% 
  select(actual_value, target_value, period_end) %>% 
  gather(value.type, value, -period_end)

ind_1[ind_1==0] <- NA

ggplot(data = ind_1,
       aes(x=period_end, y=value, label = value, 
           group=value.type, linetype=value.type, color=value.type)) +
  geom_line()+
  geom_point() +
  theme_light() +
  scale_color_manual(values = c("#58508d", "#6aaa96")) +
  labs(title = "New water users (Repaired/Rehabilitated water points,\nnew water points (boreholes fitted with HP and Small scale water supply networks)",
       x="Year of measurement",y= "Percentage of total water points", fill="") +
  theme(plot.title = element_text(family="Assistant-Regular", size=12),
        axis.title = element_text(family="Assistant-Regular", size=12),
        axis.text = element_text(family="Assistant-Regular", size=10, angle = 90, vjust = 0.5, hjust=1),
        legend.text = element_text(family="Assistant-Regular", size=12),
        legend.title = element_blank()) +
  geom_text(data = ind_1 %>% filter(value != 0), 
            nudge_y = 20000, size=3, family = "Assistant-Regular", color="black") +
  geom_vline(xintercept=as.numeric(as.Date("2020-12-31")), linetype=4)
  geom_vline(xintercept = Sys.Date(), linetype="dotted", 
             color = "blue", size=1.5)

# [2] "New users household water treatment"                                                                                                    
ind_2 <- rsrdata[["results"]][['indicators']][[1]][['periods']][[2]]

# [3] "Proportion of water points functioning within the last semester" 
ind_3 <- rsrdata[["results"]][['indicators']][[1]][['periods']][[3]]

# ODF
ODF <- data.frame("value" = c(3000, 6300, 7600, 2500, 4000, 8000),
                  "year" = c(2019, 2020, 2021, 2019, 2020, 2021),
                  "odf" = c("value","value","value", "target","target","target")) %>%
  mutate(odf = factor(odf, levels=c( "value", "target")))

rehab_wp <- data.frame(
  "Period end" = as.Date(c("2022-01-01", "2021-01-01", "2020-01-01", "2019-08-01")),
  "Target"	= c(100,100,100,100),
  "Actual" = c(NA,80,100,100)) %>% 
  gather(value.type, value, -Period.end)

access_to_water <- data.frame(
  "Period start" = as.Date(c("2022-01-01", "2021-01-01", "2020-01-01", "2019-08-01")),
  "Period end" = as.Date(c("2022-12-31", "2021-12-31", "2020-12-31", "2019-12-31")),
  "Target"	= c(0,0,0,400000),
  "Actual" = c(0,0,194978,169550)) %>% 
  gather(value.type, value, -Period.start, -Period.end)

# REHABILITATE WP
ggplot(data = rehab_wp,
       aes(x=`Period.end`, y=value, label = paste0(value, "%"), 
           group=value.type, linetype=value.type, color=value.type)) +
  geom_line()+
  geom_point() +
  theme_light() +
  ylim(0, 100)+
  scale_color_manual(values = c("#58508d", "#6aaa96")) +
  labs(title = "", x="Year of measurement",y= "Percentage of total water points", fill="") +
  theme(axis.title = element_text(family="Assistant-Regular", size=12),
        axis.text = element_text(family="Assistant-Regular", size=12),
        legend.text = element_text(family="Assistant-Regular", size=12),
        legend.title = element_blank()) +
  geom_text(nudge_y = -5, size=4, family = "Assistant-Regular", color="black")

# ACCESS TO WATER
ggplot(data = access_to_water,
       aes(x=`Period.end`, y=value, label = value, group=value.type, linetype=value.type, color=value.type)) +
  geom_line()+
  geom_point() +
  theme_light() +
  scale_color_manual(values = c("#58508d", "#6aaa96")) +
  labs(title = "", x="Year of measurement",y= "Number of people", fill="") +
  theme(axis.title = element_text(family="Assistant-Regular", size=12),
        axis.text = element_text(family="Assistant-Regular", size=12),
        legend.text = element_text(family="Assistant-Regular", size=12),
        legend.title = element_blank()) +
  scale_y_continuous(labels = scales::comma) +
  geom_text(nudge_y = 10000, size=3, family = "Assistant-Regular")

# ODF
ggplot(ODF, aes(x=year, y=value, group=odf, label=value, color=odf)) +
  geom_line(aes(linetype=odf))+
  geom_point() +
  theme_light() +
  scale_color_manual(values = c("#58508d", "#6aaa96")) +
  labs(title = "", x="Year",y= "Number of communities", fill="") +
  theme(axis.title = element_text(family="Assistant-Regular"),
        axis.text = element_text(family="Assistant-Regular")) +
  theme(axis.title = element_text(family="Assistant-Regular", size=12),
        axis.text = element_text(family="Assistant-Regular", size=12),
        legend.text = element_text(family="Assistant-Regular", size=12),
        legend.title = element_blank()) +
  scale_y_continuous(labels = scales::comma) +
  geom_text(nudge_y = 250, size=4, family = "Assistant-Regular")+
  scale_x_continuous(name ="Year", 
                   limits=c(2019,2021),
                   breaks=c(2019,2020, 2021))

basic_san <- data.frame(
  "male" = c(10000, 30000, 76000, 15000, 40000, 80000),
  "female" = c(15000, 17000, 48000, 15000, 35000, 80000),
  "year" = c(2019, 2020, 2021, 2019, 2020, 2021),
  "odf" = c( "value","value","value", "target", "target", "target" )
) %>%
  mutate(odf = factor(odf, levels=c( "value", "target"))) %>% 
  gather(key, value, -year, -odf) %>%
  mutate(group = paste0(odf, "_", key))


ggplot(data=basic_san, aes(x=year, y=value, group=group, linetype=odf, color=key, label=value)) +
  geom_line() +
  # geom_line(data=basic_san, aes(x=year, y=female, group=odf, linetype=odf), colour="#ff6361") +
  geom_point() +
  theme_light() +
  geom_text(nudge_y = 2000, size=3, family = "Assistant-Regular") +
  scale_color_manual(values = c("#003f5c", "#ff6361", "#003f5c", "#ff6361")) +
  labs(title = "", x="Year",y= "Number of people", fill="") +
  theme(axis.title = element_text(family="Assistant-Regular"),
        axis.text = element_text(family="Assistant-Regular")) +
  scale_x_continuous(name ="Year", 
                     limits=c(2019,2021),
                     breaks=c(2019,2020, 2021)) +
  theme(axis.title = element_text(family="Assistant-Regular"),
        axis.text = element_text(family="Assistant-Regular")) +
  theme(axis.title = element_text(family="Assistant-Regular", size=12),
        axis.text = element_text(family="Assistant-Regular", size=12),
        legend.text = element_text(family="Assistant-Regular", size=12),
        legend.title = element_blank()) +
  scale_y_continuous(labels = scales::comma) 
