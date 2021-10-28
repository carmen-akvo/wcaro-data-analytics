## Check the order of the risk levels based on acutal ecoli scores
SL_ecoli <- SL_ecoli %>%
  rename("risk_level_ecoli" = "ecoli_result_incubation|E.coli test result AFTER incubation |Health Risk Category (Based on MPN and Confidence Interval)",
         "score_ecoli_num" = "--CADDISFLY--|MPN(MPN/100ml)") 

SL_ecoli$score_ecoli_num <- gsub( ">", "", paste(SL_ecoli$score_ecoli_num))

SL_ecoli$score_ecoli_num <- as.numeric(SL_ecoli$score_ecoli_num)

SL_risk <- SL_ecoli %>% 
  select("risk_level_ecoli", 
         "score_ecoli_num") %>%
  group_by(risk_level_ecoli) %>%
  summarise(mean_ecoli=mean(score_ecoli_num)) %>%
  arrange(mean_ecoli) 

## Convert to data table and risk level as factors 
SL_sub <- as.data.table(SL_sub)
SL_sub$risk_level_ecoli <- as.factor(SL_sub$risk_level_ecoli)

SL_sub$risk_level_ecoli <- factor(SL_sub$risk_level_ecoli, 
                                  levels = c("Low Risk / Safe", 
                                             "Intermediate Risk / Probably Safe",
                                             "Intermediate Risk / Possibly Safe",
                                             "High Risk / Possibly Unsafe",
                                             "High Risk / Probably Unsafe",
                                             "Very High Risk / Unsafe")) 

## Bar graph: Risk level 
ggplot(SL_sub, aes(risk_level_ecoli))+
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  geom_text(stat='count', aes(label=..count..), vjust=-1) +
  xlab("Risk level ecoli") 

## Due to the relative low counts in levels (except "Very high risk")
## it is chosen to merge Probably Safe/Possibly Safe and
## Possibly Unsafe/Probably Unsafe 

## New categories: Low Risk, Intermediate Risk, High Risk, Very High Risk
SL_sub$risk_level_ecoli <- sub("/.*", "", SL_sub$risk_level_ecoli) %>%
  trimws(which = "both") 

SL_sub$risk_level_ecoli <- as.factor(SL_sub$risk_level_ecoli)

SL_sub$risk_level_ecoli <- factor(SL_sub$risk_level_ecoli,
                                  levels=c("Low Risk", "Intermediate Risk",
                                           "High Risk", "Very High Risk")) 

SL_sub$stored_observed <- as.factor(SL_sub$stored_observed)
levels(SL_sub$stored_observed)
