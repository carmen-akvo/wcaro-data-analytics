library(httr) 
library(jsonlite)

## Only the first impact indicator (improved health, nutrition and well being .. ) + children visible (4 indicators)

api_key <- "a67b5b463a5823096469c41edb29651bafd17b7a"
call <- "http://rsr.akvo.org/rest/v1"
topic <- "results_framework_lite"

general_page <- "?page=8"
general_project <- "8643"
cotedivore_project <- "8646"
niger_project <- "8811"

dataset_page8 <- GET(paste(call, topic, general_page, sep="/"), add_headers(Authorization = paste("Token", api_key)))

RSRpage8 <- jsonlite::fromJSON(content(dataset, as="text")) %>% as.data.frame()
ASWA_general_indi <- RSRpage8[RSRpage8$results.project == general_project, ]$results.indicators

# 8646 DGIS ASWA II (Cote d’Ivoire)
ASWA_cotedivore <- RSRpage8[RSRpage8$results.project == cotedivore_project, ]$results.indicators
ASWA_cotedivore %>% 
  as.data.frame() %>% 
  select("id", "parent_indicator", "title", "baseline_year", "baseline_value", "target_value", "baseline_score", "target_score", "result") %>% left_join()

  
# 8811 DGIS ASWA II (Niger)

page <- "?page=10"
data_json <- GET(paste(call, topic, page, sep="/"), add_headers(Authorization = paste("Token", api_key)))
data_set <- jsonlite::fromJSON(content(dataset, as="text")) %>% as.data.frame()

