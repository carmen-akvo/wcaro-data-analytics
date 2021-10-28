
functionality_var <- melt(
  table(puits$region, puits$functionality_main), 
  varnames = c("ADM1_FR", "functionality")) %>% 
  filter(value != 0)

functionality_var <- functionality_var %>%
  left_join(functionality_var %>% 
              group_by(ADM1_FR) %>% 
              summarise(total = sum(value))) %>%
  filter(functionality == "fonctionnel") %>%
  mutate(ADM1_FR = str_to_title(ADM1_FR)) %>%
  mutate(Percentage = round((value/total)*100,0))

mli.shape <-read_sf(dsn = here::here("Mali/data/shapefiles/mli_adm_1m_dnct_2019_shp/", 
                                     "mli_admbnda_adm1_1m_dnct_20190802.shp"))

shape.data <- ggplot2::fortify(mli.shape, region='ADM1_FR') %>%
  # mutate(ADM1_FR = tolower(ADM1_FR)) %>%
  mutate(ADM1_FR = gsub("Ségou", "Segou", ADM1_FR)) %>%
  left_join(functionality_var) %>% 
  select(ADM1_FR, value, total, Percentage)

bins <- c(200, 300, 500, 1000, 2000, 4000, 6000, 8000)
pal <- colorBin("YlOrRd", domain = shape.data$value, bins = bins)

leaflet(shape.data, width="100%") %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(
    fillColor = ~pal(value),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7) %>% 
  addLegend(pal = pal, values = ~value, opacity = 0.7,
            title = "Nr of functional WP",
            position = "bottomright")
