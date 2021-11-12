### Determine WP coverage based on facebook population density
## 08-11-2021

# Library
library(here)
library(readxl)
library(sf)
library(sp)
library(rgdal)
library(maps)

### CIRCLES!
wp_2016_processed <- read.csv2(here("data/processed","wp_2016_processed.csv"))

# Selection of wp 2016 data
wp_2016_sub <- wp_2016_processed %>% 
  dplyr::select(#"ADM2", "ADM3", "ADM4", "ADM5",
    "Water.point.Name", 
    "Latitude", "Longitude",
    "Functionality",
    "sdg_improved_source",
    "sphere_nr_people") %>% 
  unique() 

# Convert to sf, set the crs to EPSG:4326 (lat/long), 
# and transform to EPSG:3035
wp_2016_sf <- st_as_sf(
  wp_2016_sub, 
  coords = c( "Longitude", "Latitude"), 
  crs = 4326) %>% 
  st_transform(3035)

# Buffer circles by 100m
wp_2016_circles <- st_buffer(wp_2016_sf, dist = 500)

# Polygon
sl.shape <- read_sf(dsn = here::here("data/raw/SIL_admin_SHP/", "SIL.shp")) %>%
  # filter(ADM2 == "Bombali") %>%
  st_set_crs(4326) %>% 
  st_transform(3035)

# Intersect the circles with the polygons
wp_2016_int_circles <- st_intersection(sl.shape, wp_2016_circles)

# Facebook population
fb_pop_sl <- read.csv2(here("data/raw", "population_sle_2019-07-01.csv"), sep=",")
fb_pop_sl <- fb_pop_sl %>% 
  mutate(ID = paste0("V", rownames(fb_pop_sl)))

# Add ID to population data, which can later by used to identify duplicates. 
# Determine duplicates and divide pop density by the # of duplicates to get density equally dispersed across wp
# Then compare pop dens with wp coverage

# To sf file
pnts_sf <- st_as_sf(fb_pop_sl, 
                    coords = c( "Lon", "Lat"), 
                    crs = 4326) %>% 
  st_transform(3035)

# Link Admin levels to FB density
FB_density_admin <- st_intersection(sl.shape, pnts_sf) 
FB_density_admin <- FB_density_admin %>% select(ADM2, Population, ID.1) 

st_write(FB_density_admin, here("data/processed", "FB density with admin level.shp"))

# Find overlap

# Per waterpoint -> problem: population denstiy is counted double!
kept_points_with_ID <- st_intersection(wp_2016_int_circles, pnts_sf) 
kept_points_with_ID_no_geometry <- kept_points_with_ID %>% st_drop_geometry()

kept_points_with_ID_no_geometry <- kept_points_with_ID_no_geometry %>% 
  select(Water.point.Name, ADM2, sdg_improved_source, sphere_nr_people, ID.1, Population) %>% 
  mutate(Population = as.numeric(Population)) %>%
  unique() 

# Duplicates:
duplicate_pop_density <- kept_points_with_ID_no_geometry %>% 
  group_by(ID.1) %>% 
  summarise(count=n())

kept_points_with_ID_no_geometry <- kept_points_with_ID_no_geometry %>% 
  left_join(duplicate_pop_density) %>%
  mutate(Population = Population/count)  

kept_points_with_ID_no_geometry <- kept_points_with_ID_no_geometry %>% 
  group_by(Water.point.Name, ADM2, sphere_nr_people) %>% 
  summarise(Population_within_circle = sum(Population, na.rm=TRUE)) 

# Show percentage of population served within the 500m radius 
# and show percentage of the population that lives outside of the 500m radius
wp_coverage_fb_density <- FB_density_admin %>% 
  st_drop_geometry() %>%
  mutate(Population = as.numeric(Population)) %>%
  group_by(ADM2) %>% 
  summarise(Total_pop_fb = sum(Population)) %>%
  left_join(kept_points_with_ID_no_geometry %>% 
              group_by(ADM2) %>%
              summarise(sphere_nr_people = sum(sphere_nr_people, na.rm=TRUE),
                        Population_within_circle = sum(Population_within_circle, na.rm=TRUE))) %>% 
  mutate(Population_outside_circle = Total_pop_fb - Population_within_circle) %>% 
  mutate(Percentage_served_within_circle = round(sphere_nr_people/Population_within_circle,2)*100) %>% 
  mutate(Percentage_pop_outside_circle = round(Population_outside_circle/Total_pop_fb,2)*100) %>% 
  mutate(actual_percentage_served = ifelse(Percentage_served_within_circle>100, 100, Percentage_served_within_circle)) %>%
  mutate(actual_pop_served = Population_within_circle*(actual_percentage_served/100)) %>% 
  mutate(percentage_population_served = round(actual_pop_served/Total_pop_fb,2)*100) #%>% View()
  #write.csv2(here("data/processed", "wp_coverage_estimates_fb_pop.csv"), dec=".")

# Plot percentage of the population covered by wp within 500m radius
sl.shape.data.percentage.reach <- sl.shape.data %>% 
  left_join(wp_coverage_fb_density %>% select(ADM2, percentage_population_served)) %>%
  mutate(percentage_population_served = replace_na(percentage_population_served, 0))

sl.shape.data.percentage.reach <- unique(
  sl.shape.data.percentage.reach[,c("ADM2", "geometry", 
                                    "percentage_population_served")]) %>%
  group_by(ADM2) %>%
  summarise(percentage_population_served = unique(percentage_population_served))

ggplot(sl.shape.data.percentage.reach) +
  
  #add a shapefile layer to the plot, set the line size and colour for the polygons, then fill them according to our data
  geom_sf(size = 0.3, color = "#808080", aes(fill=percentage_population_served)) +
  geom_sf_text(aes(label=paste0(ADM2, "\n", percentage_population_served, "%"))) + #, check_overlap = TRUE) +
  scale_fill_gradient(low="red",high="green", na.value="#F2F2F2")

# Compare to population projections ministry
wp_coverage_fb_density %>% select(ADM2, percentage_population_served) %>% 
  left_join(percentage_reach_2016_sphere_district %>% select(ADM2, Percentage_coverage))
  
population_served <- kept_points_with_ID_no_geometry %>% 
  group_by(ADM2) %>% 
  summarise(sphere_nr_people = sum(sphere_nr_people),
            Population = sum(Population)) %>%
  mutate(percentage_served = round(sphere_nr_people/Population,2)*100)

# Find the population density squares that fall in/out of the water point reach
pnts_sf_not_covered <- pnts_sf %>%
  mutate(covered_by_wp = ifelse(
    geometry %in% unique(kept_points_with_ID$geometry), 
    "yes", "no")) %>%
  filter(covered_by_wp == "no") %>%
  mutate(percentage_served = 0) %>%
  mutate(Population = as.numeric(Population))

## ONLY FUNCTIONAL

functional_kept_points_with_ID_no_geometry <- kept_points_with_ID %>% 
  st_drop_geometry() %>%
  filter(Functionality %in% c("Yes – Functional (and in use)", 
                              "Yes – Functional (but not in use)",
                              "Yes - But damaged" ))

functional_kept_points_with_ID_no_geometry <- functional_kept_points_with_ID_no_geometry %>% 
  
  select(Water.point.Name, ADM2, sdg_improved_source, sphere_nr_people, ID.1, Population) %>% 
  mutate(Population = as.numeric(Population)) %>%
  unique() 

# Duplicates:
functional_duplicate_pop_density <- functional_kept_points_with_ID_no_geometry %>% 
  group_by(ID.1) %>% 
  summarise(count=n())

functional_kept_points_with_ID_no_geometry <- functional_kept_points_with_ID_no_geometry %>% 
  left_join(functional_duplicate_pop_density) %>%
  mutate(Population = Population/count)  

functional_kept_points_with_ID_no_geometry <- functional_kept_points_with_ID_no_geometry %>% 
  group_by(Water.point.Name, ADM2, sphere_nr_people) %>% 
  summarise(Population_within_circle = sum(Population, na.rm=TRUE)) 

# Show percentage of population served within the 500m radius 
# and show percentage of the population that lives outside of the 500m radius
functional_wp_coverage_fb_density <- FB_density_admin %>% 
  st_drop_geometry() %>%
  mutate(Population = as.numeric(Population)) %>%
  group_by(ADM2) %>% 
  summarise(Total_pop_fb = sum(Population)) %>%
  left_join(functional_kept_points_with_ID_no_geometry %>% 
              group_by(ADM2) %>%
              summarise(sphere_nr_people = sum(sphere_nr_people, na.rm=TRUE),
                        Population_within_circle = sum(Population_within_circle, na.rm=TRUE))) %>% 
  mutate(Population_outside_circle = Total_pop_fb - Population_within_circle) %>% 
  mutate(Percentage_served_within_circle = round(sphere_nr_people/Population_within_circle,2)*100) %>% 
  mutate(Percentage_pop_outside_circle = round(Population_outside_circle/Total_pop_fb,2)*100) %>% 
  mutate(actual_percentage_served = ifelse(Percentage_served_within_circle>100, 100, Percentage_served_within_circle)) %>%
  mutate(actual_pop_served = Population_within_circle*(actual_percentage_served/100)) %>% 
  mutate(percentage_population_served = round(actual_pop_served/Total_pop_fb,2)*100) #%>% View()
#write.csv2(here("data/processed", "wp_coverage_estimates_fb_pop.csv"), dec=".")

# Plot percentage of the population covered by wp within 500m radius
sl.shape.data.percentage.reach <- sl.shape.data %>% 
  left_join(functional_wp_coverage_fb_density %>% select(ADM2, percentage_population_served)) %>%
  mutate(percentage_population_served = replace_na(percentage_population_served, 0))

sl.shape.data.percentage.reach <- unique(
  sl.shape.data.percentage.reach[,c("ADM2", "geometry", 
                                    "percentage_population_served")]) %>%
  group_by(ADM2) %>%
  summarise(percentage_population_served = unique(percentage_population_served))

ggplot(sl.shape.data.percentage.reach) +
  
  #add a shapefile layer to the plot, set the line size and colour for the polygons, then fill them according to our data
  geom_sf(size = 0.3, color = "#808080", aes(fill=percentage_population_served)) +
  geom_sf_text(aes(label=paste0(ADM2, "\n", percentage_population_served, "%"))) + #, check_overlap = TRUE) +
  scale_fill_gradient(low="red",high="green", na.value="#F2F2F2", limits = c(0,100))
