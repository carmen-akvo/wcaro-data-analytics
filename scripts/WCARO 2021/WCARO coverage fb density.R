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

# Selection of wp 2016 data
wp_2016_sub <- wp_2016 %>% 
  filter(ADM2 == "Bombali") %>%
  dplyr::select(#"ADM2", "ADM3", "ADM4", "ADM5",
    "Water point Name", 
    "Latitude", "Longitude",
    "Functionality",
    "sdg_improved_source",
    "sphere_nr_people") %>% 
  unique() 

# Convert to sf, set the crs to EPSG:4326 (lat/long), 
# and transform to EPSG:3035
wp_2016_sf <- st_as_sf(wp_2016_sub, 
                       coords = c( "Longitude", "Latitude"), 
                       crs = 4326) %>% 
  st_transform(3035)

# Buffer circles by 100m
wp_2016_circles <- st_buffer(wp_2016_sf, dist = 500)

# Polygon
sl.shape <- read_sf(dsn = here::here("data/raw/SIL_admin_SHP/", "SIL.shp")) %>%
  filter(ADM2 == "Bombali") %>%
  st_set_crs(4326) %>% 
  st_transform(3035)

# Intersect the circles with the polygons
wp_2016_int_circles <- st_intersection(sl.shape, wp_2016_circles)

# Find reach per district
wp_2016_coverage_district <- wp_2016_int_circles %>%
  group_by(ADM2) %>% 
  summarize(geometry = st_union(geometry),
            sphere_coverage = sum(sphere_nr_people))

# Create choropleth map with reach numbers for 2016 data
ggplot(wp_2016_coverage_district) +
  
  # add a shapefile layer to the plot, set the line size and colour for the polygons, then fill them according to our data
  geom_sf(size = 0.3, color = "#808080", aes(fill=sphere_coverage)) 

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

# Find overlap

# Per region
# kept_points_summary <- st_intersection(wp_2016_int_circles_summary, pnts_sf)

# Per waterpoint -> problem: population denstiy is counted double!
kept_points_with_ID <- st_intersection(wp_2016_int_circles, pnts_sf) 

kept_points_with_ID <- kept_points_with_ID %>% 
  select(Water.point.Name, ADM2, sdg_improved_source, sphere_nr_people, ID.1, Population, geometry) %>% 
  mutate(Population = as.numeric(Population)) %>%
  unique() 

# Duplicates:
duplicate_pop_density <- kept_points_with_ID %>% 
  st_drop_geometry() %>% 
  group_by(ID.1) %>% 
  summarise(count=n())

# Find the population density squares that fall in/out of the water point reach
pnts_sf_not_covered <- pnts_sf %>%
  mutate(covered_by_wp = ifelse(
    geometry %in% unique(kept_points_with_ID$geometry), 
    "yes", "no")) %>%
  filter(covered_by_wp == "no") %>%
  mutate(percentage_served = 0) %>%
  mutate(Population = as.numeric(Population))

# Remove Duplicated entries and sum per district or community how many people live within 500 m of a water point 
population_served_bombali <- kept_points_with_ID %>% 
  left_join(duplicate_pop_density) %>%
  mutate(Population = Population/count) %>%
  group_by(Water.point.Name, sdg_improved_source, sphere_nr_people) %>% 
  summarise( Population = sum(Population)) %>%
  mutate(percentage_served = round(sphere_nr_people/Population,2)*100)

population_served_bombali_not_served <- rbind(population_served_bombali %>% 
                                                select(Population, percentage_served, geometry),
                                              pnts_sf_not_covered %>%
                                                select(Population, percentage_served, geometry))

population_served_bombali %>% 
  mutate(percentage_served = ifelse(percentage_served > 100, 100, percentage_served)) %>%
  ggplot() + 
  geom_sf(aes(color=percentage_served)) +
  scale_color_gradient(low = "red", high = "green") +
  geom_point(data=pnts_sf, 
             aes(x=Longitude, y=Latitude, color=Partner), 
             size = 2, alpha=1, shape=20) 

population_served_bombali_not_served %>% 
  mutate(percentage_served = ifelse(percentage_served > 100, 100, percentage_served)) %>%
  ggplot() + 
  geom_sf(aes(color=percentage_served)) +
  scale_color_gradient(low = "red", high = "green") +
  geom_point(data=pnts_sf, 
             aes(x=Longitude, y=Latitude, color=Partner), 
             size = 2, alpha=1, shape=20) 

pnts_sf_not_covered %>% 
  ggplot() + 
  geom_sf(aes(color=Population)) +
  scale_color_gradient(low = "blue", high = "red")


# Find out what part of the population DOES NOT live within 500 m of a water point

# Determine area covered by water points per district 
wp_2016_int_circles_summary <-  wp_2016_int_circles %>%
  group_by( NAME_2) %>% 
  summarise() %>% 
  mutate(area = st_area(.))

