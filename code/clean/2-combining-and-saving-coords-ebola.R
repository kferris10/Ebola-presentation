
library(dplyr)
library(lubridate)
library(ggplot2)

# loading `lib_coords` of coordinates for Liberia counties
load("data/clean/Lib-county-coords.RData")
# loading `lib_ebola` of ebola data over time
load("data/clean/lib_ebola.RData")

# merging county coordinates ------------------------------

county_dat <- lib_ebola %>% 
  select(-Country, -lon, -lat) %>% 
  filter(Location != "National") %>% 
  inner_join(lib_coords) %>% 
  select(Location, lon:order, Date:Total_suspected_cases)

# national data -------------------------------------------

national_dat <- lib_ebola %>% 
  filter(Location == "National") %>% 
  select(-Country, -Location)

# coordinates of each county ------------------------------

county_centers <- lib_ebola %>% 
  filter(Location != "National") %>% 
  select(Location, lon, lat) %>% 
  distinct()

# saving --------------------------------------------------

# county centers
save(county_centers, file = "data/clean/county_center.RData")
# national over time
save(national_dat, file = "data/clean/lib_national_data.RData")
# county with coordinates
save(county_dat, file = "data/clean/lib_county_data.RData")



