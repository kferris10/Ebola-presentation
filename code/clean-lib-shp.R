
library(maptools)
library(rgdal)
library(ggplot2)
library(dplyr)

# loading data -------------------------------------------

# reading in the shape file
lib_shp <- readShapePoly("data/raw/LIB/LIB-level_1.shp")

# getting the coordinates into a data.frame
lib_coords <- fortify(lib_shp) %>% 
  tbl_df() %>% 
  group_by(group) %>% 
  arrange(order) %>% 
  ungroup() %>% 
  select(long, lat, id, order) %>% 
  rename(lon = long)

# identifying counties -----------------------------------

counties <- data_frame(
  id = 0:14 %>% as.character(), 
  Location = c("Bomi", 
               "Bong", 
               "Grand Bassa", 
               "Grand Cape Mount", 
               "Grand Gedeh", 
               "Grand Kru", 
               "Lofa", 
               "Margibi", 
               "Maryland", 
               "Montserrado", 
               "Nimba", 
               "RiverCess", 
               "Sinoe", 
               "River Gee", 
               "Gbarpolu")
)

lib_coords <- lib_coords %>% 
  left_join(counties)

# map to check -----------------------------------------

library(ggmap)
m <- get_map("liberia", zoom = 7)

ggmap(m) + 
  geom_polygon(aes(x = lon, y = lat, fill = Location), alpha = I(.3), 
               data = lib_coords)

# saving -----------------------------------------------

save(lib_coords, file = "data/clean/Lib-county-coords.RData")




