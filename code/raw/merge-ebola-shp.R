
library(maptools)
library(rgdal)
library(ggplot2)
library(ggmap)
library(dplyr)

# load data -----------------------------------------

# polygons
d <- readShapePoly("data/raw/LIB/LIB.shp")
coords <- d %>% 
  fortify() %>% 
  tbl_df() %>% 
  group_by(group) %>% 
  arrange(order) %>% 
  ungroup()

# liberia data
dat <- d@data %>% 
  tbl_df() 

# ebola data - saving doesn't work yet
load("data/clean/lib_ebola.RData")

# plots for entire area -----------------------------

plot(d, axes = T)
qplot(long, lat, data = coords, group = group, geom = "path") 
qplot(long, lat, data = coords, group = group, geom = "polygon", fill = id) + 
  scale_fill_discrete(guide = F)

# map
m <- get_map("liberia", zoom = 8)
ggmap(m) + 
  geom_polygon(aes(x = long, y = lat, group = group, fill = id), 
               alpha = I(.3), data = x) + 
  scale_fill_discrete(guide = F)


