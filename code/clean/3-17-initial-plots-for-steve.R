
library(dplyr)
library(lubridate)
library(ggplot2)
library(ggmap)
library(animation)
library(animint)
library(maptools)
library(rgdal)

load("data/clean/lib_ebola.RData")
# polygons
d <- readShapePoly("data/raw/LIB/LIB.shp")
coords <- d %>% 
  fortify() %>% 
  tbl_df() %>% 
  group_by(group) %>% 
  arrange(order) %>% 
  ungroup()

# comparing June to December ----------------------------------------

# map of liberia
m <- get_map("liberia", zoom = 7)

# June
june_dat <- lib_ebola %>% 
  filter(month(Date) == 6) %>% 
  group_by(Location) %>% 
  filter(Date == max(Date)) %>% 
  ungroup() %>% 
  select(Date:Location, Total_suspected_cases) 
ggmap(m) + 
  geom_point(aes(x = lon, y = lat, size = Total_suspected_cases), data = june_dat)

# December
dec_dat <- lib_ebola %>% 
  filter(Date == ymd("2014-12-01")) %>% 
  select(Date:Location, Total_suspected_cases) 
ggmap(m) + 
  geom_point(aes(x = lon, y = lat, size = Total_suspected_cases), data = dec_dat)

# cases over time ---------------------------------------------------

lib_ebola %>% 
  select(Date:Location, Total_suspected_cases) %>% 
  filter(!is.na(Total_suspected_cases)) %>% 
  qplot(Date, Total_suspected_cases, data = ., geom = "line", 
        colour = Location, size = I(1.1)) + 
  theme_bw()

# making an animint plot ----------------------------------

# data for animint
ani_dat <- lib_ebola %>% 
  select(Date:Location, Total_suspected_cases) %>% 
  filter(Location != "National", !is.na(Total_suspected_cases)) %>% 
  mutate(Date = as.numeric(Date)) %>% 
  data.frame()

# over time
p_time <- ggplot() + 
  make_tallrect(data = ani_dat, "Date") + 
  geom_line(aes(x = Date, y = Total_suspected_cases, group = Location, colour = Location), 
            data = ani_dat) + 
  scale_x_continuous(breaks = c(1.405e9, 1.41e9, 1.415e9), 
                     labels = c("July", "September", "November")) + 
  theme_animint(width = 350) + 
  ggtitle("Ebola Cases in Liberia in 2014")
# map
p_map <- ggplot() + 
  geom_path(aes(x = long, y = lat, group = group), 
            data = coords, alpha = I(.6), colour = "grey", size = I(.3)) + 
  geom_point(aes(x = lon, y = lat, size = Total_suspected_cases, colour = Location, 
                 showSelected = Date), 
             data = ani_dat, alpha = I(.7)) + 
  scale_size_continuous(range = c(1, 10)) + 
  ggtitle("Ebola Cases in Liberia")

# animint
ani_list <- list(timeLine = p_time, 
                 ebolaMap = p_map, 
                 time = list(variable = "Date", ms = 1000))
animint2dir(ani_list, out.dir = "linked_viz", open.browser = FALSE)
servr::httd("linked_viz")


