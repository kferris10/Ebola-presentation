
library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggmap)
library(leaflet)

# loading national data
load("data/clean/lib_national_data.RData")
# loading county data
load("data/clean/lib_ebola.RData")
# loading county data with coordinates
load("data/clean/lib_county_data.RData")

# plots over time --------------------------------------

# all variables
national_dat %>% 
  select(-lon, -lat) %>% 
  gather(Var, Count, -Date) %>% 
  qplot(Date, Count, data = ., geom = "line", facets = ~Var)

# smoother over time
national_dat %>% 
  qplot(Date, Total_death_s_in_confirmed_probable_suspected_cases, 
        data = ., geom = c("line", "smooth"))

# proportion of deaths over time
national_dat %>% 
  filter(Contacts_seen != 0) %>% 
  mutate(prop = Total_death_s_in_confirmed_probable_suspected_cases / Contacts_seen) %>% 
  qplot(Date, prop, data = ., geom = c("line", "smooth"))

# counties over time -------------------------------------

# contacts seen by county
qplot(Date, Contacts_seen, data = lib_ebola, geom = "line", colour = Location)

# cumulative cases
qplot(Date, Total_suspected_cases, 
      data = lib_ebola, geom = "line", colour = Location)

# maps for Jim -------------------------------------------

ggm <- get_map("liberia", zoom = 7)
county_dat2 <- county_dat %>% 
  filter(Date == ymd("2014-11-24")) %>% 
  mutate(prop = Total_death_s_in_confirmed_probable_suspected_cases / Population)
ggmap(ggm) + 
  geom_polygon(aes(x = lon, y = lat, group = Location, fill = prop), 
               data = county_dat2, alpha = I(.3))

# leaflet plots ---------------------- -------------------------

# first date
county_first <- county_dat %>% 
  filter(Date == ymd("2014-06-28"), 
         !is.na(Total_death_s_in_confirmed_probable_suspected_cases)) %>% 
  mutate(Rate = Total_death_s_in_confirmed_probable_suspected_cases / Population * 1000)  %>% 
  select(Location, lon, lat, order, Date, Rate) %>% 
  arrange(Location, order)
# unique locations
unique_locs1 <- unique(county_first$Location)
# fills
cols1 <- c("#f2f0f7", "#cbc9e2")
county_fills_first <- county_first %>% 
  select(Location, Rate) %>% 
  distinct() %>% 
  mutate(bins = cut(Rate, 2), 
         levs = as.numeric(bins), 
         fill = cols1[levs])
# leaflet plot
m_first <- leaflet() %>% 
  addTiles(
        'http://server.arcgisonline.com/ArcGIS/rest/services/World_Topo_Map/MapServer/tile/{z}/{y}/{x}'
  ) %>% 
  setView(-9.429499, 6.428055, zoom = 7)
for(i in 1:length(unique_locs1)) {
  m_first <- m_first %>% 
    addPolygons(
      county_first$lon[county_first$Location == unique_locs1[i]], 
      county_first$lat[county_first$Location == unique_locs1[i]], 
      color = "grey", 
      fillOpacity = 0.8, 
      fillColor = county_fills_first$fill[county_fills_first$Location == unique_locs1[[i]]]
    )
}
m_first


# last date
county_last <- county_dat %>% 
  filter(Date == ymd("2014-12-09")) %>% 
  mutate(Rate = Total_death_s_in_confirmed_probable_suspected_cases / Population * 1000)  %>% 
  select(Location, lon, lat, order, Date, Rate) %>% 
  arrange(Location, order)
# unique locations
unique_locs <- unique(county_last$Location)
# fills
cols <- c("#cbc9e2", "#9e9ac8", "#756bb1", "#54278f")
county_fills <- county_last %>% 
  select(Location, Rate) %>% 
  distinct() %>% 
  mutate(bins = cut(Rate, 4), 
         levs = as.numeric(bins), 
         fill = cols[levs])

m <- leaflet() %>% 
  addTiles(
    'http://server.arcgisonline.com/ArcGIS/rest/services/World_Topo_Map/MapServer/tile/{z}/{y}/{x}'
  ) %>% 
  setView(-9.429499, 6.428055, zoom = 7)
for(i in 1:length(unique_locs)) {
  m <- m %>% 
    addPolygons(
      county_last$lon[county_last$Location == unique_locs[i]], 
      county_last$lat[county_last$Location == unique_locs[i]], 
      color = "grey", 
      fillOpacity = 0.8, 
      fillColor = county_fills$fill[county_fills$Location == unique_locs[[i]]]
    )
}
m

