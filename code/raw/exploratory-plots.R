
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
  mutate(prop = Total_death_s_in_confirmed_probable_suspected_cases / Contacts_seen)
ggmap(ggm) + 
  geom_polygon(aes(x = lon, y = lat, group = Location, fill = prop), 
               data = county_dat2, alpha = I(.3))

m <- leaflet() %>% 
  addTiles() %>% 
  setView(-9.429499, 6.428055, zoom = 7)
m




m %>% addPolylines(lng = ~lon, lat = ~lat, layerId = ~location, 
                   data = county_dat2)
