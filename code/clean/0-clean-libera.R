
library(dplyr)
library(lubridate)
library(stringr)
library(tidyr)
library(ggmap)
library(rvest)

# county populations --------------------------------------

pops_scrape <- html("http://en.wikipedia.org/wiki/Counties_of_Liberia") %>% 
  html_nodes("#mw-content-text .multicol .wikitable") %>% 
  html_table()
pops_clean <- pops_scrape[[1]] %>% 
  setNames(c("id", "Location", "Capital", "Established", "Area", "Population")) %>% 
  tbl_df() %>% 
  mutate_each(funs(x = str_replace_all(., ",", "")), Area, Population) %>% 
  mutate_each(funs(as.numeric), Area, Population) %>% 
  select(Location, Population) %>% 
  bind_rows(data.frame(Location = "National", Population = 3476608))

# clean-liberia -------------------------------------------

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

setwd("data/raw/ebola/liberia_data")
dat <- list.files(pattern = "[0-9]{4}") %>% 
  lapply(read.csv) %>% 
  bind_rows()
setwd("~/Projects/ebola")

# cleaning
dat_clean <- dat %>% 
  gather(Location, Count, -Date, -Variable)  %>% 
  # there's a random extra column in 12/1 data
  filter(Location != "X") %>% 
  # something's really function with the 2014-10-04 data
  filter(Date != "10/4/14") %>% 
  # some dates are recorded as m/d/14 vs m/d/2014
  mutate(
    Date = Date %>% 
      str_replace_all("/([^/]*)$", "") %>% 
      paste0("/2014") %>% 
      mdy(), 
    Variable = Variable %>% 
      str_replace_all("\\\n", "") %>% 
      str_replace_all("suspects", "suspected") %>% 
      str_replace_all("  ", " ") %>% 
      str_replace_all(" ", "_") %>% 
      str_replace_all("\\&", "_") %>% 
      str_replace_all("\\/", "_") %>% 
      str_replace_all(",", "_") %>% 
      str_replace_all("\\.", "_") %>% 
      str_replace_all("\\+", "_") %>% 
      str_replace_all("\\-", "_") %>% 
      str_replace_all("\\(", "_") %>% 
      str_replace_all("\\)", "_") %>% 
      str_replace_all("____", "_") %>% 
      str_replace_all("___", "_") %>% 
      str_replace_all("__", "_"), 
    Location = Location %>% 
      as.character() %>% 
      str_replace_all("\\.County", "") %>% 
      str_replace_all("\\.", " ")
  ) %>% 
  droplevels() %>% 
  spread(Variable, Count)

# example plot for testing
dat_clean %>% 
  filter(Location == "National") %>% 
  qplot(Date, Total_death_s_in_confirmed_cases, data = ., geom = "line") + 
  labs(y = "Deaths", title = "Number of Deaths from Ebola in Liberia") + 
  theme_bw()
# a second one
dat_clean %>% 
  qplot(Date, Total_suspected_cases, data = ., 
        geom = "line", group = Location)

# get latitudes and longitudes --------------------------

x <- dat_clean %>% 
  select(Location) %>% 
  distinct() %>% 
  mutate(Location = Location %>% 
           as.character() %>% 
           paste (., "County, Liberia") %>% 
           str_replace_all("National County, ", "")) %>% 
  bind_cols(geocode(.$Location)) %>% 
  mutate(Location = Location %>% 
           str_replace_all(" County, Liberia", "") %>% 
           str_replace_all("Liberia", "National"))

# merge data with coordinates, population --------------------------

lib_ebola <- dat_clean %>% 
  inner_join(x) %>% 
  inner_join(pops_clean) %>% 
  arrange(Location, Date) %>% 
  mutate(Country = "Liberia") %>% 
  select(Country, Location, Population, lon, lat, Date, 
         Case_Fatality_Rate_CFR_Confirmed_Probable_Cases:Total_suspected_cases)

# example plot for testing
qplot(Date, Total_death_s_in_confirmed_probable_suspected_cases, 
      data = lib_ebola, geom = "line", group = Location)

# saving -----------------------------------------------
save(lib_ebola, file = "data/clean/lib_ebola.RData")
