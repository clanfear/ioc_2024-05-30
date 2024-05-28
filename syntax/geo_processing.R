library(tidyverse)
library(sf)

`%!in%` <- Negate(`%in%`)
standardize <- function(x){
  (x - mean(x, na.rm=TRUE)) / sd(x, na.rm = TRUE)
}

load("./slides/data/letter_data_valid.RData")
load("./slides/data/lost_letter_tract_122117.RData")

st_erase <- function(x, y) {
  st_difference(x, st_make_valid(st_union(st_combine(y))))
}

old_tracts <- c(17.00,43.00,74.00,100.00,104.00,107.00,110.00,114.00)

seattle_tracts_raw <- tigris::tracts(state = "WA", county = "King") %>%
  janitor::clean_names() %>%
  st_transform(3689) %>%
  st_erase(tigris::area_water(state = "WA", county = "King") %>%
             st_transform(3689))

seattle_tracts <- seattle_tracts_raw %>%
  mutate(tract_3 = as.character(as.numeric(str_sub(tractce, 2, -3))),
         tract_5 = as.character(as.numeric(str_sub(tractce, 2, -1)))) %>%
  filter(tract_3 %in% 1:121)

seattle_all_data <- seattle_tracts %>%
  full_join(letter_tract_data %>% 
               janitor::clean_names() %>% 
               mutate(collective_efficacy = standardize(collective_efficacy)) %>%
               select(-geometry, -geoid, -name) %>%
               rename(tract_5 = tract_number))

seattle_union <- seattle_tracts %>%
  st_union()

letter_sf <- letter_data_valid %>%
  janitor::clean_names() %>%
  mutate(mailed = ifelse(mailed==1, "Mailed", "Unmailed")) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(3689)

save(seattle_tracts, file = "./slides/data/seattle_tracts.RData")
save(seattle_all_data, file = "./slides/data/seattle_all_data.RData")
save(seattle_union, file = "./slides/data/seattle_union.RData")
save(letter_sf, file = "./slides/data/letter_sf.RData")