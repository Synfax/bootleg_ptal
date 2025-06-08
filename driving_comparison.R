library(synfaxgtfs)
library(tidyverse)
library(data.table)
library(sf)
library(lubridate)
library(tictoc)
library(s2)
library(leaflet)
library(future)
library(benchmarkme)
library(furrr)
library(janitor)

#this tells me how people in each SA1 get to work.
sa1_mtwp <- read.csv('ABS/sa1_MTWP.csv', skip = 9)[-1,] %>%
  janitor::clean_names() %>%
  rename(SA1_CODE21 = 'sa1_ur') %>%
  filter(str_detect(SA1_CODE21, '2')) %>%
  mutate(public_transport = as.numeric(public_transport)) %>%
  mutate(adjusted_total = vehicle / (vehicle + public_transport + active_transport + other_mode))

#this file tells me: where do people in each sa1 actually work?


sa1_sf <- read_sf('~/Documents/r_projects/shapefiles/SA1_2021_AUST_SHP_GDA2020/SA1_2021_AUST_GDA2020.shp')

dzn_sf <- read_sf('~/Documents/r_projects/shapefiles/DZN_2021_AUST_GDA2020_SHP/DZN_2021_AUST_GDA2020.shp')

sa1_dzn_pow <- read.csv('ABS/sa1_szn_pow.csv', skip = 9 ) [-1,] %>%
  rename(sa1 = 'SA1..UR.') %>%
  janitor::clean_names() %>%
  mutate(across(starts_with('x'), as.integer))

total_pow <- data.frame(
  SA1_CODE21 = sa1_dzn_pow$sa1,
  totals = sa1_dzn_pow %>%
    select(starts_with('x')) %>%
    rowSums()
)

sa1_dzn_pow_long = sa1_dzn_pow %>%
  pivot_longer(cols = starts_with('x'), values_to = 'value', names_to = 'dzn') %>%
  select(sa1, dzn, value) %>%
  mutate(dzn = str_remove(dzn, 'x'))

sa1_dzn_grouped <- sa1_dzn_pow_long %>%
  filter(dzn %in% city_dzns ) %>%
  group_by(sa1) %>%
  summarise(total_city = sum(value)) %>%
  rename(SA1_CODE21 = 'sa1')


sa1_dzn_grouped_sf = sa1_sf %>%
  left_join(sa1_dzn_grouped, by = 'SA1_CODE21') %>%
  filter(!is.na(total_city)) %>%
  left_join(total_pow, by = 'SA1_CODE21') %>%
  mutate(prop_city = total_city / totals)


write_sf(sa1_dzn_grouped_sf, 'sf_output/sa1_pow_grouped.shp')

mtwp_sf <- sa1_sf %>%
  left_join(sa1_mtwp, by = 'SA1_CODE21') %>%
  select(SA1_CODE21, adjusted_total)


write_sf(mtwp_sf, 'sf_output/sa1_mtwp.shp', append = F)


combined_sf <- sa1_dzn_grouped_sf %>%
  left_join(sa1_mtwp, by = 'SA1_CODE21') %>%
  select(SA1_CODE21, prop_city, adjusted_total) %>%
  mutate(new_ranking = prop_city * adjusted_total)


write_sf(combined_sf, 'sf_output/combined_sf.shp')




city_dzns <- c(211180004, 211180010, 211180011,   211181080,
               215030001,
               215030002,
               215030003,
               215030004,
               215030005,
               215030006,
               215030007,
               215030008,
               215030009,
               215030010,
               215030011,
               215030012,
               215030013,
               215030014,
               215030015,
               215030016,
               215030017,
               215030018,
               215030019,
               215030020,
               215030021,
               215030022,
               215030023,
               215030024,
               215030025,
               215030026,
               215030027,
               215030028,
               215030029,
               215040001,
               215040002,
               215040003,
               215040004,
               215040005,
               215040006,
               215040007,
               215040008,
               215040009,
               215050001,
               215050002,
               215050003,
               215050004,
               215050005,
               215050006,
               215050007,
               215050008,
               215050009,
               215050010,
               215050011,
               215050012,
               215050013,
               215050014,
               215050015,
               215050016,
               215050017,
               215050018,
               215050019,
               215050020,
               215050021,
               215050022,
               215050023,
               215050024,
               215050025,
               215050026,
               215050027,
               215050028,
               215050029,
               215050030,
               215100002,
               215100004,
               215100005
)
