library(sf)
library(tidyverse)

#wait if groups of mb's have the same score (investigate further)
#then i should group and combine them by total score

trimmed_results <- qs::qread('qs/trimmed_results.qs')

trimmed_scores <- qs::qread('qs/trimmed_scores.qs')

mb_sf <- read_sf('~/Documents/r_projects/shapefiles/MB_2021_AUST_SHP_GDA2020/MB_2021_AUST_GDA2020.shp') %>%
  filter(GCC_NAME21 == 'Greater Melbourne') %>%
  st_transform('wgs84')

mb_dt <- mb_sf %>%
  as.data.table() %>%
  setkey(MB_CODE21)




#mb_list_test <- trimmed_results[2:5,]$mb_code21 %>%
  #as.character()

#mb_sf %>% filter(MB_CODE21 %chin% mb_list_test) %>% mapgl::maplibre_view()

