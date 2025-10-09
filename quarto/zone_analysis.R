library(sf)
library(tidyverse)

vicmap_planning <- st_read('sf_input/vicmap_planning/mga2020_55/esrishape/customised_delivery/MELBOURNE_WATER-0/VMPLAN/PLAN_ZONE.shp')

mb_sf <- st_read('../shapefiles/MB_2021_AUST_SHP_GDA2020/MB_2021_AUST_GDA2020.shp') %>%
  filter(GCC_NAME21 == 'Greater Melbourne') %>%
  st_transform(7855)

mb_clean <- mb_sf %>%
  select(MB_CODE21)

mb_intersects <- st_intersection(mb_clean, vicmap_planning) %>%
  mutate(intersect_area = st_area(geometry))

mb_intersects_clean = mb_intersects %>%
  select(MB_CODE21, ZONE_CODE, intersect_area) %>%
  st_drop_geometry()

qs::qsave(mb_intersects_clean,'qs/mb_zone_info.qs')
