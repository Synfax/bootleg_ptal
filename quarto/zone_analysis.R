library(sf)
library(tidyverse)

vicmap_planning <- st_read('sf_input/vicmap_planning/mga2020_55/esrishape/customised_delivery/MELBOURNE_WATER-0/VMPLAN/PLAN_ZONE.shp')

vicmap_overlay <- st_read('sf_input/vicmap_planning/mga2020_55/esrishape/customised_delivery/MELBOURNE_WATER-0/VMPLAN/PLAN_OVERLAY.shp')

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

#HO analysis

# overlay_mb_intersect %>%
#   filter(str_detect(ZONE_DESC, 'HERITAGE')) %>%
#   select(ZONE_DESC, MB_CODE21) %>%
#   mutate(HO_area = units::drop_units(st_area(geometry))) %>%
#   filter(MB_CODE21 == '20070090000') %>%
#   st_transform('wgs84') %>%
#   leaflet() %>%
#   addProviderTiles('CartoDB.Positron') %>%
#   addPolygons()

#20070090000

overlay_mb_intersect <- st_intersection(mb_clean, vicmap_overlay)

heritage_mb <- overlay_mb_intersect %>%
  filter(str_detect(ZONE_DESC, 'HERITAGE')) %>%
  select(ZONE_DESC, MB_CODE21) %>%
  mutate(HO_area = units::drop_units(st_area(geometry))) %>%
  st_drop_geometry() %>%
  group_by(MB_CODE21) %>%
  summarise(total_ho_area = sum(HO_area))

mb_sf %>%
  select(MB_CODE21) %>%
  mutate(total_area = units::drop_units(st_area(geometry))) %>%
  st_drop_geometry() %>%
  left_join(heritage_mb, by = 'MB_CODE21') %>%
  mutate(total_ho_area = replace_na(total_ho_area, 0)) %>%
  mutate(total_ho_area = ifelse(total_ho_area > total_area, total_area, total_ho_area)) %>%
  mutate(not_covered_by_overlay = total_area - total_ho_area) %>%
  mutate(total_ho_area = round(total_ho_area, 1),
         not_covered_by_overlay = round(not_covered_by_overlay, 1)) -> heritage_mb_overlay

qs::qsave(heritage_mb_overlay, 'qs/heritage_mb_overlay.qs')

#LGA analysis
lga_sf <- st_read('../shapefiles/LGA_2024_AUST_GDA2020/LGA_2024_AUST_GDA2020.shp') %>%
  st_transform(7855)

joined_mb_lga <- st_join(mb_sf, lga_sf, largest = T)
joined_mb_lga_clean <- joined_mb_lga %>%
  select(MB_CODE21, LGA_NAME24) %>%
  st_drop_geometry()

qs::qsave(joined_mb_lga_clean, 'qs/mb_lga_info.qs')

qs::qsave(mb_intersects_clean,'qs/mb_zone_info.qs')
