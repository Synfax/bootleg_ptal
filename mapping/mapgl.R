library(mapgl)
library(sf)
library(stars)
library(dplyr)
library(viridis)
library(tidyverse)
library(terra)




melbourne_sf <- read_sf('sf_output/mb_test.gpkg') %>%
  mutate(value_column = max_employment) %>%
  select(MB_CODE21, value_column) %>%
  filter(!is.na(value_column), !is.infinite(value_column))

# raster_low <- melbourne_sf %>%
#   select(value_column) %>%
#   st_transform('wgs84') %>%
#   st_rasterize(dx = 0.001, dy = 0.001)  # 500m cells for performance
#
# r <- rast(raster_low)
# writeRaster(r, 'tif_output/tif_test.tif', overwrite = T, datatype = "FLT4S")
# # Layer 1: Raster for zoomed out view (zoom 8-11)
#
# servr::httw(
#   dir = ".",
#   port = 8080
# )

mb_sf <- read_sf('~/Documents/r_projects/shapefiles/MB_2021_AUST_SHP_GDA2020/MB_2021_AUST_GDA2020.shp') %>%
  filter(GCC_NAME21 == 'Greater Melbourne') %>%
  st_transform(7855)

map_mesh_block_codes <- function(mesh_block_codes) {
  mb_sf %>%
    filter(MB_CODE21 %in% mesh_block_codes) %>%
    st_transform('wgs84') %>%
    leaflet() %>%
    addProviderTiles('CartoDB.Positron') %>%
    addPolygons()
}

summary(raster_low)
