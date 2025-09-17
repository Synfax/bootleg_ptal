library(mapgl)
library(sf)
library(stars)
library(dplyr)
library(viridis)
library(tidyverse)
library(terra)
library(remotes)
remotes::install_github('qfes/rdeck')

library(mapgl)

melbourne_sf <- read_sf('sf_output/final_result.gpkg') %>%
  mutate(value_column = empl) %>%
  select(MB_CODE21, value_column) %>%
  filter(!is.na(value_column), !is.infinite(value_column)) %>%
  mutate(area = st_area(geom))

raster_low <- melbourne_sf %>%
  select(value_column) %>%
  st_transform('wgs84') %>%
  st_rasterize(dx = 0.005, dy = 0.005)  # 500m cells for performance

# r <- rast(raster_low)
# writeRaster(r, 'tif_output/tif_test.tif', overwrite = T, datatype = "FLT4S")
# # Layer 1: Raster for zoomed out view (zoom 8-11)
#
# servr::httw(
#   dir = ".",
#   port = 8080
# )

# mb_sf <- read_sf('~/Documents/r_projects/shapefiles/MB_2021_AUST_SHP_GDA2020/MB_2021_AUST_GDA2020.shp') %>%
#   filter(GCC_NAME21 == 'Greater Melbourne') %>%
#   st_transform(7855)
#
# map_mesh_block_codes <- function(mesh_block_codes) {
#   mb_sf %>%
#     filter(MB_CODE21 %in% mesh_block_codes) %>%
#     st_transform('wgs84') %>%
#     leaflet() %>%
#     addProviderTiles('CartoDB.Positron') %>%
#     addPolygons()
# }

raster_terra <- rast(raster_low)


maplibre_view(raster_terra)



melbourne_sf %>%
  st_make_grid(cellsize = 200, square = F) %>%
  st_sf() %>%
  mutate(row_id = row_number()) -> hex_grid


hex_grid <- hex_grid[melbourne_sf,]


st_intersection(hex_grid, melbourne_sf) -> ints

ints = ints %>%
  mutate(intersect_area = st_area(geometry),
         intersect_fraction = round(units::drop_units(intersect_area/area), 2))

ints %>%
  st_drop_geometry() %>%
  group_by(row_id) %>%
  summarise(mean_employment = sum(intersect_fraction * value_column, na.rm = T)) -> ints

hex_grid = hex_grid %>%
  left_join(ints, by = 'row_id')

hex_grid = hex_grid %>% st_transform('wgs84')


melb_sf_wgs84 = st_transform(melbourne_sf, 'wgs84')
rdeck(initial_bounds = st_bbox(melb_sf_wgs84)) %>%
  add_polygon_layer(
    data = melb_sf_wgs84,
    get_polygon = geom,
    get_fill_color = scale_color_quantile(value_column, palette = scales::viridis_pal(), probs = seq.int(0,1,0.1)),
    pickable = TRUE
  ) -> deck

htmlwidgets::saveWidget(deck, 'tif_output/rdeck_test.html', selfcontained = T)


melb_hex = st_transform(hex_grid, 'wgs84')
rdeck(initial_bounds = st_bbox(hex_grid)) %>%
  add_polygon_layer(opacity = 0.5,
    data = hex_grid,
    get_polygon = geometry,

    get_fill_color = scale_color_quantile(mean_employment, palette = scales::viridis_pal(), probs = seq.int(0,1,0.1)),
    pickable = TRUE
  ) -> deck

htmlwidgets::saveWidget(deck, 'tif_output/rdeck_test.html', selfcontained = T)

# maplibre_view(
#   hex_grid %>% st_simplify(dTolerance = 200),
#   column = "mean_employment",
#   palette = viridis::viridis     # or plasma, inferno, cividis, mako, rocket, turbo
# )
#
#
# leaflet() %>%
#   addProviderTiles('CartoDB.Positron') %>%
#   addPolygons(data = hex_grid %>% st_simplify(dTolerance = 200) %>% st_transform('wgs84'))
#
#
#


hex_data <- hex_grid |>
  mutate(
    value_class = step_quantile("mean_employment", n = 7)  # 7 quantile breaks
  )

maplibre(bounds = hex_data) |>
  add_fill_layer(
    id = "hex_layer",
    source = hex_data,
    fill_color = step_quantile(
      column = "value_column",
      n = 7,
      palette = viridis::viridis(7)  # 7 colors to match breaks
    ),
    fill_opacity = 0.8
  )
