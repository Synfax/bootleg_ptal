link_open_space <- function() {

  #load mb sf
  mb_sf <- read_sf('~/Documents/r_projects/shapefiles/MB_2021_AUST_SHP_GDA2020/MB_2021_AUST_GDA2020.shp') %>%
    filter(GCC_NAME21 == 'Greater Melbourne') %>%
    st_transform(7855) %>%
    select(MB_CODE21, MB_CAT21) %>%
    mutate(area = st_area(geometry))

  #use osmdata to pull open space
  open_space <- opq(bbox = getbb("Melbourne, Victoria, Australia")) %>%
    add_osm_feature(key = "leisure",
                    value = c("park", "nature_reserve", "recreation_ground", "garden")) %>%
    osmdata_sf()

  #we only want the polygons - we can ignore the large multipolygons, points and lines
  open_space_polygons <- open_space$osm_polygons %>%
    st_transform(7855) %>%
    select(osm_id, name)

  #calculate which mesh blocks intersect with open space
  open_space_intersection <- st_intersection(mb_sf, open_space_polygons) %>%
    mutate(overlap_area = st_area(geometry),
           overlap_fraction = round(units::drop_units(overlap_area / area), 2),
           overlap_area_clean = round(units::drop_units(overlap_area), 2))


  open_space_mb_link <- open_space_intersection %>%
    st_drop_geometry() %>%
    group_by(MB_CODE21) %>%
    summarise(n_open_space = n(), total_open_space_area = sum(overlap_area_clean))

  #only care about mesh blocks above 10% intersect area (arbitrary)
  # mb_list <- open_space_intersection %>%
  #   filter(overlap_fraction > 0.1) %>%
  #   pull(MB_CODE21)
  #
  # #test code to viz mesh blocks
  # mb_sf %>%
  #   filter(MB_CODE21 %in% mb_list) %>%
  #   mapgl::maplibre_view()
}
