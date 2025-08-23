final_destinations %>%
  select(UFI) %>%
  distinct() %>%
  as.data.frame() %>%
  left_join(tr_road_infra, by = 'UFI') %>%
  st_set_geometry('geometry') -> sf

convex <- concaveman(sf, concavity = 2)

buffered_ufis <- road_infra_joined %>%
  mutate(buffer = st_buffer(geometry, dist = 150),
         UFI = as.character(UFI)) %>%
  st_drop_geometry() %>%
  select(UFI, buffer) %>%
  as.data.table() %>%
  setkey(UFI)

#800ms
profvis({

  isochrone_fragments <- sf %>%
    st_buffer(dist = 150) %>%
    st_union() %>%
    st_cast("POLYGON")

})

profvis({
  isochrone_fragments <- buffered_ufis[as.character(sf$UFI)]$buffer %>%
    st_union()
})



leaflet(isochrone_fragments %>% st_transform('wgs84')) %>%
  addProviderTiles('CartoDB.Positron') %>%
  addPolygons()
