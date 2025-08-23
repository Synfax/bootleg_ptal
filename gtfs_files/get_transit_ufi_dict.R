get_transit_ufi_dict <- function(vertices = NULL, stops = NULL) {

  if(file.exists('rdata_output/transit_ufi_dict.Rdata')) {
    return(readRDS('rdata_output/transit_ufi_dict.Rdata'))
  } else {
    vertex_lookup <- setNames( vertices$UFI, 1:nrow(vertices))

    stops_with_ufi <- stops %>%
      mutate( nearest_UFI = st_nearest_feature(stops, vertices) ) %>%
      st_drop_geometry () %>%
      mutate(nearest_UFI = vertex_lookup[nearest_UFI]) %>%
      left_join(vertices, by = c(nearest_UFI = 'UFI'))

    transit_ufi_dict <- stops_with_ufi %>%
      select(stop_id, stop_name, nearest_UFI) %>%
      as.data.table()

    saveRDS(transit_ufi_dict, 'rdata_output/transit_ufi_dict.Rdata')

    return(transit_ufi_dict)
  }

}

link_walk_stops <- function(){

  walking_files <- list.files('walking_isochrones_sa2/')

  walking_files %>% map_dfr(.f = function(file){
    fread(paste0('walking_isochrones_sa2/',file))
  }) -> all_walk

  all_walk[, start_UFI := as.numeric(start_UFI)]

  transit_copy <- copy(transit_ufi_dict)
  transit_copy <- transit_copy[!str_detect(stop_id, 'vic')]

  transit_copy[, nearest_UFI := as.numeric(nearest_UFI)]
  setkey(transit_copy, nearest_UFI)
  setkey(all_walk, start_UFI)

  walking_access_dict <- transit_copy[all_walk, on = c('nearest_UFI' = 'start_UFI'), allow.cartesian = T]

  walking_access_dict <- walking_access_dict[, .SD[sample(.N, max(1, .N * 0.1))], by = stop_id]

  setkey(walking_access_dict, stop_id)

  return(walking_access_dict)
}

map_ufi <- function(final_dest) {
  final_dest %>%
    select(UFI) %>%
    distinct() %>%
    as.data.frame() %>%
    left_join(tr_road_infra, by = 'UFI') %>%
    st_set_geometry('geometry') %>%
    st_transform('wgs84')-> sf


  leaflet(sf) %>%
    addProviderTiles('CartoDB.Positron') %>%
    addCircleMarkers()
}
