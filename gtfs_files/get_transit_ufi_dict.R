get_transit_ufi_dict <- function(vertices = NULL, stops = NULL) {

  if(file.exists('rdata_output/transit_ufi_dict.Rdata')) {
    return(readRDS('rdata_output/transit_ufi_dict.Rdata'))
  } else {
    vertex_lookup <- setNames( vertices$UFI, 1:nrow(vertices))

    stops_with_ufi <- stops %>% mutate( nearest_UFI =st_nearest_feature(stops, vertices) ) %>%
      st_drop_geometry() %>%
      mutate(nearest_UFI = vertex_lookup[nearest_UFI]) %>%
      left_join(vertices, by = c(nearest_UFI = 'UFI'))

    transit_ufi_dict <- stops_with_ufi %>% select(stop_id, stop_name, nearest_UFI) %>% as.data.table()

    saveRDS(transit_ufi_dict, 'rdata_output/transit_ufi_dict.Rdata')

    return(transit_ufi_dict)
  }

}

