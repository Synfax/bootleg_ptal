connect_stops_with_nodes <- function(vertices, stops) {

 vertex_lookup <- setNames( vertices$UFI, 1:nrow(vertices))

 stops_with_ufi <- stops %>% mutate( nearest_UFI =st_nearest_feature(stops, vertices) ) %>%
   st_drop_geometry() %>%
   mutate(nearest_UFI = vertex_lookup[nearest_UFI]) %>%
   left_join(vertices, by = c(nearest_UFI = 'UFI'))

  return(stops_with_ufi %>% select(stop_id, stop_name, nearest_UFI) %>% as.data.table())
}
