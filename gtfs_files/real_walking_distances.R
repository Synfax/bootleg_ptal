tr_road <- read_sf('sf_input/tr_road/Order_I2F4YY/mga2020_55/esrishape/customised_delivery/MELBOURNE_WATER-0/VMTRANS/TR_ROAD_ALL.shp')
tr_road = tr_road %>% st_transform('wgs84')

tr_road_infra <- read_sf('sf_input/tr_road_infrastructure/Order_08APF2/mga2020_55/esrishape/customised_delivery/MELBOURNE_WATER-0/VMTRANS/TR_ROAD_INFRASTRUCTURE.shp')
tr_road_infra <- tr_road_infra %>% st_transform('wgs84')

brunswick <- getbb("brunswick, victoria, australia", format_out = 'sf_polygon') %>%
  st_transform('wgs84')

tr_road_brunswick <- st_intersection(tr_road, brunswick)
tr_infra_brunswick <- st_intersection(tr_road_infra, brunswick)

tr_road_brunswick = tr_road_brunswick %>%
  select(FROM_UFI, TO_UFI)

tr_infra_brunswick = tr_infra_brunswick %>%
  select(UFI)

## create vertices

vertices <- tr_infra_brunswick %>%
  mutate(UFI = as.character(UFI))

## create edges

edges <- tr_road_brunswick %>%
  mutate(distance = units::drop_units(st_length(geometry)) )

edges_reversed <- edges %>%
  rename(FROM_UFI = TO_UFI, TO_UFI = FROM_UFI)

edges_dt <- bind_rows(edges, edges_reversed) %>%
  mutate(FROM_UFI = as.character(FROM_UFI),
         TO_UFI = as.character(TO_UFI)) %>%
  as.data.table()

setkey(edges_dt, FROM_UFI)

plot(st_geometry(edges), col = 'blue', lwd = 2)
plot(st_geometry(vertices), col = 'red', add = TRUE, pch = 16, cex = 1.5)

#


djikstra <- function(starting_node, max_distance = 46 * 84) {
  #set up tracking df

  djikstra_tracking <- vertices %>%
    st_drop_geometry() %>%
    mutate(distance = Inf, visited = F) %>%
    as.data.table()

  setkey(djikstra_tracking, UFI)

  djikstra_tracking[starting_node]$distance <- 0
  unreached_nodes_df <- djikstra_tracking[visited == FALSE][order(distance)]

  while (nrow(unreached_nodes_df) > 0) {
    current_node = unreached_nodes_df[1, ]$UFI

    current_tracked_distance <- unreached_nodes_df[1, ]$distance

    #EARLY TERMINATION: If closest unvisited node is beyond threshold, stop!
    if(current_tracked_distance > max_distance) {
      break
    }

    djikstra_tracking[current_node, visited := TRUE]

    reachable_nodes <- edges_dt[current_node]

    setkey(reachable_nodes, TO_UFI)

    for (node in unique(reachable_nodes$TO_UFI)) {

      shortest_distance = djikstra_tracking[node]$distance

      if(is.na(shortest_distance)) {
        next()
      }

      current_distance = min(reachable_nodes[node]$distance) + current_tracked_distance

      if (current_distance < shortest_distance) {
        djikstra_tracking[node, distance := current_distance]
      }

    }

    unreached_nodes_df <- djikstra_tracking[visited == FALSE][order(distance)]

    #remove node from unreached nodes
    # unreached_nodes_df = setorder(copy(djikstra_tracking), distance)
    # unreached_nodes_list = c(unreached_nodes_list, current_node)
    # unreached_nodes_df <- unreached_nodes_df[!c(unreached_nodes_list, current_node), on = "osm_id"]


  }

  return(djikstra_tracking)

}


djikstra("2293277") -> d

d %>% filter(visited) %>% left_join(tr_infra_brunswick %>% mutate(UFI = as.character(UFI)), by = "UFI") -> djik_test

write_sf(djik_test, 'sf_output/djikstra_test.gpkg')

##
#
# leaflet() %>%
#   addProviderTiles('CartoDB.Positron') %>%
#   addPolylines(data = tr_road_brunswick[1,]) %>%
#   addCircleMarkers(data = tr_infra_brunswick %>%
#                      filter(`UFI` %in% c(2293422, 2293205)) )
