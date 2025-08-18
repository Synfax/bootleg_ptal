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
  group_by(FROM_UFI, TO_UFI) %>%
  summarise(distance = min(distance), .groups = 'drop') %>%
  as.data.table()

setkey(edges_dt, FROM_UFI)

#map network to triple check structure
plot(st_geometry(edges), col = 'blue', lwd = 2)
plot(st_geometry(vertices), col = 'red', add = TRUE, pch = 16, cex = 1.5)



djikstra <- function(starting_node, max_distance = 46 * 84) {

  profvis({

    # Set up tracking df
    djikstra_tracking <- vertices %>%
      st_drop_geometry() %>%
      mutate(distance = Inf, visited = F) %>%
      as.data.table()

    setkey(djikstra_tracking, UFI)
    djikstra_tracking[starting_node]$distance <- 0

    # Use a simple vector to track unvisited nodes
    unvisited_nodes <- djikstra_tracking$UFI

    while (length(unvisited_nodes) > 0) {
      # Find closest unvisited node
      distances <- djikstra_tracking[unvisited_nodes]$distance
      min_idx <- which.min(distances)
      current_node <- unvisited_nodes[min_idx]
      current_tracked_distance <- distances[min_idx]

      # Early termination if we reach max distance
      if(current_tracked_distance > max_distance) {
        break
      }

      # Remove from unvisited
      unvisited_nodes <- unvisited_nodes[-min_idx]

      #update master tracking branch
      djikstra_tracking[current_node, visited := TRUE]

      #find what we can get to
      reachable_nodes <- edges_dt[current_node]

      #vectorised for speed
      if(nrow(reachable_nodes) > 0) {
        neighbor_distances <- djikstra_tracking[reachable_nodes$TO_UFI]$distance
        new_distances <- reachable_nodes$distance + current_tracked_distance
        update_mask <- new_distances < neighbor_distances & !is.na(neighbor_distances)

        if(any(update_mask)) {
          djikstra_tracking[reachable_nodes$TO_UFI[update_mask], distance := new_distances[update_mask]]
        }
      }

    }

    djikstra_tracking[, walking_time := distance %/% 84]
    djikstra_tracking = djikstra_tracking[visited == T]


  })


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
