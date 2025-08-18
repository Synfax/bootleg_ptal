#download TR road from Data VIC.
tr_road <- read_sf('sf_input/tr_road/Order_I2F4YY/mga2020_55/esrishape/customised_delivery/MELBOURNE_WATER-0/VMTRANS/TR_ROAD_ALL.shp')
tr_road = tr_road %>% st_transform('wgs84')

#download TR road infra from Data VIC.
tr_road_infra <- read_sf('sf_input/tr_road_infrastructure/Order_08APF2/mga2020_55/esrishape/customised_delivery/MELBOURNE_WATER-0/VMTRANS/TR_ROAD_INFRASTRUCTURE.shp')
tr_road_infra <- tr_road_infra %>% st_transform('wgs84')

# i restrict my sample to Brunswick, just so that I can test it.
brunswick <- getbb("brunswick, victoria, australia", format_out = 'sf_polygon') %>%
  st_transform('wgs84')

#find intersections between sample area and larger dfs
tr_road_brunswick <- st_intersection(tr_road, brunswick)
tr_infra_brunswick <- st_intersection(tr_road_infra, brunswick)

#only select columns I want.
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
  rename(FROM_UFI = TO_UFI, TO_UFI = FROM_UFI)h

edges_dt <- bind_rows(edges, edges_reversed) %>%
  mutate(FROM_UFI = as.character(FROM_UFI),
         TO_UFI = as.character(TO_UFI)) %>%
  group_by(FROM_UFI, TO_UFI) %>%
  summarise(distance = min(distance), .groups = 'drop') %>%
  as.data.table()

edges_list = split(edges_dt, edges_dt$FROM_UFI)

#map network to triple
plot(st_geometry(edges), col = 'blue', lwd = 2)
plot(st_geometry(vertices), col = 'red', add = TRUE, pch = 16, cex = 1.5)

#


dijkstra <- function(starting_node, max_distance = 46 * 84) {

  #profvis({

    # Set up tracking with named vectors for O(1) lookups
    distances <- setNames(rep(Inf, nrow(vertices)), vertices$UFI)
    distances[starting_node] <- 0
    visited <- setNames(rep(FALSE, nrow(vertices)), vertices$UFI)

    # Use a simple vector to track unvisited nodes
    unvisited_nodes <- vertices$UFI


    while (length(unvisited_nodes) > 0) {
      # Find closest unvisited node
      unvisited_distances <- distances[unvisited_nodes]
      min_idx <- which.min(unvisited_distances)
      current_node <- unvisited_nodes[min_idx]
      current_tracked_distance <- unvisited_distances[min_idx]

      # Early termination if we reach max distance
      if(current_tracked_distance > max_distance) {
        break
      }

      # Remove from unvisited
      unvisited_nodes <- unvisited_nodes[-min_idx]

      #update master tracking branch
      visited[current_node] <- TRUE

      #find what we can get to
      reachable_nodes <- edges_list[[current_node]]
      #reachable_nodes <- edges_dt[current_node]

      #vectorised for speed
      if(nrow(reachable_nodes) > 0) {
        neighbor_distances <- distances[reachable_nodes$TO_UFI]
        new_distances <- reachable_nodes$distance + current_tracked_distance
        update_mask <- new_distances < neighbor_distances & !is.na(neighbor_distances)

        if(any(update_mask)) {
          distances[reachable_nodes$TO_UFI[update_mask]] <- new_distances[update_mask]
        }
      }

    }

    # Create result data.table
    result <- data.table(
      UFI = names(distances)[visited],
      distance = distances[visited],
      walking_time = distances[visited] %/% 84,
      visited = TRUE
    )

  #})

  return(result)

}


dijkstra("2290274") -> d

stop_ufi_dict <- connect_stops_with_nodes(vertices, stops)
setkey(stop_ufi_dict, nearest_UFI)

#join result to stops eventually.


##
#
# leaflet() %>%
#   addProviderTiles('CartoDB.Positron') %>%
#   addPolylines(data = tr_road_brunswick[1,]) %>%
#   addCircleMarkers(data = tr_infra_brunswick %>%
#                      filter(`UFI` %in% c(2293422, 2293205)) )
