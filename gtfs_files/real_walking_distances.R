# i restrict my sample to Brunswick, just so that I can test it.
# brunswick <- getbb("brunswick, victoria, australia", format_out = 'sf_polygon') %>%
#   st_transform('wgs84')

#find intersections between sample area and larger dfs
# tr_road_brunswick <- st_intersection(tr_road, brunswick)
# tr_infra_brunswick <- st_intersection(tr_road_infra, brunswick)
#
# #only select columns I want.
# tr_road_brunswick = tr_road_brunswick %>%
#   select(FROM_UFI, TO_UFI)
#
# tr_infra_brunswick = tr_infra_brunswick %>%
#   select(UFI)

#download TR road from Data VIC.
tr_road <- read_sf('sf_input/tr_road/Order_I2F4YY/mga2020_55/esrishape/customised_delivery/MELBOURNE_WATER-0/VMTRANS/TR_ROAD_ALL.shp')
tr_road = tr_road %>% st_transform(7855)

#download TR road infra from Data VIC.
tr_road_infra <- read_sf('sf_input/tr_road_infrastructure/Order_08APF2/mga2020_55/esrishape/customised_delivery/MELBOURNE_WATER-0/VMTRANS/TR_ROAD_INFRASTRUCTURE.shp')
tr_road_infra <- tr_road_infra %>%
  st_transform(7855) %>%
  select(UFI)

#parameters
minutes_willing_to_walk <- 20

#load SA2 sf
sa2_sf <- read_sf('sf_input/SA2_2021_AUST_SHP_GDA2020/SA2_2021_AUST_GDA2020.shp') %>%
  filter(STE_NAME21 == 'Victoria', GCC_NAME21 == 'Greater Melbourne') %>%
  select(SA2_CODE21, SA2_NAME21) %>%
  st_transform(7855)



#assign SA2s to road infra points
sf_use_s2(F)
road_infra_joined <- st_join(tr_road_infra, sa2_sf)

#set up a dt for easy retrieval and less filtering.
road_infra_dt <- road_infra_joined %>%
  st_drop_geometry() %>%
  as.data.table() %>%
  setkey(SA2_NAME21)

#check to see it worked right
ggplot(road_infra_joined %>% filter(SA2_NAME21 == 'Brunswick West')) + geom_sf()


#edges need to be created out of the loop

## create edges
edges <- tr_road %>%
  mutate(distance = units::drop_units(st_length(geometry)) )

edges_reversed <- edges %>%
  rename(FROM_UFI = TO_UFI, TO_UFI = FROM_UFI)

edges_dt <- bind_rows(edges, edges_reversed) %>%
  mutate(FROM_UFI = as.character(FROM_UFI),
         TO_UFI = as.character(TO_UFI)) %>%
  select(FROM_UFI, TO_UFI, distance) %>%
  st_drop_geometry() %>%
  as.data.table()

edges_dt <- edges_dt[, .(distance = min(distance)), by = .(FROM_UFI, TO_UFI)]
edges_dt_keyed <- copy(edges_dt) %>% setkey(FROM_UFI)

#now for the logic.

#we are going to iterate through every SA2 in Greater Melbourne
#for each SA2 we are going to buffer by the maximum walking distance.


function(sa2) {

  current_sa2_sf = sa2_sf %>%
    filter(SA2_NAME21 == sa2)

  buffer_size = minutes_willing_to_walk * 84

  #get nodes in the current SF that will be our starting points
  #now we need to find all the nodes we could walk to so we can keep the network small.
  starting_nodes = road_infra_dt[sa2]$UFI %>% as.character()

  #buffer the sa2
  buffered_sa2 <- st_buffer(current_sa2_sf, dist = buffer_size)

  #find which other SA2s overlap with this buffer

  overlap_indexes <- st_overlaps(buffered_sa2, sa2_sf) %>% unlist()
  overlaps <- sa2_sf[overlap_indexes,]
  overlapping_sa2_names <- overlaps$SA2_NAME21

  #now we have all sa2s that could be reached within the absolute maximum walking time
  #this will form the network we use.
  #but we are only considering starting nodes in the sa2 in focus
  all_tr_points_in_overlap <- road_infra_dt[c(overlapping_sa2_names, sa2)]

  ## create vertices
  vertices <- all_tr_points_in_overlap %>%
    mutate(UFI = as.character(UFI))

  #create a smaller edges_list for each SA2 to reduce key retrieval times
  edges_list = edges_dt_keyed[vertices$UFI]
  edges_list = split(edges_list, edges_list$FROM_UFI)

  starting_nodes %>% map(.f = function(starting_node) {

    profvis({

      print(starting_node)

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
        if(current_tracked_distance > buffer_size) break

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

    })

  })

}



vis_res <- function(result) {
  copal <- colorNumeric(palette = 'Reds', domain =  result$walking_time, reverse = T)
  leaflet(result %>%
            left_join(road_infra_joined %>% mutate(UFI = as.character(UFI)), by = 'UFI') %>%
            st_set_geometry('geometry') %>%
            st_transform('wgs84')) %>%
    addProviderTiles('CartoDB.Positron') %>%
    addCircleMarkers(color = ~copal(result$walking_time)) %>% print
}
