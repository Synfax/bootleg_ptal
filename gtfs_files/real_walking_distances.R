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

all_vertices <- tr_road_infra %>%
  mutate(UFI = as.character(UFI))

##do PT linkage

transit_ufi_dict <- get_transit_ufi_dict()
# Function to process a single SA2
process_sa2 <- function(sa2) {

  #profvis({
    current_sa2_sf = sa2_sf %>%
      filter(SA2_NAME21 == sa2)

    buffer_size = minutes_willing_to_walk * 84

    #get nodes in the current SF that will be our starting points
    #now we need to find all the nodes we could walk to so we can keep the network small.
    starting_nodes = road_infra_dt[sa2]

    if(nrow(starting_nodes) == 0) {
      message("No nodes found for SA2: ", sa2, " - skipping")
      return(data.table())
    }

    starting_nodes[, UFI := as.character(UFI)]
    #
    # starting_nodes = starting_nodes[transit_ufi_dict, on = c('UFI' = 'nearest_UFI'), nomatch = NULL]
    #
    # if(nrow(starting_nodes) == 0) {
    #   message("No transit-linked nodes found for SA2: ", sa2, " - skipping")
    #   return(data.table())
    # }

    starting_nodes = starting_nodes$UFI

    #buffer the sa2
    buffered_sa2 <- st_buffer(current_sa2_sf, dist = buffer_size)

    #find which other SA2s overlap with this buffer

    overlap_indexes <- st_intersects(buffered_sa2, sa2_sf) %>%
      unlist()
    overlaps <- sa2_sf[overlap_indexes,]
    overlapping_sa2_names <- overlaps$SA2_NAME21

    #now we have all sa2s that could be reached within the absolute maximum walking time
    #this will form the network we use.
    #but we are only considering starting nodes in the sa2 in focus
    all_tr_points_in_overlap <- road_infra_dt[c(overlapping_sa2_names, sa2)]

    ## create vertices
    vertices <- all_tr_points_in_overlap %>%
      mutate(UFI = as.character(UFI)) %>%
      as.data.table()

    #create a smaller edges_list for each SA2 to reduce key retrieval times
    edges_list = edges_dt_keyed[vertices$UFI]
    edges_list = split(edges_list, edges_list$FROM_UFI)

    # Create numeric vertex mapping (no character operations)
    vertices[, vertex_index := .I]

    # Pre-compute adjacency list with numeric indices for O(1) lookup
    edges_list_numeric <- vector("list", nrow(vertices))

    # Map UFI to numeric indices
    ufi_to_index <- setNames(vertices$vertex_index, vertices$UFI)

    # Convert edges_list to use numeric indices
    for(i in seq_along(edges_list)) {

      #iterate thru the split() dt and fix it up
      current_edges <- edges_list[[i]]

      if(nrow(current_edges) > 0) {

        from_ufi <- names(edges_list)[i]
        from_index <- ufi_to_index[from_ufi]

        # Pre-compute neighbor indices
        current_edges[, to_index := ufi_to_index[TO_UFI]]
        edges_list_numeric[[from_index]] <- current_edges[!is.na(to_index)]

      }

    }

    #profvis({
    starting_nodes %>% map_dfr(.f = function(starting_node) {

      #profvis({
      #tic()

      # Get starting node index (convert UFI to numeric index)
      start_index <- ufi_to_index[starting_node]

      # Pure numeric arrays for tracking
      num_vertices <- nrow(vertices)
      distances_vec <- rep(Inf, num_vertices)
      distances_vec[start_index] <- 0
      visited_vec <- rep(FALSE, num_vertices)

      # Use head pointer queue (from dijkstra optimization)
      queue <- c(start_index)
      queue_head <- 1

      while (queue_head <= length(queue)) {
        # Get current vertex (pure numeric operations)
        current_index <- queue[queue_head]
        queue_head <- queue_head + 1

        # Skip if visited
        if(visited_vec[current_index]) next

        current_distance <- distances_vec[current_index]

        # Early termination
        if(current_distance > buffer_size) break

        # Mark as visited
        visited_vec[current_index] <- TRUE

        # Get neighbors using numeric adjacency list (no character lookup!)
        neighbors <- edges_list_numeric[[current_index]]

        if(!is.null(neighbors) && nrow(neighbors) > 0) {
          # Pure vectorized operations
          new_distances <- current_distance + neighbors$distance
          neighbor_indices <- neighbors$to_index

          # Update distances where we found better paths
          update_mask <- new_distances < distances_vec[neighbor_indices]
          if(any(update_mask)) {
            distances_vec[neighbor_indices[update_mask]] <- new_distances[update_mask]

            # Add to queue (avoiding duplicates)
            new_vertices <- neighbor_indices[update_mask & !visited_vec[neighbor_indices]]
            queue <- c(queue, new_vertices)
          }
        }
      }

      # Create result using numeric indexing
      visited_indices <- which(visited_vec)
      result <- data.table(
        start_UFI = starting_node,
        UFI = vertices$UFI[visited_indices],
        distance = distances_vec[visited_indices],
        walking_time = distances_vec[visited_indices] %/% 84
      )

      #})
      #toc()

    }) -> sa2_level_result
    #})


    fwrite(sa2_level_result, paste0('walking_isochrones_sa2/',sa2,'.csv'))

    sa2_level_result_transit = sa2_level_result[transit_ufi_dict, on = c('UFI' = 'nearest_UFI'), nomatch = NULL]

    return(sa2_level_result_transit)
  #})


}



# Main parallel processing function using FORK
run_parallel_walking_isochrones <- function() {

  num_cores = 8
  # Get list of all SA2s to process
  all_sa2s <- unique(sa2_sf$SA2_NAME21)
  message("Processing ", length(all_sa2s), " SA2s in parallel with ", num_cores, " cores using FORK")

  # Create output directory
  dir.create("walking_isochrones_sa2", showWarnings = FALSE)

  # Set up FORK cluster for shared memory
  cl <- makeCluster(num_cores, type = "FORK")

  # No need to export - FORK shares memory automatically

  # Run parallel processing with load balancing
  message("Starting parallel processing...")
  start_time <- Sys.time()

  results <- parLapplyLB(cl, all_sa2s, process_sa2)

  end_time <- Sys.time()

  # Clean up cluster
  stopCluster(cl)

  message("Parallel processing completed in ",
          round(difftime(end_time, start_time, units = "mins"), 2), " minutes")

  # Combine results if needed
  combined_results <- rbindlist(results)
  return(combined_results)

  #non parallelised version
  ############################################################################
#
#
#   all_sa2s <- unique(sa2_sf$SA2_NAME21)
#
#   start_time <- Sys.time()
#
#   results <- lapply(all_sa2s, process_sa2)
#
#   end_time <- Sys.time()
#
#   message("Parallel processing completed in ",
#           round(difftime(end_time, start_time, units = "mins"), 2), " minutes")
#
#   # Combine results if needed
#   combined_results <- rbindlist(results)
#   return(combined_results)

}

saveRDS(combined_results, 'rdata_output/walking_distances_new.Rdata')

vis_res <- function(result) {
  copal <- colorNumeric(palette = 'Reds', domain =  result$walking_time, reverse = T)
  leaflet(result %>%
            left_join(road_infra_joined %>% mutate(UFI = as.character(UFI)), by = 'UFI') %>%
            st_set_geometry('geometry') %>%
            st_transform('wgs84')) %>%
    addProviderTiles('CartoDB.Positron') %>%
    addCircleMarkers(data = (vertices %>% left_join(road_infra_joined %>% mutate(UFI = as.character(UFI)), by = 'UFI') %>% mutate(UFI = as.character(UFI), by = 'UFI')) %>%
                       st_set_geometry('geometry') %>%
                       st_transform('wgs84'), color = 'grey',  )  %>%
    addCircleMarkers(color = ~copal(result$walking_time))
}

# vis_res <- function(result) {
#   copal <- colorNumeric(palette = 'Reds', domain =  result$time_remaining, reverse = T)
#   leaflet(result %>%
#             left_join(stops, by = 'stop_id') %>%
#             st_set_geometry('geometry') %>%
#             st_transform('wgs84')) %>%
#     addProviderTiles('CartoDB.Positron') %>%
#     addCircleMarkers(color = ~copal(result$time_remaining)) %>% print
# }
