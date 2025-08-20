# Create time-expanded graph from place_registry

# Create time-expanded nodes (stop_id_time_point)
source_nodes <- paste(place_registry$source_stop_id, place_registry$mins_left_at_dep_time, sep = "_")
dest_nodes <- paste(place_registry$stop_id, place_registry$minutes_until_time_limit, sep = "_")

# All unique nodes in the graph
all_nodes <- unique(c(source_nodes, dest_nodes))

# Create edges with time-expanded endpoints and travel time weights
edges_dt <- data.table(
  from = source_nodes,
  to = dest_nodes,
  weight = place_registry$mins_left_at_dep_time - place_registry$minutes_until_time_limit,
  source_stop_id = place_registry$source_stop_id,
  dest_stop_id = place_registry$stop_id
)

# Create numeric index mapping for performance
node_to_idx <- setNames(seq_along(all_nodes), all_nodes)
idx_to_node <- setNames(all_nodes, seq_along(all_nodes))

# Convert edges to use numeric indices
edges_dt[, from_idx := node_to_idx[from]]
edges_dt[, to_idx := node_to_idx[to]]

# Create adjacency list with integer indices for fast Dijkstra traversal
adjacency_list <- split(edges_dt[, .(to_idx, weight)], edges_dt$from_idx)


# Time-expanded Dijkstra for transit routing with integer indices
dijkstra_transit <- function(starting_node, max_time = 46) {

  starting_idx <- node_to_idx[starting_node]

  # Set up distance tracking with numeric vectors for O(1) lookups
  distances_vec <- rep(Inf, length(all_nodes))
  distances_vec[starting_idx] <- 0
  visited_vec <- rep(FALSE, length(all_nodes))

  # Track best time remaining per stop for dominance pruning
  best_time_remaining <- list()  # stop_id -> max_time_remaining_seen

  # Track unvisited nodes by index
  unvisited_idx <- seq_along(all_nodes)

  while(length(unvisited_idx) > 0) {

    # Find closest unvisited node using integer indexing
    unvisited_distances <- distances_vec[unvisited_idx]

    # Early termination if all remaining distances > max_time
    if(all(unvisited_distances >= max_time)) break

    min_pos <- which.min(unvisited_distances)
    current_idx <- unvisited_idx[min_pos]
    current_distance <- unvisited_distances[min_pos]

    print(current_idx)

    # Early termination if closest node exceeds max_time
    if(current_distance >= max_time) break

    # Extract stop_id and time_remaining from node name
    current_node_name <- idx_to_node[current_idx]
    node_parts <- strsplit(current_node_name, "_")[[1]]
    stop_id <- node_parts[1]
    time_remaining <- max_time - current_distance

    # Skip if we've already visited this stop with more time remaining
    if(stop_id %in% names(best_time_remaining)) {
      if(best_time_remaining[[stop_id]] >= time_remaining) {
        # Skip this node - already have better path to this stop
        unvisited_idx <- unvisited_idx[-min_pos]
        next
      }
    }

    # Update best time remaining for this stop
    best_time_remaining[[stop_id]] <- time_remaining

    # Remove from unvisited and mark as visited
    unvisited_idx <- unvisited_idx[-min_pos]
    visited_vec[current_idx] <- TRUE

    # Get reachable nodes from adjacency list using integer index
    reachable_edges <- adjacency_list[current_idx][[1]]

    if(!is.null(reachable_edges) && nrow(reachable_edges) > 0) {
      neighbor_idx <- reachable_edges$to_idx
      neighbor_distances <- distances_vec[neighbor_idx]
      new_distances <- reachable_edges$weight + current_distance
      update_mask <- new_distances < neighbor_distances & !is.na(neighbor_distances)

      if(any(update_mask)) {
        distances_vec[neighbor_idx[update_mask]] <- new_distances[update_mask]
      }
    }
  }

  # Return results for visited nodes within time limit
  visited_nodes <- all_nodes[visited_vec & distances_vec <= max_time]
  result <- data.table(
    node = visited_nodes,
    distance = distances_vec[visited_vec & distances_vec <= max_time]
  )

  return(result)
}

# Test with sample start
sample_start <- '14315_45'
result <- dijkstra_transit(sample_start)

res <- result
res$stop_id = str_split_i(res$node, '_', 1)

res = res %>% left_join(stops, by = 'stop_id') %>% as.data.frame() %>% st_set_geometry('geometry')

leaflet(res) %>% addProviderTiles('CartoDB.Positron') %>% addCircleMarkers()
