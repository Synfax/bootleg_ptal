# Time-expanded Dijkstra routing with numeric indexing and temporal pruning
# Author: Claude Code
# Purpose: Fast transit routing using place_registry with memory-efficient temporal dominance

library(data.table)

setkey(place_registry, source_stop_id)

ss <- place_registry['14315']

dijkstra_transit_routing <- function(place_registry, starting_stops, max_time = 46) {

  # =============================================================================
  # STEP 1: Create numeric vertex mapping system
  # =============================================================================

  # Extract all unique (stop_id, time) pairs from place_registry
  source_pairs <- place_registry[, .(stop_id = source_stop_id, time = mins_left_at_dep_time)]
  dest_pairs <- place_registry[, .(stop_id, time = minutes_until_time_limit)]

  # Get all unique vertex pairs and create numeric mapping
  all_vertex_pairs <- unique(rbind(source_pairs, dest_pairs))
  setkey(all_vertex_pairs, stop_id, time)

  # Create numeric vertex indices (much faster than character lookups)
  all_vertex_pairs[, vertex_index := .I]

  # Create fast lookup tables
  vertex_to_index <- all_vertex_pairs$vertex_index
  names(vertex_to_index) <- paste0(all_vertex_pairs$stop_id, "_", all_vertex_pairs$time)

  # Store vertex metadata for quick access during algorithm
  # Convert remaining time to elapsed time for consistency
  vertex_metadata <- data.table(
    vertex_index = all_vertex_pairs$vertex_index,
    stop_id = all_vertex_pairs$stop_id,
    time_elapsed = max_time - all_vertex_pairs$time  # Convert remaining -> elapsed
  )
  setkey(vertex_metadata, vertex_index)

  message("Created ", nrow(all_vertex_pairs), " vertices with numeric indexing")

  # =============================================================================
  # STEP 2: Build adjacency list with numeric indices
  # =============================================================================

  # Create edges using numeric indices
  edges_dt <- place_registry[, .(trip_id, stop_id, minutes_until_time_limit, walking_time, mins_left_at_dep_time, time_margin, source_stop_id)]

  # Map to numeric indices
  edges_dt[, from_index := vertex_to_index[paste0(source_stop_id, "_", from_time)]]
  edges_dt[, to_index := vertex_to_index[paste0(stop_id, "_", to_time)]]

  # Group by source stop to get ALL possible departures from each stop
  adjacency_list <- split(
    edges_dt[, ],
    edges_dt$source_stop
  )

  message("Created adjacency list with ", nrow(edges_dt), " edges")

  # =============================================================================
  # STEP 3: Temporal pruning Dijkstra with pure numeric operations
  # =============================================================================

  dijkstra_with_pruning <- function(start_vertex_index) {

    num_vertices <- max(vertex_metadata$vertex_index)

    # Distance tracking (numeric vectors for O(1) access)
    distances <- rep(Inf, num_vertices)
    distances[start_vertex_index] <- 0
    visited <- rep(FALSE, num_vertices)

    # Temporal dominance: track best time seen per stop_id
    # Use numeric stop indices for speed
    unique_stops <- unique(vertex_metadata$stop_id)
    stop_to_numeric <- setNames(seq_along(unique_stops), unique_stops)
    best_time_per_stop <- rep(0, length(unique_stops))

    # Priority queue as simple vector (will optimize later)
    unvisited <- seq_len(num_vertices)[distances < Inf]

    while(length(unvisited) > 0) {

      # Find minimum distance vertex
      current_distances <- distances[unvisited]
      min_pos <- which.min(current_distances)
      current_index <- unvisited[min_pos]
      current_distance <- current_distances[min_pos]

      print(current_index)

      # Early termination if exceed max_time
      if(current_distance >= max_time) break

      # Get vertex metadata using numeric lookup
      current_meta <- vertex_metadata[current_index]
      current_stop_numeric <- stop_to_numeric[current_meta$stop_id]
      current_time_remaining <- max_time - current_distance

      #Sanity check: current_distance should match vertex time_elapsed
      if(abs(current_distance - current_meta$time_elapsed) > 0.1) {
        warning("Time inconsistency detected")
      }

      # Temporal pruning: skip if dominated by earlier visit
      if(best_time_per_stop[current_stop_numeric] >= current_time_remaining) {
        unvisited <- unvisited[-min_pos]
        next
      }

      # Update dominance tracking
      best_time_per_stop[current_stop_numeric] <- current_time_remaining

      # Mark as visited and remove from queue
      visited[current_index] <- TRUE
      unvisited <- unvisited[-min_pos]

      # Process neighbors using adjacency list
      # Get current stop from vertex metadata
      current_stop <- current_meta$stop_id

      # Get ALL possible departures from this stop
      neighbors <- adjacency_list[[current_stop]]

      if(!is.null(neighbors) && nrow(neighbors) > 0) {

        # Filter departures we can actually catch based on our arrival time
        # We can catch departures that still have >= our remaining time
        catchable_neighbors <- neighbors[from_time <= current_time_remaining]

        # Additional filter: time_margin should accommodate our travel time to get here
        valid_neighbors <- catchable_neighbors[time_margin >= current_distance]

        if(nrow(valid_neighbors) > 0) {
          new_distances <- current_distance + valid_neighbors$travel_time

          # Vectorized distance updates
          update_mask <- new_distances < distances[valid_neighbors$to_index]
          if(any(update_mask)) {
            distances[valid_neighbors$to_index[update_mask]] <- new_distances[update_mask]

            # Add newly discovered vertices to unvisited queue
            newly_reachable <- valid_neighbors$to_index[update_mask & !visited[valid_neighbors$to_index]]
            unvisited <- unique(c(unvisited, newly_reachable))
          }
        }
      }
    }

    # Return reachable vertices within time limit
    reachable_indices <- which(visited & distances <= max_time)

    res <- data.table(
        vertex_index = reachable_indices,
        travel_time = distances[reachable_indices]
      )

    # return(data.table(
    #   vertex_index = reachable_indices,
    #   travel_time = distances[reachable_indices]
    # ))
  }



  # =============================================================================
  # STEP 4: Process multiple starting stops
  # =============================================================================

  # Find starting vertex indices
  starting_vertex_names <- paste0(starting_stops, "_", max_time)
  starting_indices <- vertex_to_index[starting_vertex_names]
  starting_indices <- starting_indices[!is.na(starting_indices)]

  if(length(starting_indices) == 0) {
    stop("No valid starting vertices found")
  }

  message("Starting Dijkstra from ", length(starting_indices), " vertices")

  # Run Dijkstra for each starting point
  all_results <- rbindlist(lapply(starting_indices, function(start_index) {
    result <- dijkstra_with_pruning(start_index)
    result[, start_vertex_index := start_index]
    return(result)
  }))

  # Join back vertex metadata for readable output
  final_results <- all_results[vertex_metadata, on = "vertex_index"]

  return(final_results)
}
