# Time-expanded Dijkstra routing with stop+time vertices and stop-based adjacency
# Clean implementation with numeric indexing
library(data.table)

dijkstra_transit_routing <- function(place_registry, starting_stops, max_time = 46) {

  # =============================================================================
  # STEP 1: Create vertices as (stop_id, time) pairs with numeric indexing
  # =============================================================================

  # Extract all unique (stop_id, time) pairs from place_registry
  source_pairs <- place_registry[, .(stop_id = source_stop_id, time = mins_left_at_dep_time)]
  dest_pairs <- place_registry[, .(stop_id, time = minutes_until_time_limit)]

  # Get all unique vertex pairs and create numeric mapping
  all_vertex_pairs <- unique(rbind(source_pairs, dest_pairs))
  setkey(all_vertex_pairs, stop_id, time)

  # Create numeric vertex indices
  all_vertex_pairs[, vertex_index := .I]

  # Create fast lookup: "stop_time" -> numeric_index
  vertex_to_index <- all_vertex_pairs$vertex_index
  names(vertex_to_index) <- paste0(all_vertex_pairs$stop_id, "_", all_vertex_pairs$time)

  # Store vertex metadata for algorithm
  vertex_metadata <- data.table(
    vertex_index = all_vertex_pairs$vertex_index,
    stop_id = all_vertex_pairs$stop_id,
    time_remaining = all_vertex_pairs$time
  )
  setkey(vertex_metadata, vertex_index)

  message("Created ", nrow(all_vertex_pairs), " time-expanded vertices")

  # =============================================================================
  # STEP 2: Build adjacency list grouped by stop (not time-vertices)
  # =============================================================================

  # Prepare edges with all necessary info
  edges_dt <- place_registry[, .(
    trip_id,
    source_stop_id,
    stop_id,
    mins_left_at_dep_time,
    minutes_until_time_limit,
    travel_time = mins_left_at_dep_time - minutes_until_time_limit,
    time_margin
  )]

  # Group by source stop to get ALL departures from each stop
  adjacency_list <- split(edges_dt, by = "source_stop_id")

  message("Created adjacency list for ", length(adjacency_list), " stops")

  # =============================================================================
  # STEP 3: Dijkstra with time-expanded vertices and stop-based adjacency
  # =============================================================================

  dijkstra_with_pruning <- function(start_vertex_index) {

    profvis({
      start_vertex_index = 58812

      num_vertices <- max(vertex_metadata$vertex_index)

      # Only track visited vertices - no distance tracking needed
      visited <- rep(FALSE, num_vertices)

      # Track best time remaining seen per stop for temporal dominance (numeric indexing)
      unique_stops <- unique(vertex_metadata$stop_id)
      stop_to_numeric <- setNames(seq_along(unique_stops), unique_stops)
      best_time_per_stop <- rep(0, length(unique_stops))  # numeric array for O(1) access

      # Queue of vertices to process (start with starting vertex)
      queue <- c(start_vertex_index)

      while(length(queue) > 0) {

        # Process next vertex from queue
        current_index <- queue[1]
        queue <- queue[-1]

        # Skip if already visited
        if(visited[current_index]) next

        # Get current vertex info (stop and time remaining encoded in vertex)
        current_meta <- vertex_metadata[current_index]
        current_stop <- current_meta$stop_id
        current_time_remaining <- current_meta$time_remaining
        current_elapsed_time <- max_time - current_time_remaining

        # Temporal dominance: skip if we've already visited this stop with more time remaining
        current_stop_numeric <- stop_to_numeric[current_stop]
        if(best_time_per_stop[current_stop_numeric] >= current_time_remaining) {
          # Skip this vertex - already have better time state for this stop
          next
        }

        # Update best time remaining for this stop
        best_time_per_stop[current_stop_numeric] <- current_time_remaining

        # Mark as visited
        visited[current_index] <- TRUE

        # Get ALL possible departures from this stop
        neighbors <- adjacency_list[[current_stop]]

        if(!is.null(neighbors) && nrow(neighbors) > 0) {

          # Filter to valid connections based on time constraints
          valid_neighbors <- neighbors[
            mins_left_at_dep_time <= current_time_remaining &  # Can catch this departure
              time_margin >= current_elapsed_time                # Sufficient slack time
          ]

          if(nrow(valid_neighbors) > 0) {

            # Calculate destination vertex indices
            dest_vertex_names <- paste0(valid_neighbors$stop_id, "_", valid_neighbors$minutes_until_time_limit)
            dest_indices <- vertex_to_index[dest_vertex_names]
            dest_indices <- dest_indices[!is.na(dest_indices)]  # Remove invalid destinations

            if(length(dest_indices) > 0) {
              # Add unvisited destinations to queue
              new_vertices <- dest_indices[!visited[dest_indices]]
              queue <- unique(c(queue, new_vertices))
            }
          }
        }
      }

      # Return all visited vertices
      reachable_indices <- which(visited)

      result <- data.table(
        vertex_index = reachable_indices
      )

      # Add stop_id and time info for interpretation
      result[vertex_metadata, `:=`(stop_id = i.stop_id, time_remaining = i.time_remaining), on = "vertex_index"]

    })


    return(result)
  }

  # =============================================================================
  # STEP 4: Process starting stops
  # =============================================================================

  # Find starting vertex indices (stops at max_time)
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

  return(all_results)
}
