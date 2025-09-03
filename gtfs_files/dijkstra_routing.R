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

  # Pre-compute vertex metadata as arrays for O(1) direct indexing in hot loop
  max_vertex_index <- max(all_vertex_pairs$vertex_index)
  vertex_stop_ids <- character(max_vertex_index)
  vertex_time_remaining <- numeric(max_vertex_index)

  # Also pre-compute stop numeric indices to avoid string lookups
  unique_stops <- unique(all_vertex_pairs$stop_id)
  stop_to_numeric <- setNames(seq_along(unique_stops), unique_stops)
  vertex_stop_numeric <- numeric(max_vertex_index)

  vertex_stop_ids[all_vertex_pairs$vertex_index] <- all_vertex_pairs$stop_id
  vertex_time_remaining[all_vertex_pairs$vertex_index] <- all_vertex_pairs$time
  vertex_stop_numeric[all_vertex_pairs$vertex_index] <- stop_to_numeric[all_vertex_pairs$stop_id]

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

  # Pre-compute destination vertex indices to avoid lookups during algorithm
  edges_dt[, dest_vertex_index := {
    dest_lookup <- .SD[, .(stop_id, time = minutes_until_time_limit)]
    dest_matches <- vertex_metadata[dest_lookup, on = c("stop_id", "time_remaining" = "time"), nomatch = 0L]
    dest_matches$vertex_index
  }]

  # Remove edges with invalid destinations
  edges_dt <- edges_dt[!is.na(dest_vertex_index)]

  # Convert adjacency list to use numeric stop indices for faster lookup
  edges_dt[, source_stop_numeric := stop_to_numeric[source_stop_id]]

  # Create proper numeric-indexed adjacency list (array of data.tables)
  adjacency_list <- vector("list", length(unique_stops))

  # Split by numeric stop index and populate array
  edge_splits <- split(edges_dt, by = "source_stop_numeric")

  # Fill adjacency list ensuring all stops have entries (even if empty)
  for(i in seq_along(unique_stops)) {
    if(as.character(i) %in% names(edge_splits)) {
      adjacency_list[[i]] <- edge_splits[[as.character(i)]]
    } else {
      adjacency_list[[i]] <- data.table()  # Empty data.table for stops with no edges
    }
  }

  message("Created adjacency list for ", length(adjacency_list), " stops")

  # =============================================================================
  # STEP 3: Dijkstra with time-expanded vertices and stop-based adjacency
  # =============================================================================

  dijkstra_with_pruning <- function(start_vertex_index) {

    profvis({

      num_vertices <- max(vertex_metadata$vertex_index)

      # Only track visited vertices - no distance tracking needed
      visited <- rep(FALSE, num_vertices)

      # Track best time remaining seen per stop for temporal dominance (use pre-computed arrays)
      best_time_per_stop <- rep(0, length(unique_stops))  # numeric array for O(1) access

      # Queue of vertices to process (start with starting vertex)
      queue <- c(start_vertex_index)
      queue_head <- 1

      while(queue_head <= length(queue)) {

        # Process next vertex from queue
        current_index <- queue[queue_head]
        queue_head <- queue_head + 1

        # Skip if already visited
        if(visited[current_index]) next

        # Get current vertex info using O(1) array access (no data.table lookup!)
        current_stop <- vertex_stop_ids[current_index]
        current_time_remaining <- vertex_time_remaining[current_index]
        current_elapsed_time <- max_time - current_time_remaining

        # Temporal dominance: skip if we've already visited this stop with more time remaining
        current_stop_numeric <- vertex_stop_numeric[current_index]
        if(best_time_per_stop[current_stop_numeric] >= current_time_remaining) {
          # Skip this vertex - already have better time state for this stop
          next
        }

        # Update best time remaining for this stop
        best_time_per_stop[current_stop_numeric] <- current_time_remaining

        # Mark as visited
        visited[current_index] <- TRUE

        # Get ALL possible departures from this stop (using numeric index)
        neighbors <- adjacency_list[[current_stop_numeric]]

        #if(!is.null(neighbors) && nrow(neighbors) > 0) {

          # Filter using vectorized logical indexing (potentially faster than data.table subset)
          valid_mask <- neighbors$time_margin >= current_elapsed_time

          if(any(valid_mask)) {
            # Get pre-computed destination vertex indices using logical indexing
            dest_indices <- neighbors$dest_vertex_index[valid_mask]

            # Add unvisited destinations to queue
            new_vertices <- dest_indices[!visited[dest_indices]]
            queue <- unique(c(queue, new_vertices))
          }
          #}
        #}
      }

      # Return all visited vertices
      reachable_indices <- which(visited)

      result <- data.table(
        vertex_index = reachable_indices
      )

      # Add stop_id and time info for interpretation
      #result[vertex_metadata, `:=`(stop_id = i.stop_id, time_remaining = i.time_remaining), on = "vertex_index"]
      result[, `:=` (stop_id = (vertex_stop_ids[vertex_index]), time_remaining = vertex_time_remaining[vertex_index]) ]
      setkey(result, stop_id)

      #profvis({
        # final_destinations <- result[walking_access_dict, on = 'stop_id', nomatch = 0L][
        #   walking_time <= time_remaining  # Filter to walkable destinations
        # ]

      #})

      final_destinations <- result[walking_access_dict, on = 'stop_id', nomatch = 0L][
        walking_time <= time_remaining  # Filter to walkable destinations
      ]


      mesh_blocks = as.character(unique(final_destinations$MB_CODE21))
      employment = sum(mb_employment_dict[mesh_blocks])

      # UFIs <- as.character(unique(final_destinations$UFI))
      # employment <- sum(ufi_employment_fractions[UFIs]$total_allocated_employment)

    })

    return(data.table(empl = employment))
  }

  # =============================================================================
  # STEP 4: Process starting stops
  # =============================================================================

  # Find starting vertex indices (stops at max_time)

  vertex_metadata %>%
    as.data.frame() %>%
    group_by(stop_id) %>%
    slice_max(time_remaining) %>%
    mutate(starting_vertex_names = paste0(stop_id,'_',time_remaining)) -> starting_vertices


  starting_indices <- vertex_to_index[starting_vertices$starting_vertex_names]
  starting_indices <- starting_indices[!is.na(starting_indices)]

  if(length(starting_indices) == 0) {
    stop("No valid starting vertices found")
  }

  message("Starting Dijkstra from ", length(starting_indices), " vertices")

  starting_indices = starting_indices[sample(length(starting_indices), length(starting_indices))]

  # Run Dijkstra for each starting point
  all_results <- rbindlist(lapply(starting_indices, function(start_index) {
    result <- dijkstra_with_pruning(start_index)
    result[, start_vertex_index := start_index]
    return(result)
  }))

  all_results[, stop_id := vertex_stop_ids[start_vertex_index]]

  {
    all_results %>%
      as.data.frame() %>%
      left_join(stops, by = 'stop_id') %>%
      st_set_geometry('geometry')-> res_sf

    st_write(res_sf, 'sf_output/res_sf.gpkg', append = F)
  }

  return(all_results)
}
