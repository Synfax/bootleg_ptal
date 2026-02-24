dijkstra_transit_routing <- function() {

  #need some function to return vertex_metadata

  # =============================================================================
  # STEP 1: Create vertices as (stop_id, time) pairs with numeric indexing
  # =============================================================================

  # Extract all unique (stop_id, time) pairs from place_registry
  source_pairs <- place_registry[, .(stop_id = source_stop_id, time = mins_left_at_dep_time)]
  dest_pairs <- place_registry[, .(stop_id, time = minutes_until_time_limit)]


  #NEW: Logic to generate a fake start point for each stop
  #we are going to add new source pairs at mins_left_at_dep_time = max_time
  max_time = 46

  fake_start_pairs <- test[, .(stop_id, walking_time)]
  fake_start_pairs[, time := max_time - walking_time]
  fake_start_pairs[, walking_time := NULL]


  # Get all unique vertex pairs and create numeric mapping
  all_vertex_pairs <- unique(rbindlist(list(source_pairs, dest_pairs, fake_start_pairs)))
  setkey(all_vertex_pairs, stop_id, time)

  # Create numeric vertex indices
  all_vertex_pairs[, vertex_index := .I]

  # Create fast lookup: "stop_time" -> numeric_index
  vertex_to_index <- all_vertex_pairs$vertex_index

  names(vertex_to_index) <- paste0(all_vertex_pairs$stop_id, "_", all_vertex_pairs$time)

  vertex_to_index <<- vertex_to_index

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

  #CSR logic

  setorder(edges_dt, source_stop_numeric)

  {
    adj_dest = edges_dt$dest_vertex_index
    adj_margin = edges_dt$time_margin

    adj_offsets <- tabulate(edges_dt$source_stop_numeric, nbins = length(unique_stops))
    adj_offsets <- c(1, cumsum(adj_offsets) + 1)
  }

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
  Rcpp::sourceCpp('cpp/bfs_routing.cpp')
  message("Created adjacency list for ", length(adjacency_list), " stops")


  # =============================================================================
  # STEP 3: Dijkstra with time-expanded vertices and stop-based adjacency
  # =============================================================================

  dijkstra_with_pruning <- function(start_vertex_index, method = 'cpp') {

    #print(start_vertex_index)

    if(method == 'r') {

      # START MASS COMMENT ---

      num_vertices <- max(vertex_metadata$vertex_index)

      # Only track visited vertices - no distance tracking needed
      visited <- rep(FALSE, num_vertices)

      # Track best time remaining seen per stop for temporal dominance (use pre-computed arrays)
      best_time_per_stop <- rep(0, length(unique_stops))  # numeric array for O(1) access

      # Queue of vertices to process (start with starting vertex)
      queue <- integer(num_vertices)  # Pre-allocated queue
      queue[1] <- start_vertex_index
      queued <- rep(FALSE, num_vertices)  # Track what's been queued
      queued[start_vertex_index] <- TRUE
      queue_head <- 1
      queue_tail <- 1

      while(queue_head <= queue_tail) {

        # Process next vertex from queue
        current_index <- queue[queue_head]
        queue_head <- queue_head + 1

        # Skip if already visited
        if(visited[current_index]) next

        # Get current vertex info using O(1) array access (no data.table lookup!)
        #current_stop <- vertex_stop_ids[current_index] #not used
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
        #neighbors[, stop_name := stop_id_to_name[stop_id]]

        #if(!is.null(neighbors) && nrow(neighbors) > 0) {

          # Filter using vectorized logical indexing (potentially faster than data.table subset)
          valid_mask <- neighbors$time_margin >= current_elapsed_time

          if(any(valid_mask)) {
            # Get pre-computed destination vertex indices using logical indexing
            dest_indices <- neighbors$dest_vertex_index[valid_mask]

            # Add unvisited and unqueued destinations to queue
            unqueued_mask <- !visited[dest_indices] & !queued[dest_indices]
            new_vertices <- dest_indices[unqueued_mask]

            if(length(new_vertices) > 0) {
              # Add to pre-allocated queue
              next_tail <- queue_tail + length(new_vertices)
              queue[(queue_tail + 1):next_tail] <- new_vertices
              queue_tail <- next_tail

              # Mark as queued
              queued[new_vertices] <- TRUE
            }

          }
          #}
        #}
      }

      # Return all visited vertices
      reachable_indices <- which(visited)

      # END MASS COMMENT

    }

    if(method == 'cpp') {
      reachable_indices = bfs_pruned(
        start_vertex_index = start_vertex_index - 1L,
        n_vertices = as.integer(max(vertex_metadata$vertex_index)),
        n_stops = as.integer(length(unique_stops)),
        vertex_time_remaining = vertex_time_remaining,
        vertex_stop_numeric = as.integer(vertex_stop_numeric - 1L),
        max_time = max_time,
        adj_offsets = as.integer(adj_offsets - 1L),
        adj_dest = as.integer(adj_dest - 1L),
        adj_margin = adj_margin
      )
    }

      result <- data.table(
        vertex_index = reachable_indices
      )

      # Add stop_id and time info for interpretation
      #result[vertex_metadata, `:=`(stop_id = i.stop_id, time_remaining = i.time_remaining), on = "vertex_index"]
      result[, `:=` (stop_id = (vertex_stop_ids[vertex_index]), time_remaining = vertex_time_remaining[vertex_index]) ]
      setkey(result, stop_id)

      #result[, stop_name := stop_id_to_name[stop_id]]


      #OPTION 1
      {
        #find which final walking UFIs I can walk to after I get off my last connection
        final_destinations <- result[walking_access_dict, on = 'stop_id', nomatch = NULL, allow.cartesian = T][
          walking_time <= time_remaining  # Filter to walkable destinations
        ]

        final_destinations[, time_remaining_incl_walking := time_remaining - walking_time]

        #final_destinations[,.(time_remaining,MB_CODE21)][, .SD[which.min(time_remaining)], by = MB_CODE21]

        final_dt <- final_destinations[,.(time_remaining_incl_walking,MB_CODE21)][order(-time_remaining_incl_walking), .SD[1], by = MB_CODE21]


        #get the mesh block IDs of all of the UFIs I can walk to
        mesh_blocks = as.character(final_dt$MB_CODE21)
        travel_times = final_dt$time_remaining_incl_walking

        #employment = sum(mb_employment_dict[mesh_blocks]$jobs, na.rm = T)

      }

      total_amenity <- master_amenity_dt[mesh_blocks][, MB_CODE21 := NULL]




      # UFIs <- as.character(unique(final_destinations$UFI))
      # employment <- sum(ufi_employment_fractions[UFIs]$total_allocated_employment)

    #})

    final_values <- total_amenity[, lapply(.SD, sum, na.rm = TRUE)]
    final_values[, mesh_block_list := list(mesh_blocks)]
    final_values[, travel_times := list(travel_times)]
    return(final_values)
  }

  # =============================================================================
  # STEP 4: Process starting stops
  # =============================================================================

  # Find starting vertex indices (stops at max_time)




  #this is the start point logic - currently selects the earliest time at each stop
  # vertex_metadata %>%
  #   as.data.frame() %>%
  #   group_by(stop_id) %>%
  #   slice_max(time_remaining) %>%
  #   mutate(starting_vertex_names = paste0(stop_id,'_',time_remaining)) -> starting_vertices

  # starting_vertices = start_points %>%
  #   as.data.frame() %>%
  #   mutate(starting_vertex_names = paste0(stop_id,'_',time)) %>%
  #   pull(starting_vertex_names)

  starting_vertices = fake_start_pairs %>%
      as.data.frame() %>%
      mutate(starting_vertex_names = paste0(stop_id,'_',time)) %>%
      pull(starting_vertex_names) %>%
    unique()

  starting_indices <- vertex_to_index[starting_vertices]
  starting_indices <- starting_indices[!is.na(starting_indices)]

  if(length(starting_indices) == 0) {
    stop("No valid starting vertices found")
  }
  start_time <- Sys.time()
  message("Starting Dijkstra from ", length(starting_indices), " vertices")

  starting_indices = starting_indices[sample(length(starting_indices), length(starting_indices))]

  # Run Dijkstra for each starting point
  all_results <- rbindlist(lapply(starting_indices, function(start_index) {
    result <- dijkstra_with_pruning(start_index, method = 'cpp')
    result[, start_vertex_index := start_index]
    return(result)
  }))

  all_results[, stop_id := vertex_stop_ids[start_vertex_index]]
  end_time <- Sys.time()

  message('time elapsed:', (end_time - start_time))

  return(all_results)
}
























