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

  # Pre-compute Rcpp arguments once (avoid repeated conversions)
  cpp_n_vertices <- as.integer(max_vertex_index)
  cpp_n_stops <- as.integer(length(unique_stops))
  cpp_vertex_stop_numeric <- as.integer(vertex_stop_numeric - 1L)
  cpp_adj_offsets <- as.integer(adj_offsets - 1L)
  cpp_adj_dest <- as.integer(adj_dest - 1L)

  run_bfs <- function(start_vertex_index) {
    bfs_pruned(
      start_vertex_index = start_vertex_index - 1L,
      n_vertices = cpp_n_vertices,
      n_stops = cpp_n_stops,
      vertex_time_remaining = vertex_time_remaining,
      vertex_stop_numeric = cpp_vertex_stop_numeric,
      max_time = max_time,
      adj_offsets = cpp_adj_offsets,
      adj_dest = cpp_adj_dest,
      adj_margin = adj_margin
    )
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

  # =============================================================================
  # STEP 4: BFS + post-processing per starting vertex
  # =============================================================================

  amenity_cols <- setdiff(names(master_amenity_dt), 'MB_CODE21')
  setkey(walking_access_dict, stop_id)

  message("Running BFS + post-processing for ", length(starting_indices), " starting vertices...")
  tic()

  all_results <- rbindlist(lapply(seq_along(starting_indices), function(i) {

    if(i %% 1000 == 0) message("  vertex ", i, "/", length(starting_indices))

    start_index <- starting_indices[i]

    # BFS: returns ~870 (stop_numeric, time_remaining) pairs
    result <- run_bfs(start_index)

    bfs_dt <- data.table(
      stop_id = unique_stops[result$stop_numeric],
      time_remaining = result$time_remaining
    )
    setkey(bfs_dt, stop_id)

    # Walking join: ~870 stops × ~41 MBs = ~36k rows
    destinations <- walking_access_dict[bfs_dt, on = 'stop_id', nomatch = NULL, allow.cartesian = TRUE][
      walking_time <= time_remaining
    ]

    if(nrow(destinations) == 0L) return(NULL)

    destinations[, time_remaining_incl_walking := time_remaining - walking_time]

    # Dedup: keep best arrival per MB
    setorder(destinations, MB_CODE21, -time_remaining_incl_walking)
    final <- unique(destinations, by = 'MB_CODE21')

    # Join amenities and sum in one pass
    with_amenities <- master_amenity_dt[final, on = 'MB_CODE21', nomatch = NULL]

    data.table(
      start_vertex_index = start_index,
      as.list(colSums(with_amenities[, ..amenity_cols], na.rm = TRUE)),
      mesh_block_list = list(as.character(final$MB_CODE21)),
      travel_times = list(final$time_remaining_incl_walking)
    )

  }))

  all_results[, stop_id := vertex_stop_ids[start_vertex_index]]

  toc()
  end_time <- Sys.time()
  message('total time elapsed:', (end_time - start_time))

  return(all_results)
}
























