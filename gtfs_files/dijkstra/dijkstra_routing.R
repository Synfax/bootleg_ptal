dijkstra_transit_routing <- function(doParallel, num_cores) {

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
  # adjacency_list <- vector("list", length(unique_stops))
  #
  # # Split by numeric stop index and populate array
  # edge_splits <- split(edges_dt, by = "source_stop_numeric")
  #
  # # Fill adjacency list ensuring all stops have entries (even if empty)
  # for(i in seq_along(unique_stops)) {
  #   if(as.character(i) %in% names(edge_splits)) {
  #     adjacency_list[[i]] <- edge_splits[[as.character(i)]]
  #   } else {
  #     adjacency_list[[i]] <- data.table()  # Empty data.table for stops with no edges
  #   }
  # }
  Rcpp::sourceCpp('cpp/bfs_routing.cpp')
  # message("Created adjacency list for ", length(adjacency_list), " stops")


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
  # STEP 4: Build walking access CSR + amenity matrix for C++
  # =============================================================================

  amenity_cols <- setdiff(names(master_amenity_dt), 'MB_CODE21')

  # MB numeric mapping
  all_mbs <- unique(walking_access_dict$MB_CODE21)
  mb_to_numeric <- setNames(seq_along(all_mbs), all_mbs)
  cpp_n_mbs <- as.integer(length(all_mbs))

  # Walking access CSR keyed by stop_numeric
  walk_dt <- walking_access_dict[, .(
    stop_numeric = stop_to_numeric[stop_id],
    mb_numeric = mb_to_numeric[MB_CODE21],
    walking_time
  )]
  walk_dt <- walk_dt[!is.na(stop_numeric)]
  setorder(walk_dt, stop_numeric)

  cpp_walk_offsets <- as.integer(c(0L, cumsum(tabulate(walk_dt$stop_numeric, nbins = length(unique_stops)))))
  cpp_walk_mb <- as.integer(walk_dt$mb_numeric - 1L)
  cpp_walk_time <- as.numeric(walk_dt$walking_time)

  # Amenity matrix (rows = mb_numeric order, cols = amenity_cols)
  amenity_lookup_dt <- data.table(MB_CODE21 = all_mbs, mb_idx = seq_along(all_mbs))
  amenity_joined <- master_amenity_dt[amenity_lookup_dt, on = 'MB_CODE21']
  setorder(amenity_joined, mb_idx)
  cpp_amenity_matrix <- as.matrix(amenity_joined[, ..amenity_cols])
  cpp_amenity_matrix[is.na(cpp_amenity_matrix)] <- 0

  message("Built walking CSR (", length(cpp_walk_mb), " entries) and amenity matrix (", cpp_n_mbs, " MBs x ", length(amenity_cols), " cols)")

  # =============================================================================
  # STEP 5: Find starting vertices
  # =============================================================================

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
  message("Starting BFS from ", length(starting_indices), " vertices")

  starting_indices = starting_indices[sample(length(starting_indices), length(starting_indices))]

  # =============================================================================
  # STEP 6: BFS + post-processing per starting vertex (all in C++)
  # =============================================================================

  message("Running BFS + post-processing for ", length(starting_indices), " starting vertices...")
  tic()

  # Worker function: BFS + walking join + amenity sum all in C++
  process_vertex <- function(start_index) {
    result <- bfs_with_post_processing(
      start_vertex_index = start_index - 1L,
      n_vertices = cpp_n_vertices,
      n_stops = cpp_n_stops,
      vertex_time_remaining = vertex_time_remaining,
      vertex_stop_numeric = cpp_vertex_stop_numeric,
      max_time = max_time,
      adj_offsets = cpp_adj_offsets,
      adj_dest = cpp_adj_dest,
      adj_margin = adj_margin,
      walk_offsets = cpp_walk_offsets,
      walk_mb_numeric = cpp_walk_mb,
      walk_time = cpp_walk_time,
      amenity_matrix = cpp_amenity_matrix,
      n_mbs = cpp_n_mbs
    )

    if(length(result$mb_numeric) == 0L) return(NULL)

    list(
      start_vertex_index = start_index,
      amenity_sums = result$amenity_sums,
      mb_codes = all_mbs[result$mb_numeric],
      travel_times = result$travel_times
    )
  }

  if(doParallel) {
    cl <- makeCluster(num_cores, type = "FORK")
    message('FORK cluster established with ', num_cores, ' workers')
    results_list <- parLapplyLB(cl = cl, X = starting_indices, fun = process_vertex, chunk.size = 10)
    stopCluster(cl)
    gc()
  } else {
    results_list <- lapply(starting_indices, process_vertex)
  }

  # Filter NULLs and assemble into data.table from pre-allocated vectors
  results_list <- results_list[!vapply(results_list, is.null, logical(1))]
  n <- length(results_list)

  start_vertex_vec <- integer(n)
  amenity_matrix <- matrix(0, nrow = n, ncol = length(amenity_cols), dimnames = list(NULL, amenity_cols))
  mb_list <- vector("list", n)
  tt_list <- vector("list", n)

  for(i in seq_len(n)) {
    r <- results_list[[i]]
    start_vertex_vec[i] <- r$start_vertex_index
    amenity_matrix[i, ] <- r$amenity_sums
    mb_list[[i]] <- r$mb_codes
    tt_list[[i]] <- r$travel_times
  }

  all_results <- data.table(
    start_vertex_index = start_vertex_vec,
    as.data.table(amenity_matrix),
    mesh_block_list = mb_list,
    travel_times = tt_list
  )

  all_results[, stop_id := vertex_stop_ids[start_vertex_index]]

  toc()
  end_time <- Sys.time()
  message('total time elapsed:', (end_time - start_time))

  return(all_results)
}
























