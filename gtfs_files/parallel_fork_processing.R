# Fork-based parallel batch isochrone processing using shared memory

parallel_fork_processing <- function(result, walking_distances) {

  # Configuration
  num_cores <- 8  # Adjust based on your system
  batch_size <- 15  # Keep smaller to avoid cartesian join issues


  message("Setting up fork cluster with ", num_cores, " workers")
  message("Objects in memory before forking:")
  message("  result size: ", format(object.size(result), "MB"))
  message("  walking_distances size: ", format(object.size(walking_distances), "MB"))

  # Setup fork cluster - objects are shared via copy-on-write
  cl <- makeCluster(num_cores, type = "FORK")
  registerDoParallel(cl)

  # Create batches
  all_stops <- sample(unique_stops)
  stop_batches <- split(all_stops, ceiling(seq_along(all_stops) / batch_size))
  message("Processing ", length(all_stops), " stops in ", length(stop_batches), " batches")

  # Function to process a single batch
  process_batch <- function(batch_data) {

    setkey(result, 'source_stop_id')

    start_stop_ids <- batch_data$stops
    batch_idx <- batch_data$idx

    # Objects (result, walking_distances, stop_id_to_name) are already available via shared memory
    message("Worker processing batch ", batch_idx, " with ", length(start_stop_ids), " stops")
    batch_start_time <- Sys.time()

    # === P1: First iteration (BATCHED) ===
    p1_raw <- result[.(start_stop_ids), nomatch=0L]

    # Add batch identifier
    start_lookup <- data.table(
      source_stop_id = start_stop_ids,
      batch_start_id = start_stop_ids
    )
    setkey(start_lookup, source_stop_id)

    p1_raw[start_lookup, batch_start_id := i.batch_start_id, on = .(source_stop_id)]

    # Group by both batch_start_id and destination stop_id
    p1_travel_times <- p1_raw[, .SD[which.max(minutes_until_time_limit)],
                              by = .(batch_start_id, stop_id)][, .(
                                travel_time_from_p1 = 46 - minutes_until_time_limit,
                                source_stop_id = stop_id,
                                batch_start_id
                              )]
    setkey(p1_travel_times, source_stop_id)

    # === P2: Second iteration (BATCHED) ===
    p2_stop_ids <- unique(p1_travel_times$source_stop_id)

    if(length(p2_stop_ids) == 0) {
      message("Batch ", batch_idx, " - No P2 connections found")
      return(list(
        batch_idx = batch_idx,
        duration = 0,
        stops_processed = length(start_stop_ids),
        p1_connections = 0,
        p2_connections = 0,
        walking_connections = 0,
        files_written = 0
      ))
    }

    p2_raw <- result[.(p2_stop_ids), nomatch=0L]

    # Join with travel times - using allow.cartesian for known large joins
    master_p2 <- p1_travel_times[p2_raw, on = .(source_stop_id), nomatch=0L, allow.cartesian=TRUE][
      time_margin > travel_time_from_p1 + walking_time
    ][, .SD[which.max(minutes_until_time_limit)], by = .(batch_start_id, stop_id)]

    # === P3: Walking connections (BATCHED) ===
    if(nrow(master_p2) == 0) {
      message("Batch ", batch_idx, " - No P3 connections found")
      return(list(
        batch_idx = batch_idx,
        duration = 0,
        stops_processed = length(start_stop_ids),
        p1_connections = nrow(p1_travel_times),
        p2_connections = 0,
        walking_connections = 0,
        files_written = 0
      ))
    }

    p2_travel_times <- master_p2[, .(
      total_time_travelled = 46 - minutes_until_time_limit,
      travel_time_from_p1,
      source_stop_id = stop_id,
      time_left = minutes_until_time_limit,
      batch_start_id
    )]
    setkey(p2_travel_times, source_stop_id)

    max_time_left <- max(p2_travel_times$time_left, na.rm = TRUE)

    # walking_cons <- walking_distances[walking_time <= max_time_left][
    #   p2_travel_times,on = .(origin_stop_id = source_stop_id), nomatch=0L,
    # ][walking_time <= time_left]

    walking_cons <- p2_travel_times[walking_distances, on = .(source_stop_id = origin_stop_id), nomatch=0L, allow.cartesian=TRUE][
      walking_time <= max_time_left & walking_time <= time_left
    ]
    walking_cons[, time_to_point := total_time_travelled + walking_time]


    # Write results to files within worker
    files_written <- 0
    if(nrow(walking_cons) > 0) {
      walking_cons[, {
        # Keep only the best (maximum) time_left per destination
        best_destinations <- .SD[, .SD[which.min(time_to_point)], by = destination_stop_id]
        fwrite(best_destinations[, .(destination_stop_id, walking_time, time_to_point)],
               paste0('stop_isochrones/', batch_start_id, '.csv'))
        NULL
      }, by = batch_start_id]
      files_written <- length(unique(walking_cons$batch_start_id))
    }

    batch_end_time <- Sys.time()
    batch_duration <- as.numeric(difftime(batch_end_time, batch_start_time, units = "secs"))

    message("Batch ", batch_idx, " completed in ", round(batch_duration, 2), " seconds")

    # Return summary stats
    return(list(
      batch_idx = batch_idx,
      duration = batch_duration,
      stops_processed = length(start_stop_ids),
      p1_connections = nrow(p1_travel_times),
      p2_connections = nrow(master_p2),
      walking_connections = nrow(walking_cons),
      files_written = files_written
    ))
  }

  # Prepare batch data with indices
  batch_data_list <- lapply(seq_along(stop_batches), function(i) {
    list(stops = stop_batches[[i]], idx = i)
  })

  # Run batches in parallel using fork cluster
  message("\nStarting parallel processing...")
  overall_start_time <- Sys.time()

  # Use parLapply instead of future_map - no data transfer needed
  batch_results <- parLapplyLB(cl, batch_data_list, process_batch)

  overall_end_time <- Sys.time()
  total_duration <- as.numeric(difftime(overall_end_time, overall_start_time, units = "secs"))

  # Clean up cluster
  stopCluster(cl)
  registerDoSEQ()  # Reset to sequential

  # Summarize results
  total_stops <- sum(sapply(batch_results, function(x) x$stops_processed))
  total_files <- sum(sapply(batch_results, function(x) x$files_written))
  avg_duration_per_batch <- mean(sapply(batch_results, function(x) x$duration))
  avg_duration_per_stop <- total_duration / total_stops

  message("\n=== FORK PARALLEL BATCH PROCESSING COMPLETE ===")
  message("Total time: ", round(total_duration, 2), " seconds")
  message("Total stops processed: ", total_stops)
  message("Total files written: ", total_files)
  message("Average time per batch: ", round(avg_duration_per_batch, 2), " seconds")
  message("Average time per stop: ", round(avg_duration_per_stop, 3), " seconds")

  # Calculate theoretical vs actual speedup
  sequential_estimate <- avg_duration_per_batch * length(stop_batches)
  actual_speedup <- sequential_estimate / total_duration
  message("Theoretical sequential time: ", round(sequential_estimate, 2), " seconds")
  message("Actual speedup: ", round(actual_speedup, 2), "x")

  # Print detailed batch statistics
  batch_stats <- data.table(
    batch = sapply(batch_results, function(x) x$batch_idx),
    duration = sapply(batch_results, function(x) x$duration),
    stops = sapply(batch_results, function(x) x$stops_processed),
    p1_conn = sapply(batch_results, function(x) x$p1_connections),
    p2_conn = sapply(batch_results, function(x) x$p2_connections),
    walking_conn = sapply(batch_results, function(x) x$walking_connections),
    files = sapply(batch_results, function(x) x$files_written)
  )
  batch_stats[, time_per_stop := duration / stops]

  message("\nBatch performance summary:")
  print(batch_stats[order(batch)])

  message("\nFork cluster processing completed successfully!")

}
