place_registry <- function(unique_stops, isochrone_params, arrival_time_dict) {
  # Add required packages


  # Set up parallel processing
  plan(multisession, workers = parallel::detectCores() - 1)

  # Run the parallelized version
  result <- future_map(unique_stops, function(current_stop_id) {
    # Get reachable stops for current stop
    stops_to_transfer_to <- unlist(.pkgenv$reachable_stops_dict[as.character(current_stop_id)])
    valid_transfer_stops <- unname(stops_to_transfer_to)
    if(is.null(valid_transfer_stops)) {
      return(list())
    }
    # Get departures from these transfer stops
    departures_from_transferable_stops <- do.call(
      rbind,
      .pkgenv$stop_to_departures[as.character(valid_transfer_stops)]
    )
    departures_from_transferable_stops$minutes_until_time_limit = as.numeric(as.duration(isochrone_params$time_limit_ - departures_from_transferable_stops$departure_time), 'minutes')
    departure_trip_ids = departures_from_transferable_stops$trip_id
    # Filter main dataframe for these trips
    places_i_could_get_to <- do.call(rbind, .pkgenv$trip_to_destinations[departure_trip_ids] ) %>%
      group_by(trip_id) %>%
      filter(stop_sequence >= if(any(stop_id %in% valid_transfer_stops)) {
        min(stop_sequence[stop_id %in% valid_transfer_stops])
      } else {
        50  # Fallback value
      }) %>%
      ungroup()
    places_i_could_get_to$minutes_until_time_limit = as.numeric(as.duration(isochrone_params$time_limit_ - places_i_could_get_to$departure_time), 'minutes')
    places_i_could_get_to$stop_name = .pkgenv$stop_id_to_name[as.character(places_i_could_get_to$stop_id)]
    potential_arrival_times <- arrival_time_dict[as.character(current_stop_id)][[1]]
    arrival_minutes_before_time_limit <- unique(as.numeric(as.duration(isochrone_params$time_limit_ - potential_arrival_times), 'minutes'))

    sset_dfs <- arrival_minutes_before_time_limit %>% map(function(x) {
      #this prevents time travelling
      trip_ids_above_time_limit = .pkgenv$departures_from_transferable_stops[departures_from_transferable_stops$minutes_until_time_limit > x, ]$trip_id
      rows_to_sset <- which(!(places_i_could_get_to$trip_id %in% trip_ids_above_time_limit) & places_i_could_get_to$minutes_until_time_limit < x)
      subset_df <- places_i_could_get_to[rows_to_sset, ] %>%
        group_by(stop_id) %>%
        slice_max(minutes_until_time_limit)
      return(subset_df)
    }) %>% setNames(as.character(arrival_minutes_before_time_limit))

    return(sset_dfs)
  }, .options = furrr_options(seed = TRUE), .progress = TRUE) %>%
    setNames(unique_stops)

  plan('sequential')

  # Store in package environment
  .pkgenv$place_registry <- result
}
