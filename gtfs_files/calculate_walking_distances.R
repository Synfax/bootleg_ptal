calculate_walking_distances <- function() {

  if(exists()) {

    #return(readRDS('temp/test_dt.Rdata'))

  } else {

    list_of_stops <- gtfs_pre_stops
    full_duration <- (as.numeric(as.duration(isochrone_params$time_limit - hms(isochrone_params$start_time)), 'minutes'))
    max_reachable_distance <- 46 * 84

    sf_use_s2(F)
    stops_within_distance <- st_is_within_distance(stops, dist = units::set_units(max_reachable_distance, 'm'), sparse = T, remove_self = F)

    walking_distances <- rbindlist(lapply(1:nrow(stops), function(matrix_index) {
      print(matrix_index)

      stop_id <- list_of_stops[matrix_index]
      row_numbers_within_max_dist <- unlist(aa[matrix_index])
      stops_within_max_dist <- stops[row_numbers_within_max_dist,]
      distances <- units::drop_units(st_distance(stops[matrix_index,], stops_within_max_dist))
      walking_times <- distances[1,] %/% 84

      # Return data.table directly instead of named vector
      data.table(
        origin_stop_id = stop_id,
        destination_stop_id = stops_within_max_dist$stop_id,
        walking_time = as.numeric(walking_times)
      )
    }))

    setkey(walking_distances, origin_stop_id, destination_stop_id)

    saveRDS(walking_distances, 'rdata_output/walking_distances.Rdata')

    return(walking_distances)

  }

}

