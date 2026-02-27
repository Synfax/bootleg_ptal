generate_place_registry <- function(doParallel, num_cores) {


  departures_dt <- setDT(gtfs_prefilter %>%
                           select(stop_id, trip_id, departure_time, route_id, stop_sequence) %>%
                           mutate(minutes_until_time_limit = as.numeric(as.duration(isochrone_params$time_limit_ - hms(departure_time)), 'minutes')) %>%
                           select(-departure_time))

  departures_dt[, stop_id := as.character(stop_id)]

  departures_stop_dt <- setkey(copy(departures_dt), stop_id)
  departures_trip_dt <- setkey(copy(departures_dt), trip_id)


  #set the number of cores to work on in parallel
  #core count < RAM/5
  #e.g 32gb RAM, 6 cores would do, but you cant alloc

  # Set up parallel processing

  if(doParallel) {
    message('starting to process in parallel with ', num_cores, ' cores (FORK)')
  } else {
    message('processing sequentially')
  }

  setkey(walking_distances, start_UFI)

  setkey(transit_ufi_dict, stop_id)

  process_stop <- function(current_stop_id) {
      walking_dist_key <- transit_ufi_dict[current_stop_id]$nearest_UFI

      #get which stops I can walk to
      stops_to_transfer_to <- walking_distances[.(walking_dist_key), nomatch = NULL]
      stops_to_transfer_to[, destination_stop_id := stop_id]
      stops_to_transfer_to[, c('stop_id', 'UFI','stop_name', 'start_UFI') := NULL]

      # Change 2: setorder + unique instead of .SD[which.min()]
      setorder(stops_to_transfer_to, destination_stop_id, walking_time)
      stops_to_transfer_to <- unique(stops_to_transfer_to, by = 'destination_stop_id')
      stops_to_transfer_to[, time_left_after_walking := 46 -
                             walking_time]


      #trip id of departures from stops you can walk to
      departures_from_transfer_stops <- departures_stop_dt[stops_to_transfer_to$destination_stop_id]

      #join back with walking distances
      departures_from_transfer_stops <- departures_from_transfer_stops[stops_to_transfer_to, on = .(stop_id = destination_stop_id)]

      #clean up and calculate time left after walking there
      departures_from_transfer_stops[, time_left_after_walking := 46 - walking_time]
      departures_from_transfer_stops[, route_id := NULL]


      #changed sign
      #want more time leftover after waiting there than time before the service comes
      departures_from_transfer_stops <- departures_from_transfer_stops[time_left_after_walking >= minutes_until_time_limit]


      # Change 3: setorder + unique instead of .SD[which.max()]
      setorder(departures_from_transfer_stops, trip_id, -time_left_after_walking)
      departures_from_transfer_stops <- unique(departures_from_transfer_stops, by = 'trip_id')

      #dont need to recalculate min stop sequences, its literally already present in stop_to_departures
      #now you find all the places you can get to
      transfer_connections <- departures_trip_dt[departures_from_transfer_stops, on = .(trip_id)]
      transfer_connections[, route_id := NULL]
      #some renaming
      transfer_connections[,alighting_stop_name := stop_id_to_name[stop_id]]
      transfer_connections[,boarding_stop_name := stop_id_to_name[i.stop_id]]

      transfer_connections[, `:=`(min_stop_seq = i.stop_sequence, mins_left_at_dep_time = i.minutes_until_time_limit)][,`:=`(i.stop_id = NULL, i.minutes_until_time_limit = NULL, i.stop_sequence = NULL)]
      # Change 4: vectorised filter instead of grouped .SD filter
      transfer_connections <- transfer_connections[stop_sequence >= min_stop_seq]
      transfer_connections[,time_margin := time_left_after_walking - mins_left_at_dep_time]

      transfer_connections[, source_stop_id := current_stop_id]

    return(transfer_connections)
  }

  stop_ids <- as.character(unique_stops)
  if(doParallel) {
    results_list <- mclapply(stop_ids, process_stop, mc.cores = num_cores)
  } else {
    results_list <- lapply(stop_ids, process_stop)
  }
  place_registry <- rbindlist(results_list)

  return(place_registry)
}
