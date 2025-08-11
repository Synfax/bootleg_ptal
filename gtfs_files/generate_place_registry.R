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
    message('starting to process in parallel')
    plan(multisession, workers = num_cores)
  } else {
    message('planning: sequential')
    plan('sequential')
  }

  message('plan established with ', nbrOfWorkers(), ' of workers')

  result <- future_map_dfr(as.character(unique_stops), function(current_stop_id) {

    stops_to_transfer_to <- walking_distances[.(current_stop_id)]
    stops_to_transfer_to <- stops_to_transfer_to[, .SD[which.min(walking_time)], by = destination_stop_id]
    stops_to_transfer_to[, time_left_after_walking := 46 -
                           walking_time]



    #trip id of departures from stops you can walk to
    departures_from_transfer_stops <- departures_stop_dt[stops_to_transfer_to$destination_stop_id]
    departures_from_transfer_stops <- departures_from_transfer_stops[stops_to_transfer_to, on = .(stop_id = destination_stop_id)]


    #clean up and calculate time left after walking there
    departures_from_transfer_stops[, time_left_after_walking := 46 - walking_time]
    departures_from_transfer_stops[, route_id := NULL]


    #changed sign
    #want more time leftover after waiting there than time before the service comes
    departures_from_transfer_stops <- departures_from_transfer_stops[time_left_after_walking >= minutes_until_time_limit]


    #hat it's doing: For each trip, it's finding the first/earliest stop where you can board (the stop with the most time remaining until the time limit).

    departures_from_transfer_stops <- departures_from_transfer_stops[, .SD[which.max(minutes_until_time_limit)], by = trip_id]

    #dont need to recalculate min stop sequences, its literally already present in stop_to_departures
    transfer_connections <- departures_trip_dt[departures_from_transfer_stops, on = .(trip_id)]
    transfer_connections[, route_id := NULL]
    transfer_connections[,alighting_stop_name := stop_id_to_name[stop_id]]
    transfer_connections[,boarding_stop_name := stop_id_to_name[i.stop_id]]

    transfer_connections[, `:=`(min_stop_seq = i.stop_sequence, mins_left_at_dep_time = i.minutes_until_time_limit)][,`:=`(i.stop_id = NULL, i.minutes_until_time_limit = NULL, i.stop_sequence = NULL)]
    transfer_connections <- transfer_connections[, .SD[stop_sequence >= min_stop_seq] , by = .(trip_id)]
    transfer_connections[,time_margin := time_left_after_walking - mins_left_at_dep_time]

    transfer_connections[, source_stop_id := current_stop_id]
    return(transfer_connections)
  }, .progress = TRUE)

  plan('sequential')

  return(result)
}
