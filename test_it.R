

iter <- function(start_stop_id, isochrone_params, full_env, restrict_initial_xfer = T) {

  # Export package environment variables to local variables
  stop_id_to_name <- package_env$stop_id_to_name
  arrival_time_dict <- package_env$arrival_time_dict
  place_registry <- package_env$place_registry
  stops <- package_env$stops

  get_place <- function(stop_id, time) {
    place_registry[[as.character(stop_id)]][as.character(time)][[1]]
  }

  memoized_get_place <- memoise(get_place)

  result_env = new.env(hash = T)

  xfer_penalty_m <- as.numeric(as.duration(isochrone_params$xfer_penalty), 'minutes')

  current_stop_id = as.character(start_stop_id)
  current_station_name = stop_id_to_name[as.character(current_stop_id)]

  arrival_times <- (arrival_time_dict[current_stop_id][[1]])
  arrival_times = as.character(as.numeric(
    as.duration(isochrone_params$time_limit_ - arrival_times),
    'minutes'
  ))
  arrival_time <- (max(as.numeric(arrival_times)))
  full_duration <- (as.numeric(as.duration(
    isochrone_params$time_limit_ - hms(isochrone_params$start_time_)
  ), 'minutes'))
  correction_factor = full_duration - arrival_time

  #p1 <- place_registry[[current_stop_id]][as.character(arrival_time)][[1]]
  p1 <- memoized_get_place(current_stop_id, arrival_time)

  if (restrict_initial_xfer) {
    p1 <- p1 %>% group_by(trip_id) %>% filter(any(stop_id == current_stop_id)) %>% ungroup()
  }

  if (is.null(p1)) {
    message('P1 is Null. Likely R-Bus.')
  }

  for (i in 1:nrow(p1)) {
    process_p1_row(as.character(p1$stop_id[i]), p1$arrival_time[i] )
  }


  res_unwound <- as.list(result_env)
  #print(length(res_unwound))

  res <- data.frame(
    stop_id = names(res_unwound),
    arrival_time = sapply(res_unwound, function(x) ((x$arrival_time)))
  )

  return(res)


}

process_p1_row <- function(current_stop_id, r_arrival_time) {

  min_to_limit = (as.numeric(
    as.duration(isochrone_params$time_limit_ - r_arrival_time),
    'minutes'
  ))

  existing_results <- names(result_env)

  if (current_stop_id %in% existing_results) {
    if (min_to_limit <= result_env[[current_stop_id]]$arrival_time) {
      return()
    }
  }

  result_env = checkres(
    result_env,
    current_stop_id = current_stop_id,
    r_arrival_time = min_to_limit
  )

  min_to_limit = min_to_limit - xfer_penalty_m

  #p2 = place_registry[[as.character(current_stop_id)]][as.character(min_to_limit)][[1]]
  p2 <- memoized_get_place(current_stop_id, min_to_limit)

  if (is.null(p2))
    return()


  if (nrow(p2) > 0) {
    for (j in 1:nrow(p2)) {
        process_p2_row(as.character(p2$stop_id[j]), p2$arrival_time[j])
    }
  }
}

process_p2_row <- function(current_stop_id, r_arrival_time) {

  min_to_limit = (as.numeric(
    as.duration(isochrone_params$time_limit_ - r_arrival_time),
    'minutes'
  ))

  existing_results <- names(result_env)

  if (current_stop_id %in% existing_results) {
    if (min_to_limit <= result_env[[current_stop_id]]$arrival_time) {
      return()
    }
  }

  result_env = checkres(
    result_env,
    current_stop_id = current_stop_id,
    r_arrival_time = min_to_limit
  )

  min_to_limit = min_to_limit - xfer_penalty_m
  #p3 = place_registry[[as.character(current_stop_id)]][as.character(min_to_limit)][[1]]
  p3 = memoized_get_place(current_stop_id, min_to_limit)

  if (is.null(p3))
    return()

  if (nrow(p3) > 0) {
    for (k in 1:nrow(p3)) {

      process_p3_row(as.character(p3$stop_id[k]), p3$arrival_time[k])

    }
  }
}

process_p3_row <- function(current_stop_id, r_arrival_time) {

  min_to_limit = (as.numeric(
    as.duration(isochrone_params$time_limit_ - r_arrival_time),
    'minutes'
  ))

  existing_results <- names(result_env)

  if (current_stop_id %in% existing_results) {
    if (min_to_limit <= result_env[[current_stop_id]]$arrival_time) {
      return()
    }
  }

  result_env = checkres(
    result_env,
    current_stop_id = current_stop_id,
    r_arrival_time = min_to_limit
  )

}
