nearly_giving_up <- function(start_stop_id, isochrone_params, package_env, restrict_initial_xfer = T) {

  #profvis({

  visited_stops <- new.env(hash = TRUE)

  # Export package environment variables to local variables
  stop_id_to_name <- package_env$stop_id_to_name
  arrival_time_dict <- package_env$arrival_time_dict
  place_registry <- package_env$place_registry
  stops <- package_env$stops

  result_env = new.env(hash = T)

  xfer_penalty_m <- as.numeric(as.duration(isochrone_params$xfer_penalty), 'minutes')

  current_stop_id = as.character(start_stop_id)
  current_station_name = stop_id_to_name[as.character(current_stop_id)]

  arrival_times <- (arrival_time_dict[current_stop_id][[1]])
  arrival_times = as.character(as.numeric(as.duration(isochrone_params$time_limit_ - arrival_times), 'minutes'))
  arrival_time <- (max(as.numeric(arrival_times)))
  full_duration <- (as.numeric(as.duration(isochrone_params$time_limit_ - hms(isochrone_params$start_time_)), 'minutes'))
  correction_factor = full_duration - arrival_time

  #p1 <- memoized_get_place(current_stop_id, arrival_time)
  p1 <- place_registry[[(current_stop_id)]][as.character(arrival_time)][[1]]
  p1$stop_id = as.character(p1$stop_id)

  if(restrict_initial_xfer) {
    p1 <- p1 %>%
      group_by(trip_id) %>%
      filter(any(stop_id == current_stop_id)) %>%
      ungroup()
  }

  p1$min_to_limit <- as.numeric(as.duration(isochrone_params$time_limit_ - p1$arrival_time), 'minutes')

  for(i in 1:nrow(p1)) {

    current_stop_id = p1$stop_id[i]
    min_to_limit = p1$min_to_limit[i]
    #r_arrival_time = p1$arrival_time[i]

    #min_to_limit = (as.numeric(as.duration(isochrone_params$time_limit_ - r_arrival_time), 'minutes'))
    #existing_results <- names(result_env)

    if(exists(current_stop_id, envir = visited_stops, inherits = FALSE)) {
      if(min_to_limit <= result_env[[current_stop_id]]$arrival_time) {
        next()
      }
    }
    # Direct assignment
    result_env[[current_stop_id]] <- list(
      arrival_time = min_to_limit
    )
    visited_stops[[current_stop_id]] <- TRUE

    min_to_limit = min_to_limit - xfer_penalty_m
    if(min_to_limit <= 1) next()

    p2 = place_registry[[(current_stop_id)]][as.character(min_to_limit)][[1]]
    #p2 <- memoized_get_place(current_stop_id, min_to_limit)
    p2$stop_id = as.character(p2$stop_id)
    p2$min_to_limit <- as.numeric(as.duration(isochrone_params$time_limit_ - p2$arrival_time), 'minutes')

    if(is.null(p2)) next()
    if(nrow(p2) > 0) {
      for(j in 1:nrow(p2)) {
        current_stop_id = p2$stop_id[j]
        min_to_limit = p2$min_to_limit[j]
        #r_arrival_time = p2$arrival_time[j]

        #min_to_limit = (as.numeric(as.duration(isochrone_params$time_limit_ - r_arrival_time), 'minutes'))

        if(exists(current_stop_id, envir = visited_stops, inherits = FALSE)) {
          if(min_to_limit <= result_env[[current_stop_id]]$arrival_time) {
            next()
          }
        }

        # Direct assignment
        result_env[[current_stop_id]] <- list(
          arrival_time = min_to_limit
        )
        visited_stops[[current_stop_id]] <- TRUE

        min_to_limit = min_to_limit - xfer_penalty_m

        if(min_to_limit <= 1) next()

        #p3 <- memoized_get_place(current_stop_id, min_to_limit)
        p3 = place_registry[[(current_stop_id)]][as.character(min_to_limit)][[1]]
        p3$stop_id = as.character(p3$stop_id)

        if(is.null(p3)) {
          next()
        }

        p3$min_to_limit =  as.numeric(as.duration(isochrone_params$time_limit_ - p3$arrival_time), 'minutes')

        if(nrow(p3) > 0) {
          for(k in 1:nrow(p3)) {

            current_stop_id = p3$stop_id[k]
            min_to_limit = p3$min_to_limit[k]

            if(exists(current_stop_id, envir = visited_stops, inherits = FALSE)) {
              if(min_to_limit <= result_env[[current_stop_id]]$arrival_time) {
                next()
              }
            }
            # Direct assignment
            result_env[[current_stop_id]] <- list(
              arrival_time = min_to_limit
            )
            visited_stops[[current_stop_id]] <- TRUE

          }
        }
      }
    }
  }

  res_unwound <- as.list(result_env)
  #print(length(res_unwound))

  res <- data.frame(
    stop_id = names(res_unwound),
    arrival_time = sapply(res_unwound, function(x) ((x$arrival_time)))
  )

  #})

  return(res)
}
