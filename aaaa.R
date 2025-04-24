

profvis({

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

  p1 <- place_registry[[current_stop_id]][as.character(arrival_time)][[1]]

  if(restrict_initial_xfer) {
    p1 <- p1 %>% group_by(trip_id) %>% filter(any(stop_id == current_stop_id)) %>% ungroup()
  }

  if(is.null(p1)) {
    message('P1 is Null. Likely R-Bus.')
  }

  testn = 0

  for(i in 1:nrow(p1)) {
    current_stop_id = as.character(p1$stop_id[i])
    r_arrival_time = p1$arrival_time[i]
    min_to_limit = (as.numeric(as.duration(isochrone_params$time_limit_ - r_arrival_time), 'minutes'))

    existing_results <- names(result_env)


    if(current_stop_id %in% existing_results) {
      if(min_to_limit <= result_env[[current_stop_id]]$arrival_time ) {
        next()
      }
    }

    result_env = checkres(result_env, current_stop_id = current_stop_id,
                          r_arrival_time = min_to_limit,
                          path = paste( as.character(p1$stop_id[i]),
                                        as.character(p1$arrival_time[i])))

    min_to_limit = min_to_limit - xfer_penalty_m


    p2 = place_registry[[as.character(current_stop_id)]][as.character(min_to_limit)][[1]]

    if(is.null(p2)) next()
    if(nrow(p2) > 0) {
      for(j in 1:nrow(p2)) {
        current_stop_id = as.character(p2$stop_id[j])
        min_to_limit = (as.numeric(as.duration(isochrone_params$time_limit_ - r_arrival_time), 'minutes'))

        existing_results <- names(result_env)

        if(current_stop_id %in% existing_results) {
          if(min_to_limit <= result_env[[current_stop_id]]$arrival_time ) {
            next()
          }
        }

        result_env = checkres(result_env, current_stop_id = current_stop_id,
                              r_arrival_time = min_to_limit,
                              path = paste( as.character(p2$stop_id[j]),
                                            as.character(p2$arrival_time[j])))

        min_to_limit = min_to_limit - xfer_penalty_m
        p3 = place_registry[[as.character(current_stop_id)]][as.character(min_to_limit)][[1]]

        if(is.null(p3)) next()

        if(nrow(p3) > 0) {
          for(k in 1:nrow(p3)) {

            testn = testn + 1

            current_stop_id = as.character(p3$stop_id[k])
            r_arrival_time = p3$arrival_time[k]
            min_to_limit = (as.numeric(as.duration(isochrone_params$time_limit_ - r_arrival_time), 'minutes'))

            existing_results <- names(result_env)

            if(current_stop_id %in% existing_results) {
              if(min_to_limit <= result_env[[current_stop_id]]$arrival_time ) {
                next()
              }
            }

            result_env = checkres(result_env, current_stop_id = current_stop_id,
                                  r_arrival_time = min_to_limit,
                                  path = paste( as.character(p3$stop_id[k]),
                                                as.character(p3$arrival_time[k])))


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

})


return(res)
