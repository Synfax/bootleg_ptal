function(env, current_stop_id,r_arrival_time) {

  if(is.null(env[[as.character(current_stop_id)]]) || r_arrival_time < env[[as.character(current_stop_id)]]$arrival_time) {

    # Store a list with travel time and other information
    env[[as.character(current_stop_id)]] <- list(
      arrival_time = r_arrival_time
    )
  }

  return(env)

}
