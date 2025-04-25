# Define the processing function
process_isochrone <- function(starting_stop, isochrone_params, full_env) {
  starting_stop <- as.character(starting_stop)

  isochrone_results <- nearly_giving_up(starting_stop, isochrone_params, full_env, restrict_initial_xfer = T)

  slim_results <- isochrone_results

  fwrite(slim_results, paste0('stop_isochrones/', starting_stop, '_isochrone.csv'))

  #toc()

  # Return ID for progress tracking
  return(starting_stop)
}


