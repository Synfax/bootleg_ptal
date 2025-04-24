# Define the processing function
process_isochrone <- function(starting_stop, isochrone_params, full_env) {
  starting_stop <- as.character(starting_stop)

  #print(isochrone_params)

  # Generate isochrone
  isochrone_results <- synfaxgtfs::run_iteration_parallel(starting_stop, isochrone_params, full_env, restrict_initial_xfer = T)

  # Save slim results
  slim_results <- isochrone_results

  fwrite(slim_results, paste0('stop_isochrones/', starting_stop, '_isochrone.csv'))

  # Return ID for progress tracking
  return(starting_stop)
}


