initialise_gtfs <- function(gtfs_parameters, isochrone_params) {

  gtfs_prefilter <<- synfaxgtfs::read_gtfs(city = gtfs_parameters$city,
                                          mode_numbers = gtfs_parameters$mode_numbers,
                                          day = gtfs_parameters$day) %>%
    mutate(arrival_time = hms(arrival_time), departure_time = hms(departure_time)) %>%
    filter(departure_time < isochrone_params$time_limit_,
           departure_time > hms(isochrone_params$start_time_))


  #initalise a sf of all stops
  stops <<- synfaxgtfs::get_stops(mode_numbers = gtfs_parameters$mode_numbers,city = 'melbourne')

  #initialise a df of all stops for faster lookup
  # stops_df <- stops %>%
  #   st_drop_geometry()

  unique_stops = unique(gtfs_prefilter$stop_id)

  #probably need to set this as a dt at some point
  stop_id_to_name <<- setNames(stops$stop_name, stops$stop_id)

}





