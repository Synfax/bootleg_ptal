

current_stop_id = as.character(start_stop_id)

current_station_name = synfaxgtfs:::.pkgenv$stop_id_to_name[as.character(current_stop_id)]

arrival_times <- (synfaxgtfs:::.pkgenv$arrival_time_dict[current_stop_id][[1]])
arrival_times = as.character(as.numeric(as.duration(isochrone_params$time_limit_ - arrival_times), 'minutes'))

arrival_time <- (max(as.numeric(arrival_times)))
full_duration <- (as.numeric(as.duration(isochrone_params$time_limit_ - hms(isochrone_params$start_time_)), 'minutes'))
correction_factor = full_duration - arrival_time

.pkgenv$correction_factor = correction_factor

p1 <- synfaxgtfs:::.pkgenv$place_registry[[current_stop_id]][as.character(arrival_time)][[1]]
p1$stop_name = stop_id_to_name[as.character(p1$stop_id)]
