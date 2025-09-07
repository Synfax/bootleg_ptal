link_walk_stops <- function(){

  #this is for the final stage of the isochrone calculation
  #from all the transit stops I can reach, where are the final walking UFIs I can get to.

  walking_files <- list.files('walking_isochrones_sa2/')

  walking_files %>% map_dfr(.f = function(file){
    fread(paste0('walking_isochrones_sa2/',file))
  }) -> all_walk

  #make large dt of all walking connections (ALL UFIs)
  all_walk[, start_UFI := as.character(start_UFI)]

  #left join with master_mb_ufi to only show rows where a UFI is a MB
  all_walk = all_walk[master_mb_ufi, on = 'UFI', nomatch = NULL]

  #filter all walk to only those that are equivalent to centroid of mesh blocks


  #copy the dictionary that connects transit ufis to stop_ids
  transit_copy <- copy(transit_ufi_dict)
  #remove parent rail stops
  transit_copy <- transit_copy[!str_detect(stop_id, 'vic')]

  transit_copy[, nearest_UFI := as.character(nearest_UFI)]


  #create mega dt
  #from each transit stop, which UFIs can I walk to.
  #explosive cartesian join
  walking_access_dict <- transit_copy[all_walk, on = c('nearest_UFI' = 'start_UFI'), allow.cartesian = T, nomatch = NULL]
  gc()


  setkey(walking_access_dict, stop_id)
  walking_access_dict[, geometry := NULL]

  return(walking_access_dict)
}
