test_ <- isochrone_registry[["1000"]]

invert_stop_id_list_dt <- function(large_list) {
  # Create a table to track which list elements contain which stop_ids
  result_dt <- data.table(list_idx = integer(0), stop_id = character(0))

  # Populate the tracking table
  for (i in seq_along(large_list)) {
    # Extract unique stop_ids from this dataframe
    ids <- unique(large_list[[i]]$stop_id)

    if (length(ids) > 0) {
      # Add entries to the tracking table
      result_dt <- rbindlist(list(
        result_dt,
        data.table(
          list_idx = rep(i, length(ids)),
          stop_id = ids
        )
      ))
    }
  }

  # Convert to list format
  inverted_list <- split(result_dt$list_idx, result_dt$stop_id)

  # If your original list has names, use those instead of indices
  if (!is.null(names(large_list))) {
    inverted_list <- lapply(inverted_list, function(indices) {
      names(large_list)[indices]
    })
  }

  return(inverted_list)
}

inv_list <- invert_stop_id_list_dt(isochrone_registry)

generate_isoarrival <- function(stop_id) {
  #synfaxgtfs:::.pkgenv$reachable_stops_dict -> reachable_stops_dict

  reachable_stops <- reachable_stops_dict[[as.character(stop_id)]]

  isoarrival <- unique(unlist(inv_list[reachable_stops]))


  isoarrival_sf <- buffered_stops %>%
    filter(stop_id %in% isoarrival) %>%
    st_union()

  # isoarrival_sf <- stops_sf %>%
  #   filter(stop_id %in% isoarrival)
  #
  leaflet(isoarrival_sf) %>%
    addProviderTiles('CartoDB.Positron') %>%
    addPolygons()
}






