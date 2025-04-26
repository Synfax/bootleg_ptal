calculate_mesh_block_employment <- function(MB_CODE21) {
#
#     sf_use_s2(F)
#     stops_accessible = mb_to_stops[MB_CODE21][[1]]
#
#     #stops_reachable_within_time <- unique(do.call(rbind, isochrone_registry[stops_accessible])$stop_id)
#
#     # buffered_isochrones = buffered_stops[which(buffered_stops$stop_id %in% stops_reachable_within_time), ] %>%
#     #   st_union() %>%
#     #   st_as_sf()
#
#     # Intersection calculations
#     intersectioned <- st_intersection(buffered_isochrones, dzns_sf)
#
#     intersectioned$intersection_area <- st_area(intersectioned$x)
#     intersectioned$pc_intersection <- as.numeric(
#       ((intersectioned$intersection_area/10^6) / intersectioned$AREASQK)
#     )
#     intersectioned$weighted_employment <- intersectioned$pc_intersection * intersectioned$ttl_mpl
#
#     intersectioned_simple <- data.frame(
#       MB_CODE21 = MB_CODE21,
#       total_accessible_employment = sum(intersectioned$ttl_mpl),
#       total_weighted_acc_employment = sum(intersectioned$weighted_employment)
#     )
#
#   # Save results
#   fwrite(intersectioned_simple,
#           paste0('MB_accessibility/', MB_CODE21, '_job_access.csv'))


  #sf_use_s2(F)

  employment_simple <- data.frame(MB_CODE21 = MB_CODE21,
                                  mean_e = 0,
                                  med_e = 0
  )

  stops_accessible = mb_to_stops[MB_CODE21][[1]]

  if(any(is.na(stops_accessible))) {
    return(employment_simple)
  }

  employment_df <- do.call(rbind, isochrone_employment_registry[stops_accessible])

  if(is.null(employment_df)) {
    return(employment_simple)
  }


  employment_simple <- data.frame(MB_CODE21 = MB_CODE21,
                                  mean_e = mean(employment_df$total_weighted_acc_employment, na.rm = T),
                                  med_e = median(employment_df$total_weighted_acc_employment, na.rm = T)
                                  )

  return(employment_simple)

}



