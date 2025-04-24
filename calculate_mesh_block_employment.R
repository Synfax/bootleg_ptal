calculate_mesh_block_employment <- function(MB_CODE21) {

  sf_use_s2(F)
  stops_accessible = mb_to_stops[MB_CODE21][[1]]

  buffered_isochrones = buffered_stops[which(buffered_stops$stop_id %in% stops_accessible), ] %>%
    st_union() %>%
    st_as_sf()

  # Intersection calculations
  intersectioned <- st_intersection(buffered_isochrones, dzns_sf)

  intersectioned$intersection_area <- st_area(intersectioned$x)
  intersectioned$pc_intersection <- as.numeric(
    ((intersectioned$intersection_area/10^6) / intersectioned$AREASQKM21)
  )
  intersectioned$weighted_employment <- intersectioned$pc_intersection * intersectioned$total_employment

  intersectioned_simple <- data.frame(
    MB_CODE21 = MB_CODE21,
    total_accessible_employment = sum(intersectioned$total_employment)
  )

  # Save results
  fwrite(intersectioned_simple,
          paste0('MB_accessibility/', MB_CODE21, '_job_access.csv'))

  gc()

  return(MB_CODE21)
}



