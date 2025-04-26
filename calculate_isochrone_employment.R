calculate_isochrone_employment <- function(isochrone_num) {

  sf_use_s2(F)

  isochrone_stops <- unique(isochrone_registry[[as.character(isochrone_num)]]$stop_id)

  buffered_isochrones = buffered_stops[which(buffered_stops$stop_id %in% isochrone_stops), ] %>%
    st_union() %>%
    st_as_sf()

  intersectioned <- st_intersection(buffered_isochrones, dzns_sf)

  intersectioned$intersection_area <- st_area(intersectioned$x)
  intersectioned$pc_intersection <- as.numeric(
    ((intersectioned$intersection_area/10^6) / intersectioned$AREASQK)
  )
  intersectioned$weighted_employment <- intersectioned$pc_intersection * intersectioned$ttl_mpl

  intersectioned_simple <- data.frame(
    isochrone_number = isochrone_num,
    total_accessible_employment = sum(intersectioned$ttl_mpl),
    total_weighted_acc_employment = sum(intersectioned$weighted_employment)
  )

  return(intersectioned_simple)

}
