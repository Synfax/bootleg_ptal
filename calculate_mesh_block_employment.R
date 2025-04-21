calculate_mesh_block_employment <- function(MB_CODE21) {

  sf_use_s2(F)
  stops_accessible = mb_to_stops[MB_CODE21][[1]]

  #scope here to do slice_min instead and then do some weighting on job access
  combined_isochrone = unique(bind_rows(isochrone_registry[stops_accessible])$stop_id)

  isochrone_results = stops_sf[stops_sf$stop_id %in% combined_isochrone,]

  # Buffer and transform
  melbourne_utm <- "EPSG:32755"  # UTM Zone 55S
  buffered_isochrones <- isochrone_results %>%
    st_transform(crs = melbourne_utm) %>%
    st_buffer(dist = 450) %>%
    st_union() %>%
    st_transform(st_crs(isochrone_results)) %>%
    st_as_sf()

  buffered_isochrones = st_make_valid(buffered_isochrones)

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
  saveRDS(intersectioned_simple,
          paste0('MB_accessibility/', MB_CODE21, '_job_access.Rdata'))

  return(MB_CODE21)
}


calculate_isochrone_employment <- function(isochrone_index) {

  stops_in_iso <- isochrone_registry[1]$stop_id

}
