link_supermarkets <- function(mb_sf) {

  #load mb sf


  #cached to avoid repeated API calls
  cache_path <- 'rdata_output/osm_supermarkets.qs'
  if(file.exists(cache_path)) {
    supermarkets <- qs::qread(cache_path)
  } else {
    supermarkets <- opq(bbox = getbb("Melbourne, Victoria, Australia")) %>%
      add_osm_feature(key = "shop",
                      value = c("supermarket", "greengrocer", "grocery")) %>%
      osmdata_sf()
    qs::qsave(supermarkets, cache_path)
  }

  # Handle points vs polygons
  polygons <- supermarkets$osm_polygons
  points <- supermarkets$osm_points

  #where polygons have points within them - remove the polygon but only keep the centroid point.


  if (!is.null(points) && !is.null(polygons) && nrow(points) > 0 && nrow(polygons) > 0) {
    # Find which polygons contain points
    points_in_polygons <- st_within(points, polygons, sparse = FALSE)

    # Convert polygons that contain points to centroids
    polygons_with_points <- apply(points_in_polygons, 2, any)

    polygon_centroids <- polygons %>%
      filter(polygons_with_points) %>%
      st_centroid()

    # Keep polygons without points as-is (or convert to centroids too)
    standalone_polygons <- polygons %>%
      filter(!polygons_with_points)

    # Keep points that aren't inside any polygon
    standalone_points <- points %>%
      filter(!apply(points_in_polygons, 1, any))

    # Combine: centroids of duplicate polygons + standalone points + standalone polygons
    all_supermarkets <- bind_rows(polygon_centroids, standalone_points, standalone_polygons)
  } else {
    all_supermarkets <- bind_rows(points, polygons)
  }

  all_supermarkets = all_supermarkets %>%
    select(osm_id, name) %>%
    st_transform(7855) %>%
    mutate(geom_type = st_geometry_type(geometry))

  #link mesh blocks to specific supermarkets
  supermarket_mb_intersection <- st_intersection(mb_sf, all_supermarkets)

  #find which mesh blocks have supermarkets in them and how many
  link_supermarkets_mb <- supermarket_mb_intersection %>%
    st_drop_geometry() %>%
    group_by(MB_CODE21) %>%
    summarise(n_supermarkets = n())

  #checks
  # mb_list <- link_supermarkets_mb$MB_CODE21
  # mb_sf %>% filter(MB_CODE21 %in% mb_list) %>% mapgl::maplibre_view()

  return(link_supermarkets_mb)
}
