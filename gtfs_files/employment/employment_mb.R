employment_mb <- function(mb_sf) {

  mb_sf <- mb_sf %>%
    select(MB_CODE21)

  dzns_sf <- read_sf('sf_output/dzns_sf.shp') %>%
    st_transform(7855) %>%
    select(DZN_COD, ttl_mpl, AREASQK) %>%
    rename(total_employment = ttl_mpl)

  overlaps <- st_intersection(mb_sf, dzns_sf) %>%
    mutate(overlap_area = units::drop_units(units::set_units(st_area(geometry), 'km^2')) ) %>%
    filter(overlap_area > 0.001) %>%
    mutate(proportion = overlap_area / AREASQK, jobs = proportion * total_employment)


  mb_employment_dict <- overlaps %>%
    select(MB_CODE21, jobs) %>%
    as.data.table() %>%
    setkey(MB_CODE21)

  return(mb_employment_dict)

  #debug
  #overlaps %>%
    # st_drop_geometry() %>%
    # group_by(DZN_COD) %>%
    # summarise(true_area = mean(AREASQK), sum_area = sum(overlap_area), prop_test = sum(proportion))


}
