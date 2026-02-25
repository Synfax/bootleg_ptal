create_master_amenity_mb_dict <- function(mb_sf) {
  mb_sf <- mb_sf %>%
    select(MB_CODE21, MB_CAT21) %>%
    mutate(area = st_area(geometry))

  jobs = employment_mb(mb_sf) %>%
    st_drop_geometry() %>%
    as.data.frame() %>%
    select(-geometry)

  list.files('gtfs_files/amenities/', full.names = T) %>% map(.f = source)

  open_space <- link_open_space(mb_sf)

  supermarkets = link_supermarkets(mb_sf)

  vicmap_foi = manipulate_vicmap_foi(mb_sf)

  master_amenity_mb_dict = mb_sf %>%
    st_drop_geometry() %>%
    select(MB_CODE21) %>%
    left_join(jobs, by = 'MB_CODE21') %>%
    left_join(open_space, by = 'MB_CODE21') %>%
    left_join(supermarkets, by = 'MB_CODE21') %>%
    left_join(vicmap_foi, by = 'MB_CODE21') %>%
    mutate(across(!MB_CODE21, ~ replace_na(.,0)))

  master_amenity_dt = as.data.table(master_amenity_mb_dict)
  setkey(master_amenity_dt, MB_CODE21)

  return(master_amenity_dt)
}
