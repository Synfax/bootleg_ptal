manipulate_vicmap_foi <- function(mb_sf) {

  vicmap_foi_point <- read_sf('sf_input/Order_N13IFM/mga2020_55/esrishape/customised_delivery/MELBOURNE_WATER-0/VMFEAT/FOI_INDEX_CENTROID.shp')

  types_to_keep = c('health facility', 'sport facility', 'care facility', 'education centre', 'hospital')

  vicmap_foi_point = vicmap_foi_point %>%
    st_transform(7855) %>%
    filter(FTYPE %chin% types_to_keep, FEATSUBTYP != 'education complex')

  vicmap_foi_point = vicmap_foi_point %>%
    mutate(
      FTYPE = ifelse(FEATSUBTYP %in% c('child care', 'aged care', 'tertiary institution'), FEATSUBTYP, FTYPE),
    ) %>%
    filter(!(FTYPE %chin% c('aged care', 'care facility'))) #temp filters to review later

  vicmap_joined <- st_intersection(mb_sf, vicmap_foi_point) %>%
    select(MB_CODE21, FTYPE) %>%
    st_drop_geometry()

  vicmap_wide <- vicmap_joined %>%
    count(MB_CODE21, FTYPE) %>%
    pivot_wider(names_from = FTYPE,
                values_from = n,
                names_prefix = "n_",
                values_fill = 0)

  return(vicmap_wide)
}
