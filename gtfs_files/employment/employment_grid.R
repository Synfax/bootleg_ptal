
assign_ufi_to_employment <- function() {


  tr_road_infra <- read_sf('sf_input/tr_road_infrastructure/Order_08APF2/mga2020_55/esrishape/customised_delivery/MELBOURNE_WATER-0/VMTRANS/TR_ROAD_INFRASTRUCTURE.shp')
  tr_road_infra <- tr_road_infra %>%
    st_transform(7855) %>%
    select(UFI)

  dzns_sf <- read_sf('sf_output/dzns_sf.shp') %>%
    st_transform(7855)

  #make on sf for all of melbourne so we can grid it
  greater_melbourne = dzns_sf %>%
    filter(AREASQK < 10) %>%
    st_union() %>%
    st_as_sf()



  #create grid cells
  sf_use_s2(F)
  melbourne_grids = st_make_grid(greater_melbourne, cellsize = 250, square = F)

  melbourne_grids = st_as_sf(melbourne_grids)

}

