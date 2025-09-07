get_master_mb_ufi <- function() {

  #this creates a dictionary which links mesh block MB_CODE21 to walking UFIs

  #it has a problem that it sometimes uses minor asf roads. trying new version

  #TODO: link properly
  mb_sf <- read_sf('~/Documents/r_projects/shapefiles/MB_2021_AUST_SHP_GDA2020/MB_2021_AUST_GDA2020.shp') %>%
    filter(GCC_NAME21 == 'Greater Melbourne')

  mb_centroids <- st_centroid(mb_sf) %>%
    select(MB_CODE21) %>%
    st_transform(7855)

  #TODO: link properly
  tr_road_infra <- read_sf('sf_input/tr_road_infrastructure/Order_08APF2/mga2020_55/esrishape/customised_delivery/MELBOURNE_WATER-0/VMTRANS/TR_ROAD_INFRASTRUCTURE.shp')
  tr_road_infra <- tr_road_infra %>%
    st_transform(7855) %>%
    select(UFI)

  st_nearest_feature(mb_centroids, tr_road_infra) -> nearest_index

  master_mb_ufi <- tr_road_infra[nearest_index,] %>%
    mutate(
      MB_CODE21 = mb_centroids$MB_CODE21
    ) %>%
    st_drop_geometry() %>%
    as.data.table()

  return(master_mb_ufi)

}

get_master_mb_ufi_big_roads <- function() {


  #it has a problem that it sometimes uses minor asf roads. trying new version

  #TODO: link properly
  mb_sf <- read_sf('~/Documents/r_projects/shapefiles/MB_2021_AUST_SHP_GDA2020/MB_2021_AUST_GDA2020.shp') %>%
    filter(GCC_NAME21 == 'Greater Melbourne')

  mb_centroids <- st_centroid(mb_sf) %>%
    select(MB_CODE21) %>%
    st_transform(7855)

  #TODO: link properly
  tr_road_infra <- read_sf('sf_input/tr_road_infrastructure/Order_08APF2/mga2020_55/esrishape/customised_delivery/MELBOURNE_WATER-0/VMTRANS/TR_ROAD_INFRASTRUCTURE.shp')
  tr_road_infra <- tr_road_infra %>%
    st_transform(7855) %>%
    select(UFI)

  #TODO: fix link
  tr_road <- read_sf('sf_input/tr_road/Order_I2F4YY/mga2020_55/esrishape/customised_delivery/MELBOURNE_WATER-0/VMTRANS/TR_ROAD_ALL.shp') %>%
    st_transform(7855)

  tr_road_clean

  # st_nearest_feature(mb_centroids, tr_road_infra) -> nearest_index
  #
  # master_mb_ufi <- tr_road_infra[nearest_index,] %>%
  #   mutate(
  #     MB_CODE21 = mb_centroids$MB_CODE21
  #   ) %>%
  #   st_drop_geometry() %>%
  #   as.data.table()
  #
  # return(master_mb_ufi)


}
