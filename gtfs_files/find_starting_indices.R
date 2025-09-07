find_starting_indices <- function() {



  list.files('walking_isochrones_sa2/') %>%
        map_dfr(.f = function(file){
        fread(paste0('walking_isochrones_sa2/',file))
      }) -> all_walk

  #make large dt of all walking connections (ALL UFIs)
  all_walk[, UFI := as.character(UFI)]
  setkey(all_walk, UFI)
  setkey(transit_ufi_dict, nearest_UFI)

  all_walk = all_walk[transit_ufi_dict, on = c('UFI' = 'nearest_UFI'), nomatch = NULL]

  #begin test

  test <- master_mb_ufi[MB_CODE21 == 20460461000]
  #test[, UFI := as.character(UFI)]

  test_2 <- test[all_walk, on = c('UFI' = 'start_UFI'), nomatch = 0L]

  sources <- unique(source_pairs)
  test_3 <- test_2[sources, on = 'stop_id', nomatch = 0L]
  test_3[, mins_elapsed := max_time - time]

  test_4 <- test_3[mins_elapsed >= walking_time][, .SD[which.max(time)], by = 'stop_id']



  tr_road_infra <- read_sf('sf_input/tr_road_infrastructure/Order_08APF2/mga2020_55/esrishape/customised_delivery/MELBOURNE_WATER-0/VMTRANS/TR_ROAD_INFRASTRUCTURE.shp')
  tr_road_infra <- tr_road_infra %>%
    st_transform(7855) %>%
    select(UFI)

  leaflet() %>%
    addProviderTiles('CartoDB.Positron') %>%
    addCircleMarkers(data = (  tr_road_infra %>%
                                 filter(UFI == 60550137) %>%
                                 st_transform('wgs84'))) %>%
    addCircleMarkers(data = (  tr_road_infra %>%
                                 filter(UFI == 2288574) %>%
                                 st_transform('wgs84')), color = 'red')



}
