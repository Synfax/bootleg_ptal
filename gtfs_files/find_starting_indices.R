find_starting_indices <- function(all_walk_raw) {

  #sources <- unique(place_registry[, .(boarding_stop_name, mins_left_at_dep_time)])

  setkey(transit_ufi_dict, nearest_UFI)

  all_walk <- all_walk_raw[transit_ufi_dict, on = c('UFI' = 'nearest_UFI'), nomatch = NULL]
  all_walk = all_walk[!str_detect(stop_id, 'vic')]
  #begin test

  #test[, UFI := as.character(UFI)]

  sources <- unique(place_registry[, .(stop_id, time = minutes_until_time_limit)])

  test <- master_mb_ufi[all_walk, on = c('UFI' = 'start_UFI'), nomatch = 0L]

  #test <- test[, .SD[which.min(walking_time)], by = 'MB_CODE21', allow.cartesian = T]

  test <- test[sources, on = 'stop_id', nomatch = 0L, allow.cartesian = T][walking_time + time <= max_time][, .SD[which.max(time)], by = 'MB_CODE21']

  #start_points <- unique(test[,.(stop_id, time)])

  return(test)

  #alt version




  #i think this works but i have yet to solve the double back problem
  #essentially im simulating that everyone in a MB lives at the UFI closest to the centre of the mesh block
  #and to 'start' their journey they walk to the closest PT stop
  # but then from that closest PT stop they could walk to other PT stops if they want.
  #but this allows them to 'double back' along the way they originally walked from the centroid UFI.
  # this is to square how the routing algorthm stores start points as only PT stops
  #hopefully because mesh blocks are tiny there isn't too much doubling back, and in places without much PT there probably isn't much double back

  #brunswick station (14315) isn';t the closest
  #TODO: check if the ufi matching of pt stops to ufis is messing up stations



  #
  # test_3 <- test_2[sources, on = 'stop_id', nomatch = 0L]
  # test_3[, mins_elapsed := max_time - time]
  # #test_3[, time := NULL]
  #
  # test_4 <- test_3[mins_elapsed >= walking_time][, .SD[which.min(mins_elapsed)], by = 'stop_id']
  #


  # tr_road_infra <- read_sf('sf_input/tr_road_infrastructure/Order_08APF2/mga2020_55/esrishape/customised_delivery/MELBOURNE_WATER-0/VMTRANS/TR_ROAD_INFRASTRUCTURE.shp')
  # tr_road_infra <- tr_road_infra %>%
  #   st_transform(7855) %>%
  #   select(UFI)
  #
  # leaflet() %>%
  #   addProviderTiles('CartoDB.Positron') %>%
  #   addCircleMarkers(data = (  tr_road_infra %>%
  #                                filter(UFI == 2289021) %>%
  #                                st_transform('wgs84'))) %>%
  #   addCircleMarkers(data = (  tr_road_infra %>%
  #                                filter(UFI == 2288574) %>%
  #                                st_transform('wgs84')), color = 'red')



}
