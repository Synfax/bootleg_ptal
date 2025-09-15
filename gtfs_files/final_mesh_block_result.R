final_mb_result <- function() {



  all_res <- copy(all_results)
  all_res[, name := names(vertex_to_index[start_vertex_index]) ]

  #from find_starting_indices
  starting_index_df <- test[,.(MB_CODE21, stop_id, walking_time)]
  starting_index_df[, time := max_time - walking_time]
  starting_index_df[, name := paste0(stop_id, '_', time)]

  final_result <- starting_index_df[all_res, on = c('name'), nomatch = NULL]

  mb_sf <- read_sf('~/Documents/r_projects/shapefiles/MB_2021_AUST_SHP_GDA2020/MB_2021_AUST_GDA2020.shp') %>%
    filter(GCC_NAME21 == 'Greater Melbourne') %>%
    st_transform(7855)

  final_result <- final_result %>%
    left_join(mb_sf, by = 'MB_CODE21')

  write_sf(final_result, 'sf_output/final_result.gpkg')


  mb_test <- mb_sf %>%
    filter(MB_CODE21 %in% mesh_blocks)

  write_sf(mb_test, 'sf_output/mb_test.gpkg')

  #current attempt ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#   #find highest time per vertex
#   starting_vertices %>%
#     select(stop_id, time_remaining) %>%
#     as.data.table() -> starting_times_dict
#
#   all_res <- copy(all_results)
#
#   all_res = all_res %>%
#     select(empl, stop_id) %>%
#     st_drop_geometry()
#
#   final_result <- copy(master_mb_ufi)
#   final_result[, UFI := as.character(UFI)]
#   setkey(final_result, 'UFI')
#
#
#   list.files('walking_isochrones_sa2/') %>%
#     map_dfr(.f = function(file){
#     fread(paste0('walking_isochrones_sa2/',file))
#   }) -> all_walk
#
#   #make large dt of all walking connections (ALL UFIs)
#   all_walk[, UFI := as.character(UFI)]
#   setkey(all_walk, UFI)
#   setkey(transit_ufi_dict, nearest_UFI)
#   all_walk = all_walk[transit_ufi_dict, on = c('UFI' = 'nearest_UFI'), nomatch = NULL]
#
#   all_walk[, start_UFI := as.character(start_UFI)]
#   setkey(all_walk, start_UFI)
#
#   test <- (final_result)[all_walk, on = c('UFI' = 'start_UFI'), nomatch = NULL]
#
#   walking_adjustment = 5
#   test = test[starting_times_dict, on = 'stop_id', nomatch = 0L][(walking_time - walking_adjustment) <= (46-time_remaining)]
#
#   test = test[all_res, on = 'stop_id', nomatch = 0L]
#
#   test[, .SD[which.max(empl)], by = 'MB_CODE21'] -> final_results
#
#   final_results %>%
#     as.data.frame() %>%
#     left_join(mb_sf, by = 'MB_CODE21') %>%
#     st_set_geometry('geometry') -> final_sf
#
#   st_write(final_sf, 'sf_output/final_sf.gpkg', append = F)

  #prior attempt ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # all_res <- copy(all_results)
  # setkey(all_res, stop_id)
  #
  # mb_sf <- readRDS('rdata_output/joined_buffered_mb_df.Rdata')
  #
  # 1:nrow(mb_sf) %>% map(.f = function(index) {
  #
  #   mb_code <- mb_sf[index,]$MB_CODE21
  #   stops <- unlist(mb_sf[index,]$stops_inside)
  #
  #   employment_for_stops <- all_res[stops]
  #   return(data.table(
  #     MB_CODE21 = mb_code,
  #     mean_employment = mean(employment_for_stops$empl, na.rm = T),
  #     max_employment = max(employment_for_stops$empl, na.rm = T)
  #   ))
  # }) %>% rbindlist() -> mb_sf_em
  #
  # mb_sf <- read_sf('~/Documents/r_projects/shapefiles/MB_2021_AUST_SHP_GDA2020/MB_2021_AUST_GDA2020.shp')
  #
  # mb_melbourne <- mb_sf %>%
  #   filter(GCC_NAME21 == 'Greater Melbourne')
  #
  # mb_melbourne %>%
  #   left_join(mb_sf_em, by = 'MB_CODE21') -> joined
  #
  # joined = joined %>%
  #   filter(!is.na(max_employment))
  #
  # write_sf(joined, 'sf_output/question_mark.gpkg', append = F)

}
