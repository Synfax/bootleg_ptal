package_final_sf <- function(mb_sf) {


  #lets do 45 min budget first

  for(b in 1:length(budgets)) {

    name = budgets[b]
    result_file_name = file_name = paste0('trimmed_results_',time_name,'_',name)
    score_file_name = file_name = paste0('trimmed_scores_',time_name,'_',name)

    jitter_list = list(start_time_results[[1]][[b]],
                       start_time_results[[2]][[b]],
                       start_time_results[[3]][[b]])

    stacked_dt <- rbindlist(jitter_list)


    averaged_dt <- stacked_dt[, lapply(.SD, mean),
                              by = mb_code21,
                              .SDcols = c('jobs', 'total_open_space_area', 'n_supermarkets', 'n_sport_facility', 'n_education_centre', 'n_child_care', 'n_tertiary_institution', 'n_hospital', 'n_health_facility')]

    mean_reach = jitter_list[[2]][, .(mb_code21, mesh_block_list, travel_times)]

    trimmed_results = as.data.frame(averaged_dt[mean_reach, on = 'mb_code21'])

    qs::qsave((trimmed_results), paste0('qs/trimmed_results/',result_file_name,'.qs'))

    trimmed_scores <- trimmed_results %>%
        select(!c(mesh_block_list, travel_times)) %>%
        mutate(jobs = round(jobs,2)) %>%
        mutate(across(!mb_code21, ~percent_rank(.x))) %>%
        mutate(total_score = rowSums(across(!mb_code21)), MB_CODE21 = mb_code21)

      qs::qsave(trimmed_scores, paste0('qs/trimmed_scores/',score_file_name,'.qs'))

      final_result <- trimmed_scores %>%
        select(!mb_code21) %>%
        left_join(mb_sf, by = 'MB_CODE21')

      write_sf(final_result, paste0('sf_output/result_sfs/', time_name, name, 'final_result.gpkg'), append = F)

  }



    # for(i in 1:length(all_results)) {
    #
    #   name = names(all_results)[i]
    #
    #
    #   #summary(final_result_dt[,.(total_open_space_area, n_supermarkets, n_sport_facility, n_education_centre, n_child_care, n_tertiary_institution, n_hospital, n_health_facility)])
    #
    #   trimmed_results = final_result_dt[,.(mb_code21, mesh_block_list,travel_times, jobs, total_open_space_area, n_supermarkets, n_sport_facility, n_education_centre, n_child_care, n_tertiary_institution, n_hospital, n_health_facility)] %>%
    #     as.data.frame()
    #
    #   file_name = paste0(str_replace_all(start_time_, ":",'-'),name)
    #
    #   qs::qsave(trimmed_results, paste0('qs/budgets/',file_name,'.qs'))

      # trimmed_scores <- trimmed_results %>%
      #   select(!c(mesh_block_list, travel_times)) %>%
      #   mutate(jobs = round(jobs,2)) %>%
      #   mutate(across(!mb_code21, ~percent_rank(.x))) %>%
      #   mutate(total_score = rowSums(across(!mb_code21)), MB_CODE21 = mb_code21)
      #
      # qs::qsave(trimmed_scores, 'qs/trimmed_scores.qs')

      # final_result <- trimmed_scores %>%
      #   select(!mb_code21) %>%
      #   left_join(mb_sf, by = 'MB_CODE21')
      #
      # write_sf(final_result, 'sf_output/final_result.gpkg', append = F)



    # }






  # mb_test <- mb_sf %>%
  #   filter(MB_CODE21 %in% mesh_blocks)
  #
  # write_sf(mb_test, 'sf_output/mb_test.gpkg')

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
