final <- function() {

  all_res <- copy(all_results)
  setkey(all_res, stop_id)

  mb_sf <- readRDS('rdata_output/joined_buffered_mb_df.Rdata')

  1:nrow(mb_sf) %>% map(.f = function(index) {

    mb_code <- mb_sf[index,]$MB_CODE21
    stops <- unlist(mb_sf[index,]$stops_inside)

    employment_for_stops <- all_res[stops]
    return(data.table(
      MB_CODE21 = mb_code,
      mean_employment = mean(employment_for_stops$empl, na.rm = T),
      max_employment = max(employment_for_stops$empl, na.rm = T)
    ))
  }) %>% rbindlist() -> mb_sf_em

  mb_sf <- read_sf('~/Documents/r_projects/shapefiles/MB_2021_AUST_SHP_GDA2020/MB_2021_AUST_GDA2020.shp')

  mb_melbourne <- mb_sf %>%
    filter(GCC_NAME21 == 'Greater Melbourne')

  mb_melbourne %>%
    left_join(mb_sf_em, by = 'MB_CODE21') -> joined

  joined = joined[!is.na(max_employment)]

  write_sf(joined, 'sf_output/question_mark.gpkg', append = F)

}
