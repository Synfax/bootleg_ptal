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

  mb_sf <-

}
