calculate_mesh_block_employment <- function(MB_CODE21) {

  employment_simple <- data.frame(MB_CODE21 = MB_CODE21,
                                  mean_e = 0,
                                  med_e = 0,
                                  max_e = 0
  )

  stops_accessible = mb_to_stops[MB_CODE21][[1]]

  if(any(is.na(stops_accessible))) {
    return(employment_simple)
  }

  employment_df <- do.call(rbind, isochrone_employment_registry[stops_accessible])

  if(is.null(employment_df)) {
    return(employment_simple)
  }


  employment_simple <- data.frame(MB_CODE21 = MB_CODE21,
                                  mean_e = mean(employment_df$total_weighted_acc_employment, na.rm = T),
                                  med_e = median(employment_df$total_weighted_acc_employment, na.rm = T),
                                  max_e = max(employment_df$total_weighted_acc_employment, na.rm = T)
                                  )

  return(employment_simple)

}



