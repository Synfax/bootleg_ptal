link_results_to_mb <- function(all_results, test) {

  map(1:length(budgets), .f = function(i) {

    all_res <- all_results[[i]]
    all_res[, name := names(vertex_to_index[start_vertex_index]) ]

    #from find_starting_indices
    starting_index_df <- test[,.(MB_CODE21, stop_id, walking_time)]
    starting_index_df[, time := max_time - walking_time]
    starting_index_df[, name := paste0(stop_id, '_', time)]

    final_result_dt <- starting_index_df[all_res, on = c('name'), nomatch = NULL]
    colnames(final_result_dt) <- janitor::make_clean_names(colnames(final_result_dt))

    return(final_result_dt)

  })


}
