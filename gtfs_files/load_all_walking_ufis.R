walking_files <- list.files('walking_isochrones_sa2/')

walking_files %>% map_dfr(.f = function(file){
  fread(paste0('walking_isochrones_sa2/',file))
}) -> all_walk

all_walk[, start_UFI := as.numeric(start_UFI)]


