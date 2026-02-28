library(synfaxgtfs)
library(tidyverse)
library(data.table)
library(sf)
library(lubridate)
library(tictoc)
library(s2)
library(leaflet)
library(future)
library(benchmarkme)
library(furrr)
library(profvis)
library(parallel)
library(doParallel)
library(osmdata)
library(qs)

#source files
source('gtfs_files/calculate_walking_distances.R')
source('gtfs_files/initialise_gtfs.R')
source('reset_storage.R')
source('gtfs_files/generate_place_registry.R')
source('gtfs_files/get_transit_ufi_dict.R')
source('gtfs_files/employment/employment_mb.R')
source('gtfs_files/link_walk_stops.R')
source('gtfs_files/mb_centroids_ufi.R')
source('gtfs_files/find_starting_indices.R')
source('gtfs_files/create_master_amenity_mb_dict.R')
source('gtfs_files/dijkstra/dijkstra_routing.R')
source('gtfs_files/final_mesh_block_result.R')
source('gtfs_files/link_results_to_mb.R')

#settings - core count and whether to enable parallel processing
  #enable parallel processing (required for sub ~4 hour processing time)
  doParallel = T

  #set the number of cores to work on in parallel
  #core count < RAM/5
  #e.g 32gb RAM, 6 cores would do, but you cant alloc
  num_cores <- min(as.numeric(get_ram()/(10^9)) / 6, parallel::detectCores() - 1)
  #num_cores = 12

#files required:
  # - mesh blocks
  # - destination zones
  # - dwelling_data
  # - employment csv

#set file paths
  # mb_path <- '~/Documents/r_projects/shapefiles/MB_2021_AUST_SHP_GDA2020/MB_2021_AUST_GDA2020.shp'
  # dz_path <- '~/Documents/r_projects/shapefiles/DZN_2021_AUST_GDA2020_SHP/DZN_2021_AUST_GDA2020.shp'
  # dd_path <- '~/Documents/r_projects/shapefiles/melbourne_dwelling_data.gpkg'
  # employment_csv_path <- 'sf_input/employment_dzn.csv'

#this script stores isochrone outputs in stop_isochrones, so we need to reset the files as to not mix two gtfs schedules together


reset_storage()

#set gtfs parameters
gtfs_parameters =  list(
  mode_numbers = 2:4,
  day = 'wednesday',
  city = 'melbourne'
)

mb_sf <<- read_sf('~/Documents/r_projects/shapefiles/MB_2021_AUST_SHP_GDA2020/MB_2021_AUST_GDA2020.shp') %>%
  filter(GCC_NAME21 == 'Greater Melbourne') %>%
  st_transform(7855)

transit_ufi_dict <- get_transit_ufi_dict()

walking_distances <- calculate_walking_distances()

master_mb_ufi <<- get_master_mb_ufi(mb_sf)
master_mb_ufi[, UFI := as.character(UFI)]

all_walk_raw <- rbindlist(lapply(list.files('walking_isochrones_sa2/', full.names = TRUE), fread))
all_walk_raw[, start_UFI := as.character(start_UFI)]
all_walk_raw[, UFI := as.character(UFI)]

walking_access_dict <<- link_walk_stops(all_walk_raw)

master_amenity_dt <<- create_master_amenity_mb_dict(mb_sf)

################

budgets <<- c(45, 30, 15)
max_time <<- max(budgets)

time_name <<- 'morning'
start_times = c("07:55:00", "08:00:00", "08:05:00")

start_time_results <- vector("list", 3)

for(i in 1:length(start_times)) {

  start_time_ <<- start_times[i]

  time_limit_ <<-  as.period(as.duration(hms(start_time_)) + dminutes(max_time))

  #preload hash tables and gtfs tables for use later
  initialise_gtfs(gtfs_parameters, start_time_, time_limit_)

  #get stops list
  gtfs_pre_stops <- unique_stops

  #get stops sf
  stops_sf <- stops %>%
    mutate(stop_id = as.character(stop_id)) %>%
    filter(stop_id %in% gtfs_pre_stops)


  place_registry <<- generate_place_registry(doParallel, num_cores)

  test <<- find_starting_indices(all_walk_raw)

  all_results <<- dijkstra_transit_routing(doParallel, num_cores)

  linked_back_to_mb <- link_results_to_mb(all_results, test)

  start_time_results[[i]] <- linked_back_to_mb

}




#package_final_sf(mb_sf)


