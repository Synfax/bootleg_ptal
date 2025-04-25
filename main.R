library(synfaxgtfs)
library(tidyverse)
library(data.table)
library(sf)
library(lubridate)
library(tictoc)
library(s2)
library(leaflet)
library(future)
library(furrr)


source('process_isochrone.R')
source('nearly_giving_uyp.R')
source('calculate_mesh_block_employment.R')

#synfax gtfs

gtfs_parameters =  list(mode_numbers = unname(unlist(synfaxgtfs::get_settings('mode_numbers'))),
                        day =  'tuesday',
                        city = unname(unlist(synfaxgtfs::get_settings('city'))))

isochrone_params = list(start_time_ = synfaxgtfs::get_settings('start_time_')[[1]][1],
                        time_limit_ = synfaxgtfs::get_settings('time_limit_')[[1]][1],
                        xfer_penalty_ = synfaxgtfs::get_settings('xfer_penalty_')[[1]][1] )

synfaxgtfs:::.preload_isochrone_data(gtfs_parameters, isochrone_params)

unique_stops = (unique(synfaxgtfs:::.pkgenv$gtfs_prefilter$stop_id))

arrival_time_dict = synfaxgtfs:::.pkgenv$arrival_time_dict

#.pkgenv = synfaxgtfs:::.pkgenv



#synfaxgtfs::place_registry_2(unique_stops, isochrone_params, arrival_time_dict)

#synfaxgtfs:::.pkgenv$place_registry -> a

#get stops list
gtfs_pre_stops <- as.character(synfaxgtfs::get_stops_in_gtfs_pre())
#get stops sf
stops_sf <- synfaxgtfs::get_stops_sf() %>%
  mutate(stop_id = as.character(stop_id)) %>%
  filter(stop_id %in% gtfs_pre_stops)

#buffered_stops

melbourne_utm <- "EPSG:32755"
buffered_stops <- stops_sf %>%
  st_transform(crs = melbourne_utm) %>%
  st_buffer(dist = 450) %>%
  st_transform(st_crs(stops_sf)) %>%
  st_as_sf()

buffered_stops = st_make_valid(buffered_stops)

#load dwelling_data

if(!(file.exists('rdata_output/dwelling_sa1s.Rdata') & file.exists('rdata_output/dwelling_sa2s.Rdata'))) {
  dwelling_data = st_read('~/Documents/r_projects/shapefiles/melbourne_dwelling_data.gpkg')
  dwelling_sa1s = unique(dwelling_data$sa1_code_2021)
  dwelling_sa2s = unique(dwelling_data$sa2_code_2021)

  saveRDS(dwelling_sa1s, 'rdata_output/dwelling_sa1s.Rdata')
  saveRDS(dwelling_sa2s, 'rdata_output/dwelling_sa2s.Rdata')
} else {
  dwelling_sa1s <- readRDS('rdata_output/dwelling_sa1s.Rdata')
  dwelling_sa2s <- readRDS('rdata_output/dwelling_sa2s.Rdata')
}

if(file.exists('sf_output/dzns_sf.shp')) {
  dzns_sf <- read_sf('sf_output/dzns_sf.shp')
} else {
  #employment

  #load dzn geographies
  dzns_sf <- read_sf('~/Documents/r_projects/shapefiles/DZN_2021_AUST_GDA2020_SHP/DZN_2021_AUST_GDA2020.shp') %>%
    filter(SA2_CODE21 %in% dwelling_sa2s)


  #load csv
  employment_dzn = read.csv('employment_dzn.csv', skip = 10) %>%
    rename(DZN_CODE21 = "X1.digit.level.OCCP.Occupation", total_employment = 'Total') %>%
    select(DZN_CODE21, total_employment) %>%
    mutate(DZN_CODE21 = as.character(DZN_CODE21))

  dzns_sf = dzns_sf %>%
    left_join(employment_dzn, by = 'DZN_CODE21') %>%
    st_transform('wgs84') %>%
    select(DZN_CODE21, AREASQKM21, total_employment)

  write_sf(dzns_sf, 'sf_output/dzns_sf.shp')
}

#load mesh block geometries
mb_sf <- read_sf('~/Documents/r_projects/shapefiles/MB_2021_AUST_SHP_GDA2020/MB_2021_AUST_GDA2020.shp') %>%
  filter(GCC_NAME21 == "Greater Melbourne", SA1_CODE21 %in% dwelling_sa1s )

if(!file.exists('rdata_output/joined_buffered_mb_df.Rdata')) {

  #buffer mesh blocks
  buffered <- mb_sf %>%
    s2::as_s2_geography() %>%
    s2_buffer_cells(distance = 450, max_cells = 50)

  #re-integrate with  stop_id
  buffered_mb_sf <- st_sf(MB_CODE21 =  mb_sf$MB_CODE21, geometry = st_as_sfc(buffered))

  #join to stops
  joined_buffered_mb_sf <- buffered_mb_sf %>%
    st_join(stops_sf) %>% select(c(MB_CODE21, stop_id, geometry))

  #convert to df
  joined_buffered_mb_df = joined_buffered_mb_sf %>%
    st_drop_geometry() %>%
    group_by(MB_CODE21) %>%
    summarise(stops_inside = list(stop_id))

  saveRDS(joined_buffered_mb_df, 'rdata_output/joined_buffered_mb_df.Rdata')

} else {
  joined_buffered_mb_df <- readRDS('rdata_output/joined_buffered_mb_df.Rdata')
}

# ugz <- read_sf('Order_1IR9YA/ll_gda2020/esrishape/whole_of_dataset/victoria/VMPLAN/PLAN_UGB.shp') %>%
#   st_union() %>%
#   st_as_sf() %>%
#   st_make_valid()

#convert to hash-table
#this says, for any given MB, which stops can I access?
mb_to_stops = setNames(joined_buffered_mb_df$stops_inside, joined_buffered_mb_df$MB_CODE21)

message('Begining actual analysis')

#begin actual analysis

#plan(multicore, workers = parallel::detectCores() - 1)

xz <- load_place_registry_parallel(unique_stops, isochrone_params, arrival_time_dict)
rm(xz)
gc()

plan('default')

message('Place reg loaded')


full_env <- synfaxgtfs:::.pkgenv

# {
#
#   results <- map(
#     gtfs_pre_stops,
#     function(stop_id) {
#       # Explicitly pass both parameters to process_stop
#       process_isochrone(starting_stop = stop_id, isochrone_params = isochrone_params, full_env)
#     },
#     .progress = TRUE
#   )
#
# }



plan(multisession, workers = parallel::detectCores() - 1)

#set options to 5gb each.
options(future.globals.maxSize = 5 * 1024^3)

#get list of stops that are yet to be saved. Only use in case of an interrupted run on the same GTFS day.
# stops_completed = list.files('stop_isochrones') %>% str_replace('_isochrone.csv', '')
# stops_remaining = setdiff(gtfs_pre_stops, stops_completed)
# stops_no_rbus = full_env$stops_no_rbus$stop_id
# stops_remaining = intersect(stops_no_rbus, stops_remaining)

# Then use future_map with explicit passing of parameters
results <- future_map(
  gtfs_pre_stops,
  function(stop_id) {
    # Explicitly pass both parameters to process_stop
    process_isochrone(starting_stop = stop_id, isochrone_params = isochrone_params, full_env)
  },
  .options = furrr_options(seed = TRUE),
  .progress = TRUE
)
plan('default')
gc()

message('Isochrones done')

#for each isochrone file, place it into a named list.
#this allows for quick read times in 'calculate_mesh_block_employment'
isochrone_registry <- list.files('stop_isochrones')  %>% map(function(path) {
  fread(paste0('stop_isochrones/', path))[,1:2]
}, .progress = T) %>% setNames( (list.files('stop_isochrones') %>% str_replace('_isochrone.csv', '')) )


#set up a parallel plan and run calculate_mesh_block_employment for each MB.
plan(multisession, workers = parallel::detectCores() - 1)

results <- future_map(
  names(mb_to_stops),
  function(MB_CODE21) {
    calculate_mesh_block_employment(MB_CODE21)
  },
  .options = furrr_options(seed = TRUE),
  .progress = TRUE
)
plan('default')
gc()


message('forming employment stuff')


  mb_employment_df <- list.files('MB_accessibility') %>% map_dfr(function(path) {
    fread(paste0('MB_accessibility/', path))
  }, .progress = T)

  mb_sf_em <- mb_sf %>%
    left_join(mb_employment_df %>% mutate(MB_CODE21 = as.character(MB_CODE21), by = 'MB_CODE21'))

  write_sf(mb_sf_em, 'sf_output/mb_sf_em.shp', append = F)

  # mb_pal <- colorNumeric(palette = 'Reds', domain = mb_sf_em$total_accessible_employment)
  # leaflet(mb_sf_em) %>%
  #   addProviderTiles('CartoDB.Positron') %>%
  #   addPolygons(data = mb_sf_em,
  #               fillColor = ~mb_pal(mb_sf_em$total_accessible_employment),
  #               fillOpacity = 0.7,
  #               weight = 0.1,
  #               color = 'black',
  #               opacity = 0.2)




draw_isochrone <- function(xyz) {

  employment_pal = colorBin('Reds', bins = 10, domain = xyz$weighted_employment)

  map <- leaflet(xyz) %>%
    addProviderTiles('CartoDB.Positron') %>%
    addPolygons(fillColor = ~employment_pal(xyz$weighted_employment),
                weight = 0.4,
                color = 'black',
                stroke = T,
                fillOpacity = 1) %>%
    addLegend(position = 'bottomleft',
              pal = employment_pal,
              values = xyz$weighted_employment,
              title = "Jobs accessible within 45 mins") %>%
    addMeasure(primaryLengthUnit = 'metres')

  return(map)

}

mp <- function(x) {leaflet(x) %>% addProviderTiles('CartoDB.Positron') %>% addPolygons()}


