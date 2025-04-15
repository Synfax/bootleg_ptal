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


#synfax gtfs

gtfs_parameters =  list(mode_numbers = unname(unlist(synfaxgtfs::get_settings('mode_numbers'))),
                        day =  'monday',
                        city = unname(unlist(synfaxgtfs::get_settings('city'))))

isochrone_params = list(start_time_ = synfaxgtfs::get_settings('start_time_')[[1]][1],
                        time_limit_ = synfaxgtfs::get_settings('time_limit_')[[1]][1],
                        xfer_penalty_ = synfaxgtfs::get_settings('xfer_penalty_')[[1]][1] )

synfaxgtfs:::.preload_isochrone_data(gtfs_parameters, isochrone_params)

unique_stops = (unique(synfaxgtfs:::.pkgenv$gtfs_prefilter$stop_id))

arrival_time_dict = synfaxgtfs:::.pkgenv$arrival_time_dict

#.pkgenv = synfaxgtfs:::.pkgenv
xz <- load_place_registry_parallel(unique_stops, isochrone_params, arrival_time_dict)

plan('default')

#synfaxgtfs::place_registry_2(unique_stops, isochrone_params, arrival_time_dict)

#synfaxgtfs:::.pkgenv$place_registry -> a

#get stops list
gtfs_pre_stops <- as.character(synfaxgtfs::get_stops_in_gtfs_pre())
#get stops sf
stops_sf <- synfaxgtfs::get_stops_sf() %>%
  mutate(stop_id = as.character(stop_id)) %>%
  filter(stop_id %in% gtfs_pre_stops)


#load dwelling_data
dwelling_data = st_read('~/Documents/r_projects/shapefiles/melbourne_dwelling_data.gpkg')
dwelling_sa1s = unique(dwelling_data$sa1_code_2021)
dwelling_sa2s = unique(dwelling_data$sa2_code_2021)


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



#load mesh block geometries
mb_sf <- read_sf('~/Documents/r_projects/shapefiles/MB_2021_AUST_SHP_GDA2020/MB_2021_AUST_GDA2020.shp') %>%
  filter(GCC_NAME21 == "Greater Melbourne", SA1_CODE21 %in% dwelling_sa1s )

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

#convert to hash-table
#this says, for any given MB, which stops can I access?
mb_to_stops = setNames(joined_buffered_mb_df$stops_inside, joined_buffered_mb_df$MB_CODE21)


# gtfs_pre_stops %>% walk(function(starting_stop) {
#   starting_stop = as.character(starting_stop)
#
#   message(starting_stop)
#
#   isochrone_results <- synfaxgtfs::generate_isochrone(starting_stop)
#
#   slim_results = isochrone_results %>%
#     st_drop_geometry() %>%
#     select(stop_id, travel_time, path)
#
#   fwrite(slim_results, paste0('stop_isochrones/',as.character(starting_stop), '_isochrone.csv'))
#
#   melbourne_utm <- "EPSG:32755"  # UTM Zone 55S
#
#   buffered_isochrones <- isochrone_results %>%
#     st_transform(crs = melbourne_utm) %>%  # Project to Melbourne's UTM zone
#     st_buffer(dist = 450) %>%             # 450 meters in UTM
#     st_union() %>%
#     st_transform(st_crs(isochrone_results)) %>%  # Transform back
#     st_as_sf()
#
#   intersectioned <- st_intersection(buffered_isochrones, dzns_sf)
#
#   intersectioned$intersection_area = st_area(intersectioned$x)
#   intersectioned$pc_intersection = as.numeric(((intersectioned$intersection_area/10^6) / intersectioned$AREASQKM21))
#
#   intersectioned$weighted_employment = intersectioned$pc_intersection * intersectioned$total_employment
#
#   intersectioned_simple = data.frame(stop_id = starting_stop, total_accessible_employment = sum(intersectioned$total_employment))
#
#
#   saveRDS(intersectioned_simple, paste0('stop_job_accessibility_simple/', starting_stop, '_job_access.Rdata'))
#   #saveRDS(intersectioned, paste0('stop_job_accessibility/', starting_stop, '_job_access.Rdata'))
#
# }, .progress = T)


# Define the processing function
process_stop <- function(starting_stop, isochrone_params, full_env) {
  starting_stop <- as.character(starting_stop)

  #print(isochrone_params)

  # Generate isochrone
  isochrone_results <- synfaxgtfs::run_iteration_parallel(starting_stop, isochrone_params, full_env)

  # Save slim results
  slim_results <- isochrone_results %>%
    st_drop_geometry() %>%
    select(stop_id, travel_time, path)

  fwrite(slim_results, paste0('stop_isochrones/', starting_stop, '_isochrone.csv'))

  # Buffer and transform
  melbourne_utm <- "EPSG:32755"  # UTM Zone 55S
  buffered_isochrones <- isochrone_results %>%
    st_transform(crs = melbourne_utm) %>%
    st_buffer(dist = 450) %>%
    st_union() %>%
    st_transform(st_crs(isochrone_results)) %>%
    st_as_sf()

  buffered_isochrones = st_make_valid(buffered_isochrones)

  # Intersection calculations
  intersectioned <- st_intersection(buffered_isochrones, dzns_sf)
  intersectioned$intersection_area <- st_area(intersectioned$x)
  intersectioned$pc_intersection <- as.numeric(
    ((intersectioned$intersection_area/10^6) / intersectioned$AREASQKM21)
  )
  intersectioned$weighted_employment <- intersectioned$pc_intersection * intersectioned$total_employment

  intersectioned_simple <- data.frame(
    stop_id = starting_stop,
    total_accessible_employment = sum(intersectioned$total_employment)
  )

  # Save results
  saveRDS(intersectioned_simple,
          paste0('stop_job_accessibility_simple/', starting_stop, '_job_access.Rdata'))

  # Return ID for progress tracking
  return(starting_stop)
}

#.pkgenv <- synfaxgtfs:::.pkgenv

#synfaxgtfs::update_evertthing(temp)

plan(multisession, workers = parallel::detectCores() - 1)

# Make sure isochrone_params is well-defined before using it
print("Isochrone parameters:")
print(isochrone_params)

full_env <- synfaxgtfs:::.pkgenv

options(future.globals.maxSize = 5 * 1024^3)

stops_completed = list.files('stop_isochrones') %>% str_replace('_isochrone.csv', '')
stops_remaining = setdiff(gtfs_pre_stops, stops_completed)

# Then use future_map with explicit passing of parameters
results <- future_map(
  stops_remaining,
  function(stop_id) {
    # Explicitly pass both parameters to process_stop
    process_stop(starting_stop = stop_id, isochrone_params = isochrone_params, full_env)
  },
  .options = furrr_options(seed = TRUE),
  .progress = TRUE
)

plan('default')

stop_accessibility_lookup <- gtfs_pre_stops %>% map(function(stop) {

  file_path <- paste0('stop_job_accessibility_simple/',stop, '_job_access.Rdata' )
  does_file_exist <- file.exists(file_path)

  if(does_file_exist) {
    job_access <- readRDS(file_path)
  } else {
    job_access = NULL
  }

  return(job_access)

}) %>% setNames(gtfs_pre_stops)

mb_to_employment <- names(mb_to_stops) %>% map_dfr(function(MB_CODE21) {
  stops_in_mb = mb_to_stops[as.character(MB_CODE21)][[1]]

  stop_accessibility = bind_rows(stop_accessibility_lookup[stops_in_mb])$total_accessible_employment

  stop_accessibility = data.frame(MB_CODE21 = MB_CODE21, total_em = mean(stop_accessibility, na.rm = T))

  return(stop_accessibility)
}, .progress = T)

write_sf(mb_sf_em, 'sf_output/mb_sf_em.shp')

mb_sf_em <- mb_sf %>% left_join(mb_to_employment, by = 'MB_CODE21')


mb_pal <- colorNumeric(palette = 'Reds', domain = mb_sf_em$total_em)
leaflet(mb_sf_em) %>%
  addProviderTiles('CartoDB.Positron') %>%
  addPolygons(data = mb_sf_em, fillColor = ~mb_pal(mb_sf_em$total_em), fillOpacity = 0.7, weight = 0.1, color = 'black', opacity = 0.2)
map_xyz <- function(xyz) {

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
