library(sf)
library(tidyverse)

#wait if groups of mb's have the same score (investigate further)
#then i should group and combine them by total score

trimmed_results <- qs::qread('qs/trimmed_results.qs')

trimmed_scores <- qs::qread('qs/trimmed_scores.qs')

mb_sf <- read_sf('~/Documents/r_projects/shapefiles/MB_2021_AUST_SHP_GDA2020/MB_2021_AUST_GDA2020.shp') %>%
  filter(GCC_NAME21 == 'Greater Melbourne') %>%
  st_transform('wgs84')

mb_dt <- mb_sf %>%
  as.data.table() %>%
  setkey(MB_CODE21)

qs::qsave(mb_dt, 'qs/mb_dt.qs')

# Create summary stats for each amenity across all mesh blocks
amenity_cols_to_exclude <- c("mesh_block_list", "mb_code21")

# Get quantiles for each amenity (for box plots)
amenity_quantiles <- trimmed_results[, lapply(.SD, quantile, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE),
                                      .SDcols = !amenity_cols_to_exclude]

# Convert to long format for easier use in reactable
amenity_stats <- data.table(
  amenity = names(amenity_quantiles),
  min = as.numeric(amenity_quantiles[1, ]),
  q25 = as.numeric(amenity_quantiles[2, ]),
  median = as.numeric(amenity_quantiles[3, ]),
  q75 = as.numeric(amenity_quantiles[4, ]),
  max = as.numeric(amenity_quantiles[5, ])
)

qs::qsave(amenity_stats, 'qs/amenity_stats.qs')

#mb_list_test <- trimmed_results[2:5,]$mb_code21 %>%
  #as.character()

#mb_sf %>% filter(MB_CODE21 %chin% mb_list_test) %>% mapgl::maplibre_view()

