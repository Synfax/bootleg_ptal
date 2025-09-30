library(sf)
library(tidyverse)

final_result = read_sf('sf_output/final_result.gpkg') %>%
  st_drop_geometry
