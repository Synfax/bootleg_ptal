install.packages("mapgl")
library(mapgl)
library(tidyverse)
library(sf)

mb_sf_em <- read_sf('sf_output/mb_sf_em.shp')

mb_quartiled <- mb_sf_em %>%
  mutate(decile = ntile(max_e,10))

mb_quartiled = mb_quartiled %>%
  group_by(decile) %>%
  summarise(geometry = st_union(geometry))

#write_sf(mb_quartiled, 'sf_output/quartile_mb.shp')

brewer_pal <- RColorBrewer::brewer.pal(10, "RdYlBu")

mb_quartiled_simple = st_simplify(mb_quartiled, dTolerance = 250, preserveTopology = T)

maplibre(bounds = mb_quartiled_simple) |>
  add_fill_layer(id = "nc_data",
                 source = mb_quartiled_simple,
                 fill_color = step_expr(
                   column = "decile",
                   base = brewer_pal[1],  # color for decile 1
                   stops = brewer_pal[2:10],  # colors for deciles 2-10
                   values = 2:10,  # break points at 2,3,4,5,6,7,8,9,10
                   na_color = "blue"
                 ),
                 fill_opacity = 0.5)
