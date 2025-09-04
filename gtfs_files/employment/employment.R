
#SUPERSEDED

# generate_ufi_employment_fractions <- function() {
#
#   tr_road_infra <- read_sf('sf_input/tr_road_infrastructure/Order_08APF2/mga2020_55/esrishape/customised_delivery/MELBOURNE_WATER-0/VMTRANS/TR_ROAD_INFRASTRUCTURE.shp')
#   tr_road_infra <- tr_road_infra %>%
#     st_transform(7855) %>%
#     select(UFI)
#
#   dzns_sf <- read_sf('sf_output/dzns_sf.shp') %>%
#     st_transform(7855)
#
#   ufi_buffers <- tr_road_infra %>%
#     select(UFI) %>%
#     st_buffer(dist = 150)
#
#   # Get intersections
#   ufi_employment_intersections <- st_intersection(ufi_buffers, dzns_sf) %>%
#     mutate(overlap_area = st_area(.)) %>%
#     st_drop_geometry() %>%
#     select(UFI, DZN_COD, ttl_mpl, overlap_area) %>%
#     as.data.table()
#
#   # Calculate total coverage per DZN across ALL UFIs
#   dzn_total_coverage <- ufi_employment_intersections[
#     , .(total_coverage_area = sum(overlap_area)), by = DZN_COD
#   ]
#
#   # Join back and normalize
#   ufi_employment_fractions <- ufi_employment_intersections[dzn_total_coverage, on = "DZN_COD"][
#     , allocated_employment := ttl_mpl * (overlap_area / total_coverage_area)
#   ][
#     , .(total_allocated_employment = sum(allocated_employment)), by = UFI
#   ]
#
#   ufi_employment_fractions[, UFI := as.character(UFI)]
#   setkey(ufi_employment_fractions, UFI)
#
#   return(ufi_employment_fractions)
# }
#
#
#
#
# # ss <- ufi_employment_fractions[as.character(final_destinations$UFI)]
# #
# # ss %>%
# #   as.data.frame() %>%
# #   left_join(tr_road_infra %>% mutate(UFI = as.character(UFI)), by = 'UFI') %>%
# #   st_set_geometry('geometry') -> sf
# #
# # write_sf(sf, 'sf_output/ufi_test.gpkg')
