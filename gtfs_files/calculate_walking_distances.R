calculate_walking_distances <- function() {

  if(file.exists('rdata_output/walking_distances_new.Rdata')) {
    return(readRDS('rdata_output/walking_distances_new.Rdata'))
  } else {
    print('NOT WRITTEN YET')
  }

}
