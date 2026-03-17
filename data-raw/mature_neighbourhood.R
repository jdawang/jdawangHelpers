# Run this script once to prepare the mature_neighbourhood boundary dataset.
# Source: City of Edmonton Open Data
# https://data.edmonton.ca/Urban-Planning-Economy/Mature-Neighbourhoods/abvn-bnzg

library(sf)

mature_neighbourhood <- read_sf(
  "~/repos/website2/data/Mature Neighbourhoods_20241222.geojson"
)

usethis::use_data(mature_neighbourhood, overwrite = TRUE)
