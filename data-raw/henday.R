# Run this script once to prepare the Anthony Henday Drive boundary dataset.
# Source: City of Edmonton Open Data (derived from road network or custom polygon)

library(sf)

henday <- read_sf("~/repos/website2/data/henday.geojson")

usethis::use_data(henday, overwrite = TRUE)
