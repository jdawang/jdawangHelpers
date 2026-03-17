# load_edmonton_transit_stops ------------------------------------------------

test_that("load_edmonton_transit_stops is skipped (requires real GTFS file)", {
  skip("Requires a real ETS GTFS zip file — run manually with gtfs_path set")
  load_edmonton_transit_stops("path/to/ets.zip")
})

# add_transit_distance -------------------------------------------------------

make_stops <- function() {
  crs <- 32612
  sf::st_sf(
    stop_name = c("Stop A", "Stop B"),
    status = c("existing", "future"),
    geometry = sf::st_sfc(
      sf::st_point(c(0, 1000)), # 1 km north of origin
      sf::st_point(c(5000, 0)), # 5 km east of origin
      crs = crs
    )
  )
}

make_data_point <- function() {
  sf::st_sf(
    id = 1L,
    geometry = sf::st_sfc(sf::st_point(c(0, 0)), crs = 32612)
  )
}

test_that("add_transit_distance computes distance in km", {
  result <- add_transit_distance(make_data_point(), make_stops())
  expect_equal(result$distance_from_lrt, 1, tolerance = 0.01, ignore_attr = TRUE)
})

test_that("add_transit_distance respects status_filter", {
  result <- add_transit_distance(
    make_data_point(),
    make_stops(),
    status_filter = "future"
  )
  # Only the future stop at (5000, 0) should be considered → ~5 km
  expect_equal(result$distance_from_lrt, 5, tolerance = 0.01, ignore_attr = TRUE)
})

test_that("add_transit_distance sets label attribute", {
  result <- add_transit_distance(make_data_point(), make_stops())
  expect_equal(
    attr(result$distance_from_lrt, "label"),
    "Distance from closest LRT stop (km)"
  )
})

# make_transit_buffers -------------------------------------------------------

make_transit_stops_projected <- function() {
  sf::st_sf(
    stop_name = "Central",
    geometry = sf::st_sfc(sf::st_point(c(0, 0)), crs = 32612)
  )
}

test_that("make_transit_buffers returns one row per radius", {
  result <- make_transit_buffers(make_transit_stops_projected(), radii_km = c(1, 2))
  expect_equal(nrow(result), 2)
})

test_that("make_transit_buffers radius column is a factor with correct levels", {
  result <- make_transit_buffers(
    make_transit_stops_projected(),
    radii_km = c(0.5, 1, 1.5)
  )
  expect_s3_class(result$radius, "factor")
  expect_equal(levels(result$radius), c("0.5km", "1km", "1.5km"))
})

test_that("make_transit_buffers output geometry is linestring type", {
  result <- make_transit_buffers(make_transit_stops_projected(), radii_km = c(1))
  geom_types <- sf::st_geometry_type(result, by_geometry = FALSE)
  expect_true(geom_types %in% c("LINESTRING", "MULTILINESTRING", "GEOMETRYCOLLECTION"))
})

# add_ecdf_by_distance -------------------------------------------------------

make_ecdf_data <- function() {
  data.frame(
    year = c(2020, 2020, 2020, 2021, 2021),
    distance_from_lrt = c(1, 2, 3, 1, 4),
    units_added = c(10, 20, 30, 5, 15)
  )
}

test_that("add_ecdf_by_distance ecdf_values max per group is 1", {
  result <- add_ecdf_by_distance(make_ecdf_data())
  maxes <- tapply(result$ecdf_values, result$year, max)
  expect_true(all(maxes == 1))
})

test_that("add_ecdf_by_distance ecdf_values are non-decreasing within each group", {
  result <- add_ecdf_by_distance(make_ecdf_data())
  for (yr in unique(result$year)) {
    vals <- result$ecdf_values[result$year == yr]
    expect_true(all(diff(vals) >= 0), info = paste("year", yr))
  }
})

test_that("add_ecdf_by_distance cum_units equals cumsum of weight_var within group", {
  result <- add_ecdf_by_distance(make_ecdf_data())
  result_2020 <- result[result$year == 2020, ]
  result_2020 <- result_2020[order(result_2020$distance_from_lrt), ]
  expect_equal(result_2020$cum_units, cumsum(result_2020$units_added))
})

test_that("add_ecdf_by_distance works with non-default group_var and weight_var", {
  df <- data.frame(
    city = c("A", "A", "B"),
    distance_from_lrt = c(1, 2, 1),
    count = c(3, 7, 4)
  )
  result <- add_ecdf_by_distance(df, group_var = city, weight_var = count)
  expect_true("ecdf_values" %in% names(result))
  expect_true("cum_units" %in% names(result))
  maxes <- tapply(result$ecdf_values, result$city, max)
  expect_true(all(maxes == 1))
})
