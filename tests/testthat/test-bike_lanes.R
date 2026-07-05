# add_bike_lane_distance -----------------------------------------------------

make_bike_lanes <- function() {
  crs <- 32612
  sf::st_sf(
    id = c("A", "B"),
    classification = c("Protected Bike Lane", "Painted Bike Lane"),
    geometry = sf::st_sfc(
      # 1 km north of origin, spanning x in [-1000, 1000]
      sf::st_linestring(rbind(c(-1000, 1000), c(1000, 1000))),
      # 0.1 km north of origin, spanning x in [-1000, 1000]
      sf::st_linestring(rbind(c(-1000, 100), c(1000, 100))),
      crs = crs
    )
  )
}

make_bike_data_point <- function() {
  sf::st_sf(
    id = 1L,
    geometry = sf::st_sfc(sf::st_point(c(0, 0)), crs = 32612)
  )
}

test_that("add_bike_lane_distance computes nearest-line distance in km", {
  result <- add_bike_lane_distance(make_bike_data_point(), make_bike_lanes())
  # Nearest is the "Painted Bike Lane" at y = 100, i.e. 0.1 km away
  expect_equal(result$distance_from_bike_infra, 0.1, tolerance = 0.01, ignore_attr = TRUE)
})

test_that("add_bike_lane_distance respects classification_filter", {
  result <- add_bike_lane_distance(
    make_bike_data_point(),
    make_bike_lanes(),
    classification_filter = "Protected Bike Lane"
  )
  # Only the "Protected Bike Lane" at y = 1000 should be considered → 1 km
  expect_equal(result$distance_from_bike_infra, 1, tolerance = 0.01, ignore_attr = TRUE)
})

test_that("add_bike_lane_distance sets label attribute", {
  result <- add_bike_lane_distance(make_bike_data_point(), make_bike_lanes())
  expect_equal(
    attr(result$distance_from_bike_infra, "label"),
    "Distance from closest bike infrastructure (km)"
  )
})

# add_bike_lane_ecdf_by_distance ----------------------------------------------

make_bike_ecdf_data <- function() {
  data.frame(
    year = c(2020, 2020, 2020, 2021, 2021),
    distance_from_bike_infra = c(1, 2, 3, 1, 4),
    units_added = c(10, 20, 30, 5, 15)
  )
}

test_that("add_bike_lane_ecdf_by_distance ecdf_values max per group is 1", {
  result <- add_bike_lane_ecdf_by_distance(make_bike_ecdf_data())
  maxes <- tapply(result$ecdf_values, result$year, max)
  expect_true(all(maxes == 1))
})

test_that("add_bike_lane_ecdf_by_distance ecdf_values are non-decreasing within each group", {
  result <- add_bike_lane_ecdf_by_distance(make_bike_ecdf_data())
  for (yr in unique(result$year)) {
    vals <- result$ecdf_values[result$year == yr]
    expect_true(all(diff(vals) >= 0), info = paste("year", yr))
  }
})

test_that("add_bike_lane_ecdf_by_distance cum_units equals cumsum of weight_var within group", {
  result <- add_bike_lane_ecdf_by_distance(make_bike_ecdf_data())
  result_2020 <- result[result$year == 2020, ]
  result_2020 <- result_2020[order(result_2020$distance_from_bike_infra), ]
  expect_equal(result_2020$cum_units, cumsum(result_2020$units_added))
})

# get_edmonton_bike_lane_data --------------------------------------------------

test_that("get_edmonton_bike_lane_data is skipped (requires network + Socrata credentials)", {
  skip("Requires network access and SOCRATA_KEY_ID/SOCRATA_KEY_SECRET — run manually")
  get_edmonton_bike_lane_data(cache_dir = tempdir())
})

test_that("get_edmonton_bike_lane_data reads from cache without hitting the network", {
  cache_dir <- withr::local_tempdir()
  fake_data <- sf::st_sf(
    id = "1",
    classification = "Protected Bike Lane",
    geometry = sf::st_sfc(
      sf::st_linestring(rbind(c(0, 0), c(1, 1))),
      crs = 4326
    )
  )
  readr::write_rds(fake_data, file.path(cache_dir, "edmonton_bike_lanes.rds"))

  result <- get_edmonton_bike_lane_data(
    cache_dir = cache_dir,
    key_id = "fake",
    key_secret = "fake"
  )
  expect_equal(result$classification, "Protected Bike Lane")
})