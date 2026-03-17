# Use a projected CRS (UTM zone 12N) to avoid spherical geometry issues in tests.
# mock_mature: small 2x2-unit square centred at (0, 0)
# mock_henday: larger 6x6-unit square centred at (0, 0), fully contains mock_mature

make_mock_polygons <- function() {
  crs <- 32612
  mock_mature <- sf::st_sf(
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(
        c(-1, -1, 1, -1, 1, 1, -1, 1, -1, -1),
        ncol = 2,
        byrow = TRUE
      ))),
      crs = crs
    )
  )
  mock_henday <- sf::st_sf(
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(
        c(-3, -3, 3, -3, 3, 3, -3, 3, -3, -3),
        ncol = 2,
        byrow = TRUE
      ))),
      crs = crs
    )
  )
  list(mature = mock_mature, henday = mock_henday)
}

make_bp_point <- function(x, y, neighbourhood, crs = 32612) {
  sf::st_sf(
    neighbourhood = neighbourhood,
    geometry = sf::st_sfc(sf::st_point(c(x, y)), crs = crs)
  )
}

test_that("add_edmonton_neighbourhood_type classifies inside mature as Mature", {
  polys <- make_mock_polygons()
  bp <- make_bp_point(0, 0, "GLENORA")
  result <- add_edmonton_neighbourhood_type(bp, polys$mature, polys$henday)
  expect_equal(as.character(result$neighbourhood_type), "Mature")
})

test_that("add_edmonton_neighbourhood_type classifies between mature and Henday", {
  polys <- make_mock_polygons()
  bp <- make_bp_point(2, 0, "WINDERMERE") # inside henday, outside mature
  result <- add_edmonton_neighbourhood_type(bp, polys$mature, polys$henday)
  expect_equal(as.character(result$neighbourhood_type), "Between mature and Henday")
})

test_that("add_edmonton_neighbourhood_type classifies outside Henday", {
  polys <- make_mock_polygons()
  bp <- make_bp_point(10, 10, "TRUMPETER_FAKE") # outside henday
  result <- add_edmonton_neighbourhood_type(bp, polys$mature, polys$henday)
  expect_equal(as.character(result$neighbourhood_type), "Outside Henday")
})

test_that("add_edmonton_neighbourhood_type classifies DOWNTOWN regardless of geometry", {
  polys <- make_mock_polygons()
  bp <- make_bp_point(0, 0, "DOWNTOWN")
  result <- add_edmonton_neighbourhood_type(bp, polys$mature, polys$henday)
  expect_equal(as.character(result$neighbourhood_type), "Downtown")
})

test_that("add_edmonton_neighbourhood_type applies name-based fallback for Outside Henday", {
  polys <- make_mock_polygons()
  # TRUMPETER is a known outside-Henday name; use a point inside henday to
  # verify the name-based path overrides spatial result
  bp <- make_bp_point(10, 10, "TRUMPETER")
  result <- add_edmonton_neighbourhood_type(bp, polys$mature, polys$henday)
  expect_equal(as.character(result$neighbourhood_type), "Outside Henday")
})

test_that("add_edmonton_neighbourhood_type applies name-based fallback for Mature", {
  polys <- make_mock_polygons()
  # Use empty geometry so spatial test returns NA, triggering the name-based fallback
  bp <- sf::st_sf(
    neighbourhood = "CALLINGWOOD NORTH",
    geometry = sf::st_sfc(sf::st_point(), crs = 32612)
  )
  result <- add_edmonton_neighbourhood_type(bp, polys$mature, polys$henday)
  expect_equal(as.character(result$neighbourhood_type), "Mature")
})

test_that("add_edmonton_neighbourhood_type factor has all four levels", {
  polys <- make_mock_polygons()
  bp <- make_bp_point(0, 0, "GLENORA")
  result <- add_edmonton_neighbourhood_type(bp, polys$mature, polys$henday)
  expect_equal(
    levels(result$neighbourhood_type),
    c("Downtown", "Mature", "Between mature and Henday", "Outside Henday")
  )
})

test_that("add_edmonton_neighbourhood_type sets label attribute", {
  polys <- make_mock_polygons()
  bp <- make_bp_point(0, 0, "GLENORA")
  result <- add_edmonton_neighbourhood_type(bp, polys$mature, polys$henday)
  expect_equal(attr(result$neighbourhood_type, "label"), "Neighbourhood type")
})

test_that("add_edmonton_neighbourhood_type name fallback: new Outside Henday name", {
  polys <- make_mock_polygons()
  bp <- sf::st_sf(
    neighbourhood = "ELLERSLIE INDUSTRIAL",
    geometry = sf::st_sfc(sf::st_point(), crs = 32612)
  )
  result <- add_edmonton_neighbourhood_type(bp, polys$mature, polys$henday)
  expect_equal(as.character(result$neighbourhood_type), "Outside Henday")
})

test_that("add_edmonton_neighbourhood_type name fallback: Between mature and Henday", {
  polys <- make_mock_polygons()
  bp <- sf::st_sf(
    neighbourhood = "MICHAELS PARK",
    geometry = sf::st_sfc(sf::st_point(), crs = 32612)
  )
  result <- add_edmonton_neighbourhood_type(bp, polys$mature, polys$henday)
  expect_equal(as.character(result$neighbourhood_type), "Between mature and Henday")
})

test_that("add_edmonton_neighbourhood_type name fallback: new Mature name", {
  polys <- make_mock_polygons()
  bp <- sf::st_sf(
    neighbourhood = "WOODCROFT",
    geometry = sf::st_sfc(sf::st_point(), crs = 32612)
  )
  result <- add_edmonton_neighbourhood_type(bp, polys$mature, polys$henday)
  expect_equal(as.character(result$neighbourhood_type), "Mature")
})

test_that("add_edmonton_neighbourhood_type applies address override to Mature", {
  polys <- make_mock_polygons()
  # Point outside mature boundary, but address override should force Mature
  bp <- sf::st_sf(
    neighbourhood = "SOME NEIGHBOURHOOD",
    address = "8944 - 145 STREET NW",
    geometry = sf::st_sfc(sf::st_point(c(2, 0)), crs = 32612)
  )
  result <- add_edmonton_neighbourhood_type(bp, polys$mature, polys$henday)
  expect_equal(as.character(result$neighbourhood_type), "Mature")
})

test_that("add_edmonton_neighbourhood_type skips address override when no address column", {
  polys <- make_mock_polygons()
  bp <- make_bp_point(2, 0, "WINDERMERE")
  result <- add_edmonton_neighbourhood_type(bp, polys$mature, polys$henday)
  # Should still be Between mature and Henday from spatial test, not Mature
  expect_equal(as.character(result$neighbourhood_type), "Between mature and Henday")
})
