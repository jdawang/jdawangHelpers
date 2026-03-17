test_that("layers_transit_ecdf() returns a list", {
  result <- layers_transit_ecdf()
  expect_type(result, "list")
})

test_that("layers_transit_ecdf() with show_800m_line = TRUE returns 6 elements", {
  result <- layers_transit_ecdf(show_800m_line = TRUE)
  expect_length(result, 6)
})

test_that("layers_transit_ecdf() with show_800m_line = FALSE returns 4 elements", {
  result <- layers_transit_ecdf(show_800m_line = FALSE)
  expect_length(result, 4)
})

test_that("layers_map_base() errors when mountainmathHelpers is absent", {
  withr::with_options(list(), {
    testthat::with_mocked_bindings(
      requireNamespace = function(pkg, quietly) FALSE,
      .package = "base",
      expect_error(layers_map_base(), "mountainmathHelpers")
    )
  })
})
