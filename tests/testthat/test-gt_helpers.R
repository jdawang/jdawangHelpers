make_gt <- function() {
  gt::gt(data.frame(a = c(1, NA), b = c("x", "y")))
}

make_gt_styled <- function(mode) {
  make_gt() |> opt_stylize_jd(mode = mode)
}

test_that("opt_stylize_jd() returns a gt_tbl for both modes", {
  expect_s3_class(make_gt_styled("light"), "gt_tbl")
  expect_s3_class(make_gt_styled("dark"), "gt_tbl")
})

test_that("opt_stylize_jd() add_row_striping = FALSE works", {
  expect_no_error(make_gt() |> opt_stylize_jd(add_row_striping = FALSE))
})

test_that("finalize_gt() returns a gt_tbl", {
  result <- finalize_gt(make_gt())
  expect_s3_class(result, "gt_tbl")
})

test_that("finalize_gt() non-interactive mode succeeds without error", {
  expect_no_error(finalize_gt(make_gt(), interactive = FALSE))
})

test_that("finalize_gt() interactive mode succeeds without error", {
  expect_no_error(finalize_gt(make_gt(), interactive = TRUE))
})
