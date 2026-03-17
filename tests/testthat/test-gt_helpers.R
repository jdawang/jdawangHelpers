make_gt <- function() {
  gt::gt(data.frame(a = c(1, NA), b = c("x", "y")))
}

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
