test_that("theme_jd() returns a ggplot2 theme object", {
  result <- theme_jd()
  expect_s3_class(result, "theme")
  expect_s3_class(result, "gg")
})

test_that("theme_jd() dark mode succeeds without error", {
  expect_no_error(theme_jd(mode = "dark"))
})

test_that("theme_jd() light mode succeeds without error", {
  expect_no_error(theme_jd(mode = "light"))
})

test_that("theme_jd() errors on invalid mode", {
  expect_error(theme_jd(mode = "blue"))
})

test_that("theme_map() returns a ggplot2 theme object", {
  result <- theme_map()
  expect_s3_class(result, "theme")
  expect_s3_class(result, "gg")
})

test_that("theme_map() dark mode succeeds without error", {
  expect_no_error(theme_map(mode = "dark"))
})

test_that("theme_map() light mode succeeds without error", {
  expect_no_error(theme_map(mode = "light"))
})
