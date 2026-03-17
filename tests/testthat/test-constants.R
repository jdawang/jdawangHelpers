test_that("EDMONTON_RESIDENTIAL_BUILDING_TYPES is a character vector of length 15", {
  expect_type(EDMONTON_RESIDENTIAL_BUILDING_TYPES, "character")
  expect_length(EDMONTON_RESIDENTIAL_BUILDING_TYPES, 15)
})

test_that("CAPTION_COE is correct", {
  expect_identical(CAPTION_COE, "Jacob Dawang, City of Edmonton Open Data")
})

test_that("CAPTION_COE_SC is correct", {
  expect_identical(
    CAPTION_COE_SC,
    "Jacob Dawang, City of Edmonton Open Data, StatCan census"
  )
})

test_that("CAPTION_TORONTO is correct", {
  expect_identical(CAPTION_TORONTO, "Jacob Dawang, data StatCan census, Metrolinx")
})
