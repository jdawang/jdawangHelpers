make_raw_bp <- function() {
  sf::st_sf(
    constructi = 100000,
    month_numb = 1L,
    building_t = "Single Detached House (110)",
    units_adde = 1L,
    neighbou_2 = "DOWNTOWN",
    job_catego = "New",
    job_descri = "New house",
    date_issue = as.Date("2023-01-15"),
    count = 1L,
    latitude = 53.5,
    longitude = -113.5,
    time_permi = "2023",
    time_issue = "2023",
    row_id = 1L,
    date_permi = as.Date("2023-01-10"),
    neighbourh = "Downtown",
    permit_num = "BP2023-001",
    location = "53.5,-113.5",
    geometry = sf::st_sfc(sf::st_point(c(-113.5, 53.5)), crs = 4326)
  )
}

# clean_edmonton_bp_columns --------------------------------------------------

test_that("clean_edmonton_bp_columns renames columns correctly", {
  result <- clean_edmonton_bp_columns(make_raw_bp())
  expect_true("construction_value" %in% names(result))
  expect_true("month_number" %in% names(result))
  expect_true("building_type" %in% names(result))
  expect_true("units_added" %in% names(result))
  expect_true("neighbourhood" %in% names(result))
  expect_true("job_category" %in% names(result))
  expect_true("job_description" %in% names(result))
  expect_true("date_issued" %in% names(result))
})

test_that("clean_edmonton_bp_columns drops unused columns", {
  result <- clean_edmonton_bp_columns(make_raw_bp())
  dropped <- c(
    "count",
    "latitude",
    "longitude",
    "time_permi",
    "time_issue",
    "row_id",
    "date_permi",
    "neighbourh",
    "permit_num",
    "location",
    "constructi",
    "month_numb",
    "building_t",
    "units_adde",
    "neighbou_2",
    "job_catego",
    "job_descri",
    "date_issue"
  )
  for (col in dropped) {
    expect_false(
      col %in% names(result),
      info = paste("column", col, "should be dropped")
    )
  }
})

test_that("clean_edmonton_bp_columns preserves CRS when crs is NULL", {
  bp <- make_raw_bp()
  result <- clean_edmonton_bp_columns(bp, crs = NULL)
  expect_equal(sf::st_crs(result), sf::st_crs(bp))
})

test_that("clean_edmonton_bp_columns transforms CRS when crs is provided", {
  result <- clean_edmonton_bp_columns(make_raw_bp(), crs = 32612)
  expect_equal(sf::st_crs(result)$epsg, 32612)
})

# filter_edmonton_residential ------------------------------------------------

test_that("filter_edmonton_residential keeps residential rows", {
  bp <- data.frame(
    building_type = c(
      "Single Detached House (110)",
      "Commercial (400)",
      "Apartment (310)"
    )
  )
  result <- filter_edmonton_residential(bp)
  expect_equal(nrow(result), 2)
  expect_true(all(result$building_type %in% EDMONTON_RESIDENTIAL_BUILDING_TYPES))
})

test_that("filter_edmonton_residential returns zero rows when no residential types", {
  bp <- data.frame(building_type = c("Commercial (400)", "Industrial (500)"))
  result <- filter_edmonton_residential(bp)
  expect_equal(nrow(result), 0)
})

# add_edmonton_project_type --------------------------------------------------

make_bp_for_project_type <- function(building_type, work_type, units_added) {
  data.frame(
    building_type = building_type,
    work_type = work_type,
    units_added = units_added,
    stringsAsFactors = FALSE
  )
}

test_that("add_edmonton_project_type classifies Backyard House", {
  bp <- make_bp_for_project_type("Backyard House (110)", "(01) New", 1)
  result <- add_edmonton_project_type(bp)
  expect_equal(as.character(result$project_type), "Backyard House")
})

test_that("add_edmonton_project_type classifies Garden Suite as Backyard House", {
  bp <- make_bp_for_project_type("Garden Suite (110)", "(01) New", 1)
  result <- add_edmonton_project_type(bp)
  expect_equal(as.character(result$project_type), "Backyard House")
})

test_that("add_edmonton_project_type classifies New SFH", {
  bp <- make_bp_for_project_type("Single Detached House (110)", "(01) New", 1)
  result <- add_edmonton_project_type(bp)
  expect_equal(as.character(result$project_type), "New SFH")
})

test_that("add_edmonton_project_type classifies Addition/Conversion", {
  bp <- make_bp_for_project_type(
    "Single Detached House (110)",
    "(03) Interior Alterations",
    0
  )
  result <- add_edmonton_project_type(bp)
  expect_equal(as.character(result$project_type), "Addition/Conversion")
})

test_that("add_edmonton_project_type classifies Duplex to Fourplex", {
  bp <- make_bp_for_project_type("Duplex (210)", "(01) New", 3)
  result <- add_edmonton_project_type(bp)
  expect_equal(as.character(result$project_type), "Duplex to Fourplex")
})

test_that("add_edmonton_project_type classifies Fiveplex to Eightplex", {
  bp <- make_bp_for_project_type("Apartment (310)", "(01) New", 6)
  result <- add_edmonton_project_type(bp)
  expect_equal(as.character(result$project_type), "Fiveplex to Eightplex")
})

test_that("add_edmonton_project_type classifies 9+ Row House", {
  bp <- make_bp_for_project_type("Row House (330)", "(01) New", 12)
  result <- add_edmonton_project_type(bp)
  expect_equal(as.character(result$project_type), "9+ Row House")
})

test_that("add_edmonton_project_type classifies 9+ Apartment", {
  bp <- make_bp_for_project_type("Apartment (310)", "(01) New", 20)
  result <- add_edmonton_project_type(bp)
  expect_equal(as.character(result$project_type), "9+ Apartment")
})

test_that("add_edmonton_project_type returns factor with all levels", {
  bp <- make_bp_for_project_type("Single Detached House (110)", "(01) New", 1)
  result <- add_edmonton_project_type(bp)
  expect_s3_class(result$project_type, "factor")
  expect_equal(
    levels(result$project_type),
    c(
      "New SFH",
      "Addition/Conversion",
      "Backyard House",
      "Duplex to Fourplex",
      "Fiveplex to Eightplex",
      "9+ Row House",
      "9+ Apartment"
    )
  )
})

test_that("add_edmonton_project_type sets label attribute", {
  bp <- make_bp_for_project_type("Single Detached House (110)", "(01) New", 1)
  result <- add_edmonton_project_type(bp)
  expect_equal(attr(result$project_type, "label"), "Project type")
})

test_that("add_edmonton_project_type returns NA for unclassifiable rows", {
  bp <- make_bp_for_project_type("Commercial (400)", "(99) Other", 0)
  result <- add_edmonton_project_type(bp)
  expect_true(is.na(result$project_type))
})

# add_edmonton_suite_info ----------------------------------------------------

make_bp_suite <- function(job_description, units_added = 2) {
  data.frame(
    job_description = job_description,
    units_added = units_added,
    stringsAsFactors = FALSE
  )
}

test_that("add_edmonton_suite_info parses explicit digit count", {
  result <- add_edmonton_suite_info(make_bp_suite("New house with 3 secondary suites"))
  expect_equal(result$num_secondary_suites, 3)
})

test_that("add_edmonton_suite_info parses 'a secondary suite' as 1", {
  result <- add_edmonton_suite_info(make_bp_suite("New house with a secondary suite"))
  expect_equal(result$num_secondary_suites, 1)
})

test_that("add_edmonton_suite_info parses 'secondary suite' as 1", {
  result <- add_edmonton_suite_info(make_bp_suite("Includes secondary suite"))
  expect_equal(result$num_secondary_suites, 1)
})

test_that("add_edmonton_suite_info parses 'secondary suites' (no digit) as units_added/2", {
  result <- add_edmonton_suite_info(make_bp_suite(
    "New build with secondary suites",
    units_added = 4
  ))
  expect_equal(result$num_secondary_suites, 2)
})

test_that("add_edmonton_suite_info returns 0 for no match", {
  result <- add_edmonton_suite_info(make_bp_suite("New house"))
  expect_equal(result$num_secondary_suites, 0)
})

test_that("add_edmonton_suite_info detects garden suite as backyard home", {
  result <- add_edmonton_suite_info(make_bp_suite("New house with garden suite"))
  expect_true(result$has_backyard_home)
})

test_that("add_edmonton_suite_info detects backyard house as backyard home", {
  result <- add_edmonton_suite_info(make_bp_suite("New backyard house"))
  expect_true(result$has_backyard_home)
})

test_that("add_edmonton_suite_info returns FALSE has_backyard_home when no keyword", {
  result <- add_edmonton_suite_info(make_bp_suite("Standard renovation"))
  expect_false(result$has_backyard_home)
})
