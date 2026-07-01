#' Download Edmonton General Building Permits from the Open Data SODA3 API
#'
#' Fetches all records via the SODA3 GeoJSON endpoint, paginates automatically,
#' and caches the result to disk. The returned object has column names
#' compatible with [filter_edmonton_residential()], [add_edmonton_project_type()],
#' and the rest of the pipeline — do **not** pass the result to
#' [clean_edmonton_bp_columns()].
#'
#' @param cache_dir Path to a local directory for caching downloaded data.
#'   Cache files are named by the date range, e.g.
#'   `edmonton_building_permits_2023-01-01_all.rds`.
#' @param start_date Optional start date (inclusive) for `issue_date` filtering.
#'   A `Date` or ISO-8601 string. Default `NULL` (no lower bound).
#' @param end_date Optional end date (inclusive) for `issue_date` filtering.
#'   A `Date` or ISO-8601 string. Default `NULL` (no upper bound).
#' @param force_refresh If `TRUE`, re-downloads even if a cached file exists.
#'   Default `FALSE`.
#' @param key_id Socrata API key ID. Defaults to `SOCRATA_KEY_ID` env var.
#' @param key_secret Socrata API key secret. Defaults to `SOCRATA_KEY_SECRET`
#'   env var.
#'
#' @return An `sf` object with Point geometry (CRS 4326; `NA` for permits
#'   without coordinates), a `date_issued` `Date` column, and all other
#'   columns from the dataset.
#' @export
get_edmonton_building_permit_data <- function(
    cache_dir,
    start_date = NULL,
    end_date = NULL,
    force_refresh = FALSE,
    key_id = Sys.getenv("SOCRATA_KEY_ID"),
    key_secret = Sys.getenv("SOCRATA_KEY_SECRET")) {
  if (!nzchar(key_id) || !nzchar(key_secret)) {
    stop(
      "Socrata credentials not found. ",
      "Set SOCRATA_KEY_ID and SOCRATA_KEY_SECRET environment variables, ",
      "or pass key_id and key_secret explicitly."
    )
  }

  start_str <- if (is.null(start_date)) "all" else as.character(as.Date(start_date))
  end_str <- if (is.null(end_date)) "all" else as.character(as.Date(end_date))
  cache_file <- file.path(
    cache_dir,
    sprintf("edmonton_building_permits_%s_%s.rds", start_str, end_str)
  )
  if (!force_refresh && file.exists(cache_file)) {
    return(readRDS(cache_file))
  }

  date_filters <- c(
    if (!is.null(start_date)) sprintf("issue_date >= '%s'", as.Date(start_date)),
    if (!is.null(end_date)) sprintf("issue_date <= '%s'", as.Date(end_date))
  )
  where_clause <- if (length(date_filters) > 0) {
    paste("WHERE", paste(date_filters, collapse = " AND "))
  } else {
    ""
  }

  base_url <- "https://data.edmonton.ca/api/v3/views/24uj-dj8v/query.geojson"
  page_size <- 50000L
  offset <- 0L
  pages <- list()

  repeat {
    query <- trimws(sprintf(
      "SELECT * %s ORDER BY :id LIMIT %d OFFSET %d",
      where_clause, page_size, offset
    ))
    resp <- httr2::request(base_url) |>
      httr2::req_auth_basic(key_id, key_secret) |>
      httr2::req_url_query(query = query) |>
      httr2::req_timeout(120) |>
      httr2::req_retry(max_tries = 3, backoff = \(i) 5) |>
      httr2::req_perform()
    httr2::resp_check_status(resp)

    tmp <- tempfile(fileext = ".geojson")
    on.exit(unlink(tmp), add = TRUE)
    writeBin(httr2::resp_body_raw(resp), tmp)
    page <- sf::read_sf(tmp, quiet = TRUE)

    pages <- c(pages, list(page))
    if (nrow(page) < page_size) break
    offset <- offset + page_size
  }

  result <- do.call(rbind, pages)

  result <- dplyr::rename(result, date_issued = "issue_date")
  result <- dplyr::mutate(
    result,
    date_issued = as.Date(.data$date_issued),
    dplyr::across(
      dplyr::any_of(c("construction_value", "floor_area", "units_added", "year", "month_number")),
      as.numeric
    )
  )
  result <- dplyr::select(
    result,
    -dplyr::any_of(c(
      "row_id", "count", "latitude", "longitude", "location",
      "neighbourhood_numberr", "permit_date"
    )),
    -dplyr::starts_with(":")
  )

  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  saveRDS(result, cache_file)
  result
}

#' Clean Edmonton building permit column names
#'
#' Renames truncated column names from the Edmonton Open Data building permits
#' shapefile to human-readable names and drops unused columns. Optionally
#' transforms to a target CRS.
#'
#' @param bp An `sf` object of building permits from Edmonton Open Data.
#' @param crs A CRS specification passed to [sf::st_transform()]. If `NULL`,
#'   no transformation is applied.
#'
#' @return The input `sf` object with renamed columns and unused columns removed.
#' @export
clean_edmonton_bp_columns <- function(bp, crs = NULL) {
  bp <- bp |>
    dplyr::select(
      -dplyr::any_of(c(
        "count",
        "latitude",
        "longitude",
        "time_permi",
        "time_issue",
        "row_id",
        "date_permi",
        "neighbourh",
        "permit_num"
      )),
      -dplyr::contains("location")
    ) |>
    dplyr::rename(
      construction_value = constructi,
      month_number = month_numb,
      building_type = building_t,
      units_added = units_adde,
      neighbourhood = neighbou_2,
      job_category = job_catego,
      job_description = job_descri,
      date_issued = date_issue
    )
  bp <- bp |>
    dplyr::mutate(date_issued = as.Date(.data$date_issued))
  if (!is.null(crs)) {
    bp <- sf::st_transform(bp, crs)
  }
  bp
}

#' Filter Edmonton building permits to residential types
#'
#' Keeps only rows whose `building_type` is one of
#' [EDMONTON_RESIDENTIAL_BUILDING_TYPES].
#'
#' @param bp A data frame or `sf` object with a `building_type` column.
#'
#' @return Filtered data frame.
#' @export
filter_edmonton_residential <- function(bp) {
  dplyr::filter(
    bp,
    .data$building_type %in% EDMONTON_RESIDENTIAL_BUILDING_TYPES
  )
}

#' Add project type categorization to Edmonton building permits
#'
#' Creates a `project_type` factor column classifying each permit into one of:
#' Single detached, Addition/Conversion, Backyard House, Duplex to Fourplex,
#' Fiveplex to Eightplex, 9+ Row House, 9+ Apartment.
#'
#' Requires columns `building_type`, `work_type`, and `units_added`.
#'
#' @param bp A data frame or `sf` object of Edmonton building permits (after
#'   [clean_edmonton_bp_columns()]).
#'
#' @return The input with a new `project_type` factor column. The column has a
#'   `label` attribute set to `"Project type"` for use as a default ggplot2
#'   axis/legend title.
#' @export
add_edmonton_project_type <- function(bp) {
  bp |>
    dplyr::mutate(
      project_type = structure(
        forcats::fct(
          dplyr::case_when(
            .data$building_type %in%
              c(
                "Backyard House (110)",
                "Garden Suite (110)"
              ) ~ "Backyard House",
            stringr::str_detect(
              .data$work_type,
              stringr::coll("new", ignore_case = TRUE)
            ) &
              .data$units_added == 1 ~ "Single detached",
            .data$work_type %in%
              c(
                "(03) Interior Alterations",
                "(03) Exterior Alterations",
                "(07) Add Suites to Single Dwelling",
                "(08) Add Suites to Multi-Dwelling",
                "(09) Convert Non-Res to Residential",
                "(02) Addition",
                "(12) Move Building on to Site"
              ) ~ "Addition/Conversion",
            dplyr::between(.data$units_added, 2, 4) ~ "Duplex to Fourplex",
            dplyr::between(.data$units_added, 5, 8) ~ "Fiveplex to Eightplex",
            stringr::str_detect(
              .data$building_type,
              stringr::fixed("Row House", ignore_case = TRUE)
            ) ~ "9+ Row House",
            .data$units_added >= 9 ~ "9+ Apartment",
            .default = NA_character_
          ),
          levels = c(
            "Single detached",
            "Addition/Conversion",
            "Backyard House",
            "Duplex to Fourplex",
            "Fiveplex to Eightplex",
            "9+ Row House",
            "9+ Apartment"
          )
        ),
        label = "Project type"
      )
    )
}

#' Add secondary suite and backyard home flags to Edmonton building permits
#'
#' Extracts suite count from the `job_description` text and flags permits that
#' include a backyard home (garden suite or backyard house).
#'
#' @param bp A data frame or `sf` object with `job_description` and
#'   `units_added` columns.
#'
#' @return The input with three new columns:
#'   - `secondary_suite_desc`: raw matched text from `job_description`
#'   - `num_secondary_suites`: parsed numeric count of secondary suites
#'   - `has_backyard_home`: logical flag
#' @export
add_edmonton_suite_info <- function(bp) {
  bp |>
    dplyr::mutate(
      secondary_suite_desc = stringr::str_trim(
        stringr::str_extract(
          .data$job_description,
          stringr::regex("(\\d|a)? ?secondary suites?", ignore_case = TRUE)
        )
      ),
      num_secondary_suites = dplyr::case_when(
        stringr::str_detect(.data$secondary_suite_desc, "\\d") ~
          as.numeric(stringr::str_extract(.data$secondary_suite_desc, "\\d")),
        stringr::str_to_lower(.data$secondary_suite_desc) %in%
          c("a secondary suite", "secondary suite") ~ 1,
        stringr::str_to_lower(.data$secondary_suite_desc) %in%
          c("secondary suites", "a secondary suites") ~ .data$units_added / 2,
        .default = 0
      ),
      has_backyard_home = stringr::str_detect(
        .data$job_description,
        stringr::regex("garden suites?", ignore_case = TRUE)
      ) |
        stringr::str_detect(
          .data$job_description,
          stringr::regex("backyard houses?", ignore_case = TRUE)
        )
    )
}
