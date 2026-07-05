#' Download Edmonton bike lane network from the Open Data SODA3 API
#'
#' Fetches all bike lane network segments via the SODA3 GeoJSON endpoint and
#' caches the result to disk. Unlike [get_edmonton_building_permit_data()],
#' this does not paginate: the full dataset (~10,400 rows as of 2026) fits in
#' a single un-paginated `SELECT *` response.
#'
#' @param cache_dir Path to a local directory for caching downloaded data.
#'   Cache file is named `edmonton_bike_lanes.rds`.
#' @param force_refresh If `TRUE`, re-downloads even if a cached file exists.
#'   Default `FALSE`.
#' @param key_id Socrata API key ID. Defaults to `SOCRATA_KEY_ID` env var.
#' @param key_secret Socrata API key secret. Defaults to `SOCRATA_KEY_SECRET`
#'   env var.
#'
#' @return An `sf` object with MULTILINESTRING geometry (CRS 4326) and columns
#'   `id`, `type`, `classification`, `network_classification`,
#'   `road_segment_type`, `duration`, `street_name_full`, `line_weight`,
#'   `route_coming_soon`, and `construction_year`.
#' @export
get_edmonton_bike_lane_data <- function(
    cache_dir,
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

  cache_file <- file.path(cache_dir, "edmonton_bike_lanes.rds")
  if (!force_refresh && file.exists(cache_file)) {
    return(readr::read_rds(cache_file))
  }

  base_url <- "https://data.edmonton.ca/api/v3/views/vd4b-a4iv/query.geojson"
  resp <- httr2::request(base_url) |>
    httr2::req_auth_basic(key_id, key_secret) |>
    httr2::req_url_query(query = "SELECT *") |>
    httr2::req_timeout(120) |>
    httr2::req_retry(max_tries = 3, backoff = \(i) 5) |>
    httr2::req_perform()
  httr2::resp_check_status(resp)

  tmp <- tempfile(fileext = ".geojson")
  on.exit(unlink(tmp), add = TRUE)
  writeBin(httr2::resp_body_raw(resp), tmp)
  result <- sf::read_sf(tmp, quiet = TRUE) |>
    dplyr::select(-dplyr::starts_with(":"))

  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  readr::write_rds(result, cache_file)
  result
}

#' Add distance-to-bike-infrastructure column
#'
#' Calculates the minimum distance in kilometres from each row to the nearest
#' bike lane segment in `bike_lanes`. Optionally filters to a subset of
#' `classification` values first (e.g. to exclude painted-lane-only
#' segments that aren't physically separated infrastructure).
#'
#' @param data An `sf` object.
#' @param bike_lanes An `sf` object of bike lane segments (e.g. from
#'   [get_edmonton_bike_lane_data()]), with a `classification` column.
#' @param classification_filter Character vector of `classification` values to
#'   keep before computing distance. `NULL` (default) uses all segments.
#'
#' @return The input with a new `distance_from_bike_infra` numeric column
#'   (km). The column has a `label` attribute for use as a default ggplot2
#'   axis title.
#' @export
add_bike_lane_distance <- function(data, bike_lanes, classification_filter = NULL) {
  lanes <- if (!is.null(classification_filter)) {
    dplyr::filter(bike_lanes, .data$classification %in% classification_filter)
  } else {
    bike_lanes
  }
  lanes <- lanes |> sf::st_union() |> sf::st_sf()
  add_distance_generic(
    data, lanes,
    distance_col = "distance_from_bike_infra",
    label = "Distance from closest bike infrastructure (km)"
  )
}

#' Compute weighted ECDF by distance from bike infrastructure
#'
#' Thin wrapper around the same generic ECDF machinery used by
#' [add_ecdf_by_distance()], operating on `distance_from_bike_infra` instead
#' of `distance_from_lrt`.
#'
#' @inheritParams add_ecdf_by_distance
#' @export
add_bike_lane_ecdf_by_distance <- function(data, group_var = year, weight_var = units_added) {
  add_ecdf_by_distance_generic(
    data,
    distance_col = "distance_from_bike_infra",
    group_var = {{ group_var }},
    weight_var = {{ weight_var }}
  )
}
