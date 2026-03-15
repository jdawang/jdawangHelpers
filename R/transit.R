#' Load Edmonton LRT stops from GTFS
#'
#' Reads an Edmonton Transit System GTFS zip, extracts active LRT stops
#' (routes 021R, 022R, 023R), standardizes stop names, merges with a future
#' stops layer, deduplicates, and returns stop centroids.
#'
#' This function encodes Edmonton-specific logic: the three LRT route IDs,
#' known stop name inconsistencies, and ghost stops to exclude.
#'
#' @param gtfs_path Path to the ETS GTFS zip file.
#' @param service_date A `Date` used to select active service IDs from the
#'   GTFS calendar. Defaults to `NULL`, which uses all service IDs.
#' @param future_stops An `sf` object of future LRT stops with a `stop_name`
#'   column, or `NULL` to omit future stops.
#' @param crs CRS to transform the result to.
#'
#' @return An `sf` object with columns `stop_name`, `stop_name_short`, `status`
#'   (`"existing"` or `"future"`), and point geometry.
#' @export
load_edmonton_transit_stops <- function(gtfs_path, service_date = NULL,
                                        future_stops = NULL, crs = NULL) {
  gtfs <- tidytransit::read_gtfs(gtfs_path)
  route_ids <- c("021R", "022R", "023R")

  service_ids <- if (!is.null(service_date)) {
    dplyr::filter(gtfs$calendar, .data$date == service_date) |>
      dplyr::pull("service_id")
  } else {
    NULL
  }

  existing <- tidytransit::gtfs_as_sf(gtfs) |>
    tidytransit::filter_stops(
      route_ids   = route_ids,
      service_ids = service_ids
    ) |>
    dplyr::mutate(
      stop_name = dplyr::case_match(
        .data$stop_name,
        "Bay Enterprise Square  Station" ~ "Bay Enterprise Square Station",
        "Churchill Stop"                 ~ "Churchill Station",
        .default = .data$stop_name
      ),
      status = "existing"
    ) |>
    dplyr::filter(
      !(.data$stop_name %in% c("Metro Line JTTl Track", "DL MacDonald Platform", "NAIT Station"))
    )

  stops <- if (!is.null(future_stops)) {
    future_stops <- dplyr::mutate(future_stops, status = "future")
    dplyr::bind_rows(existing, future_stops)
  } else {
    existing
  }

  stops <- stops |>
    dplyr::group_by(.data$stop_name, .data$status) |>
    dplyr::summarize(geometry = sf::st_union(.data$geometry), .groups = "drop") |>
    dplyr::mutate(
      geometry        = sf::st_centroid(.data$geometry),
      stop_name_short = stringr::str_remove(.data$stop_name, " (Stop|Station)")
    )

  if (!is.null(crs)) stops <- sf::st_transform(stops, crs)
  stops
}

#' Add distance-to-transit column
#'
#' Calculates the minimum distance in kilometres from each row to the nearest
#' stop in `transit_stops`. Optionally filters stops by `status`.
#'
#' @param data An `sf` object.
#' @param transit_stops An `sf` object of transit stops.
#' @param status_filter Character string to filter `transit_stops$status`
#'   (e.g. `"existing"`). `NULL` uses all stops.
#'
#' @return The input with a new `distance_from_lrt` numeric column (km). The
#'   column has a `label` attribute for use as a default ggplot2 axis title.
#' @export
add_transit_distance <- function(data, transit_stops, status_filter = NULL) {
  stops <- if (!is.null(status_filter)) {
    dplyr::filter(transit_stops, .data$status == status_filter)
  } else {
    transit_stops
  }
  data |> dplyr::mutate(
    distance_from_lrt = structure(
      apply(sf::st_distance(data, stops), 1, min) / 1000,
      label = "Distance from closest LRT stop (km)"
    )
  )
}

#' Build concentric transit buffer rings
#'
#' Creates `sf` linestring boundaries (rings) around transit stops at each
#' specified radius. Useful as map overlays.
#'
#' @param transit_stops An `sf` object of transit stop points.
#' @param radii_km Numeric vector of radii in kilometres. Defaults to
#'   `c(1, 1.5, 2)`.
#'
#' @return An `sf` object with columns `radius` (ordered factor) and linestring
#'   geometry.
#' @export
make_transit_buffers <- function(transit_stops, radii_km = c(1, 1.5, 2)) {
  make_one <- function(r) {
    transit_stops |>
      sf::st_buffer(units::as_units(paste0(r, " km"))) |>
      sf::st_union() |>
      sf::st_boundary() |>
      sf::st_sf() |>
      dplyr::mutate(radius = paste0(r, "km"))
  }
  result <- purrr::map(radii_km, make_one) |> dplyr::bind_rows()
  labels <- paste0(radii_km, "km")
  dplyr::mutate(result, radius = forcats::fct_relevel(.data$radius, labels))
}

#' Compute weighted ECDF by distance from transit
#'
#' Groups `data` by `group_var`, sorts by `distance_from_lrt`, and computes
#' a cumulative share of `weight_var`. Returns the ECDF values as
#' `ecdf_values`.
#'
#' @param data A data frame with `distance_from_lrt` and the columns referenced
#'   by `group_var` and `weight_var`.
#' @param group_var <[`data-masking`][dplyr::dplyr_data_masking]> Grouping
#'   variable. Defaults to `year`.
#' @param weight_var <[`data-masking`][dplyr::dplyr_data_masking]> Variable to
#'   accumulate. Defaults to `units_added`.
#'
#' @return The input with new columns `cum_units` and `ecdf_values`, ungrouped.
#' @export
add_ecdf_by_distance <- function(data, group_var = year, weight_var = units_added) {
  data |>
    dplyr::group_by({{ group_var }}) |>
    dplyr::arrange(.data$distance_from_lrt, .by_group = TRUE) |>
    dplyr::mutate(
      cum_units   = cumsum({{ weight_var }}),
      ecdf_values = .data$cum_units / sum({{ weight_var }})
    ) |>
    dplyr::ungroup()
}
