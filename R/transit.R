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
load_edmonton_transit_stops <- function(
  gtfs_path,
  service_date = NULL,
  future_stops = NULL,
  crs = NULL
) {
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
      route_ids = route_ids,
      service_ids = service_ids
    ) |>
    dplyr::mutate(
      stop_name = dplyr::case_match(
        .data$stop_name,
        "Bay Enterprise Square  Station" ~ "Bay Enterprise Square Station",
        "Churchill Stop" ~ "Churchill Station",
        .default = .data$stop_name
      ),
      status = "existing"
    ) |>
    dplyr::filter(
      !(.data$stop_name %in%
        c("Metro Line JTTl Track", "DL MacDonald Platform", "NAIT Station"))
    )

  stops <- if (!is.null(future_stops)) {
    future_stops <- dplyr::mutate(future_stops, status = "future")
    dplyr::bind_rows(existing, future_stops)
  } else {
    existing
  }

  stops <- stops |>
    dplyr::group_by(.data$stop_name, .data$status) |>
    dplyr::summarize(
      geometry = sf::st_union(.data$geometry),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      geometry = sf::st_centroid(.data$geometry),
      stop_name_short = stringr::str_remove(.data$stop_name, " (Stop|Station)")
    )

  if (!is.null(crs)) {
    stops <- sf::st_transform(stops, crs)
  }
  stops
}

#' Service IDs active on a given date
#'
#' Shared helper for looking up which GTFS `service_id`s are active on a
#' date, used by both [load_edmonton_transit_stops()] and
#' [get_edmonton_frequent_bus_stops()].
#'
#' @param gtfs A `tidytransit` GTFS object.
#' @param service_date A `Date`, or `NULL` to return `NULL` (all services).
#'
#' @return A character vector of `service_id`s, or `NULL`.
#' @keywords internal
#' @noRd
service_ids_for_date <- function(gtfs, service_date) {
  if (is.null(service_date)) {
    return(NULL)
  }
  dplyr::filter(gtfs$calendar, .data$date == service_date) |>
    dplyr::pull("service_id")
}

#' Stop IDs meeting a frequency threshold
#'
#' Given a `get_stop_frequency()`-style data frame, returns the distinct
#' `stop_id`s whose `mean_headway` (seconds) is at or under the threshold.
#' Separated out from [get_edmonton_frequent_bus_stops()] so the filtering
#' logic can be unit-tested without a real GTFS file.
#'
#' @param freq_df A data frame with `stop_id` and `mean_headway` (seconds)
#'   columns, e.g. from `tidytransit::get_stop_frequency()`.
#' @param threshold_min Numeric headway threshold in minutes.
#'
#' @return A character vector of distinct `stop_id`s.
#' @keywords internal
#' @noRd
frequent_stop_ids <- function(freq_df, threshold_min) {
  dplyr::filter(freq_df, .data$mean_headway <= threshold_min * 60) |>
    dplyr::distinct(.data$stop_id) |>
    dplyr::pull("stop_id")
}

#' Load Edmonton frequent bus network stops from GTFS
#'
#' Reads an Edmonton Transit System GTFS zip, restricts trips to the given
#' frequent-network route numbers (routes 1-9 by default), and returns the
#' stops where scheduled headway meets a frequency threshold.
#'
#' Headway is computed per route (via `tidytransit::get_stop_frequency()`
#' with `by_route = TRUE`), not by combining unrelated routes at a shared
#' stop. Route 1 branches into 1A and 1B, which share a single GTFS
#' `route_id` but run every ~30 minutes individually; per-route aggregation
#' automatically recombines them into ~15-minute service on their shared
#' trunk, while leaving branch-only stops at ~30 minutes, with no
#' route-specific special-casing required.
#'
#' @param gtfs_path Path to the ETS GTFS zip file.
#' @param route_numbers Integer vector of frequent-network route numbers.
#'   Defaults to `1:9`. Matched against `route_short_name` as zero-padded
#'   three-digit strings (e.g. `"001"`).
#' @param service_date A `Date` used to select active service IDs from the
#'   GTFS calendar. Defaults to `NULL`, which uses the service ID with the
#'   most departures (see `tidytransit::get_stop_frequency()`).
#' @param start_time,end_time Analysis window for headway calculation, as
#'   `"HH:MM:SS"` strings. Default `"06:00:00"`-`"21:00:00"`.
#' @param freq_threshold_min Numeric headway threshold in minutes. A stop is
#'   considered part of the frequent network if at least one route serving
#'   it has a mean headway at or under this threshold. Default `15`.
#' @param crs CRS to transform the result to.
#'
#' @return An `sf` object with columns `stop_id`, `stop_name`, and point
#'   geometry.
#' @export
get_edmonton_frequent_bus_stops <- function(
  gtfs_path,
  route_numbers = 1:9,
  service_date = NULL,
  start_time = "06:00:00",
  end_time = "21:00:00",
  freq_threshold_min = 15,
  crs = NULL
) {
  gtfs <- tidytransit::read_gtfs(gtfs_path)
  route_short_names <- stringr::str_pad(route_numbers, width = 3, pad = "0")
  freq_route_ids <- gtfs$routes |>
    dplyr::filter(.data$route_short_name %in% route_short_names) |>
    dplyr::pull("route_id")
  trip_ids <- gtfs$trips |>
    dplyr::filter(.data$route_id %in% freq_route_ids) |>
    dplyr::pull("trip_id")
  gtfs_f <- tidytransit::filter_feed_by_trips(gtfs, trip_ids)

  service_ids <- service_ids_for_date(gtfs, service_date)
  freq <- tidytransit::get_stop_frequency(
    gtfs_f,
    start_time = start_time,
    end_time = end_time,
    service_ids = service_ids,
    by_route = TRUE
  )
  stop_ids <- frequent_stop_ids(freq, freq_threshold_min)

  stops <- tidytransit::gtfs_as_sf(gtfs_f)$stops |>
    dplyr::filter(.data$stop_id %in% stop_ids) |>
    dplyr::select("stop_id", "stop_name")

  if (!is.null(crs)) {
    stops <- sf::st_transform(stops, crs)
  }
  stops
}

#' Compute minimum distance from each row to a reference geometry
#'
#' Generic core shared by [add_transit_distance()] and
#' [add_bike_lane_distance()]. Not exported.
#'
#' @param data An `sf` object.
#' @param ref_geom An `sf` object to measure distance to.
#' @param distance_col Name of the output column (string).
#' @param label Value for the output column's `label` attribute.
#'
#' @return `data` with a new numeric column named `distance_col` (km).
#' @keywords internal
#' @noRd
add_distance_generic <- function(data, ref_geom, distance_col, label) {
  dplyr::mutate(
    data,
    "{distance_col}" := structure(
      apply(sf::st_distance(data, ref_geom), 1, min) / 1000,
      label = label
    )
  )
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
  add_distance_generic(
    data, stops,
    distance_col = "distance_from_lrt",
    label = "Distance from closest LRT stop (km)"
  )
}

#' Add distance-to-frequent-bus-stop column
#'
#' Calculates the minimum distance in kilometres from each row to the
#' nearest stop in `frequent_bus_stops` (e.g. from
#' [get_edmonton_frequent_bus_stops()]).
#'
#' @param data An `sf` object.
#' @param frequent_bus_stops An `sf` object of frequent bus stops.
#'
#' @return The input with a new `distance_from_frequent_bus` numeric column
#'   (km). The column has a `label` attribute for use as a default ggplot2
#'   axis title.
#' @export
add_frequent_bus_distance <- function(data, frequent_bus_stops) {
  add_distance_generic(
    data, frequent_bus_stops,
    distance_col = "distance_from_frequent_bus",
    label = "Distance from closest frequent bus stop (km)"
  )
}

#' Add combined LRT + frequent bus distance column
#'
#' Calculates [add_transit_distance()] and [add_frequent_bus_distance()],
#' then adds `distance_from_frequent_transit`: each row's raw distance (km)
#' to the nearer of an LRT stop or a frequent bus stop. Because LRT and
#' frequent bus stops use different walking thresholds (see
#' [layers_frequent_transit_ecdf()]), this raw distance isn't itself a
#' walkability threshold check; use `distance_from_lrt` and
#' `distance_from_frequent_bus` directly for that (e.g.
#' `distance_from_lrt <= 0.8 | distance_from_frequent_bus <= 0.4`).
#'
#' @param data An `sf` object.
#' @param transit_stops An `sf` object of transit stops (e.g. from
#'   [load_edmonton_transit_stops()]).
#' @param frequent_bus_stops An `sf` object of frequent bus stops (e.g. from
#'   [get_edmonton_frequent_bus_stops()]).
#'
#' @return The input with `distance_from_lrt`, `distance_from_frequent_bus`,
#'   and `distance_from_frequent_transit` numeric columns added. The combined
#'   column has a `label` attribute for use as a default ggplot2 axis title.
#' @export
add_frequent_transit_distance <- function(
  data,
  transit_stops,
  frequent_bus_stops
) {
  data |>
    add_transit_distance(transit_stops) |>
    add_frequent_bus_distance(frequent_bus_stops) |>
    dplyr::mutate(
      distance_from_frequent_transit = structure(
        pmin(.data$distance_from_lrt, .data$distance_from_frequent_bus),
        label = "Distance from nearest LRT or frequent bus stop (km)"
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

#' Compute weighted ECDF by distance
#'
#' Generic core shared by [add_ecdf_by_distance()] and
#' [add_bike_lane_ecdf_by_distance()]. Not exported.
#'
#' @param data A data frame.
#' @param distance_col Name of the distance column to sort by (string).
#' @param group_var <[`data-masking`][dplyr::dplyr_data_masking]> Grouping
#'   variable.
#' @param weight_var <[`data-masking`][dplyr::dplyr_data_masking]> Variable to
#'   accumulate.
#'
#' @return `data` with new columns `cum_units` and `ecdf_values`, ungrouped.
#' @keywords internal
#' @noRd
add_ecdf_by_distance_generic <- function(data, distance_col, group_var, weight_var) {
  data |>
    dplyr::group_by({{ group_var }}) |>
    dplyr::arrange(.data[[distance_col]], .by_group = TRUE) |>
    dplyr::mutate(
      cum_units = cumsum({{ weight_var }}),
      ecdf_values = .data$cum_units / sum({{ weight_var }})
    ) |>
    dplyr::ungroup()
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
add_ecdf_by_distance <- function(
  data,
  group_var = year,
  weight_var = units_added
) {
  add_ecdf_by_distance_generic(
    data,
    distance_col = "distance_from_lrt",
    group_var = {{ group_var }},
    weight_var = {{ weight_var }}
  )
}

#' Compute weighted ECDF by distance from a frequent bus stop
#'
#' Thin wrapper around the same generic ECDF machinery used by
#' [add_ecdf_by_distance()], operating on `distance_from_frequent_bus`
#' instead of `distance_from_lrt`.
#'
#' @inheritParams add_ecdf_by_distance
#' @export
add_frequent_bus_ecdf_by_distance <- function(
  data,
  group_var = year,
  weight_var = units_added
) {
  add_ecdf_by_distance_generic(
    data,
    distance_col = "distance_from_frequent_bus",
    group_var = {{ group_var }},
    weight_var = {{ weight_var }}
  )
}

#' Compute weighted ECDF by combined LRT + frequent bus distance
#'
#' Thin wrapper around the same generic ECDF machinery used by
#' [add_ecdf_by_distance()], operating on `distance_from_frequent_transit`
#' (see [add_frequent_transit_distance()]) instead of `distance_from_lrt`.
#'
#' @inheritParams add_ecdf_by_distance
#' @export
add_frequent_transit_ecdf_by_distance <- function(
  data,
  group_var = year,
  weight_var = units_added
) {
  add_ecdf_by_distance_generic(
    data,
    distance_col = "distance_from_frequent_transit",
    group_var = {{ group_var }},
    weight_var = {{ weight_var }}
  )
}
