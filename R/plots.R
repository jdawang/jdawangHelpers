#' ECDF step plot layers by distance
#'
#' Generic core shared by [layers_transit_ecdf()] and
#' [layers_bike_lane_ecdf()]. Not exported.
#'
#' @param distance_col Name of the distance column to plot on the x axis
#'   (string).
#' @param colour_var <[`data-masking`][ggplot2::aes]> Variable to colour by.
#' @param x_max Numeric upper limit for the x axis (km).
#' @param show_ref_line Logical; if `TRUE` adds a dashed vertical reference
#'   line and label.
#' @param ref_line_km Numeric x-intercept (km) for the reference line.
#' @param ref_line_label Character label for the reference line.
#' @param ref_line_label_y Numeric y position (0-1) for the reference line
#'   label.
#'
#' @return A list of ggplot2 layers.
#' @keywords internal
#' @noRd
layers_ecdf_generic <- function(
  distance_col,
  colour_var,
  x_max,
  show_ref_line,
  ref_line_km,
  ref_line_label,
  ref_line_label_y
) {
  layers <- list(
    ggplot2::geom_step(
      ggplot2::aes(
        x = .data[[distance_col]],
        y = .data$ecdf_values,
        colour = factor({{ colour_var }})
      ),
      linewidth = 0.8
    ),
    ggplot2::scale_x_continuous(limits = c(0, x_max), oob = scales::squish),
    ggplot2::scale_y_continuous(labels = scales::label_percent()),
    ggplot2::guides(colour = ggplot2::guide_legend(ncol = 2))
  )
  if (show_ref_line) {
    layers <- c(
      layers,
      list(
        ggplot2::geom_vline(xintercept = ref_line_km, linetype = "dashed"),
        ggplot2::annotate(
          "text",
          x = ref_line_km + 0.5,
          y = ref_line_label_y,
          label = ref_line_label
        )
      )
    )
  }
  layers
}

#' ECDF step plot layers for transit distance
#'
#' Returns a list of ggplot2 layers that plot a cumulative distribution of
#' units/permits by distance from LRT, coloured by a grouping variable.
#' Designed to be added to a `ggplot()` call alongside `theme_jd()`.
#'
#' @param colour_var <[`data-masking`][ggplot2::aes]> Variable to colour by.
#'   Defaults to `year`.
#' @param x_max Numeric upper limit for the x axis (km). Defaults to 14.
#' @param show_800m_line Logical; if `TRUE` (default) adds a dashed vertical
#'   line and label at 800 m.
#' @param ref_line_label_y Numeric y position (0-1) for the reference line
#'   label. Defaults to 0.9.
#'
#' @return A list of ggplot2 layers.
#'
#' @examples
#' \dontrun{
#' ggplot(ecdf_data) +
#'   layers_transit_ecdf() +
#'   theme_jd() +
#'   labs(title = "Cumulative units by LRT distance", caption = CAPTION_COE)
#' }
#' @export
layers_transit_ecdf <- function(
  colour_var = year,
  x_max = 14,
  show_800m_line = TRUE,
  ref_line_label_y = 0.9
) {
  layers_ecdf_generic(
    distance_col = "distance_from_lrt",
    colour_var = {{ colour_var }},
    x_max = x_max,
    show_ref_line = show_800m_line,
    ref_line_km = 0.8,
    ref_line_label = "800m",
    ref_line_label_y = ref_line_label_y
  )
}

#' ECDF step plot layers for bike infrastructure distance
#'
#' Returns a list of ggplot2 layers that plot a cumulative distribution of
#' units/permits by distance from bike infrastructure, coloured by a grouping
#' variable. Mirrors [layers_transit_ecdf()]; expects data prepared with
#' [add_bike_lane_distance()] and [add_bike_lane_ecdf_by_distance()].
#'
#' @param colour_var <[`data-masking`][ggplot2::aes]> Variable to colour by.
#'   Defaults to `year`.
#' @param x_max Numeric upper limit for the x axis (km). Defaults to 3.
#' @param show_250m_line Logical; if `TRUE` (default) adds a dashed vertical
#'   line and label at 250 m.
#' @param ref_line_label_y Numeric y position (0-1) for the reference line
#'   label. Defaults to 0.9.
#'
#' @return A list of ggplot2 layers.
#' @export
layers_bike_lane_ecdf <- function(
  colour_var = year,
  x_max = 3,
  show_250m_line = TRUE,
  ref_line_label_y = 0.9
) {
  layers_ecdf_generic(
    distance_col = "distance_from_bike_infra",
    colour_var = {{ colour_var }},
    x_max = x_max,
    show_ref_line = show_250m_line,
    ref_line_km = 0.25,
    ref_line_label = "250m",
    ref_line_label_y = ref_line_label_y
  )
}

#' ECDF step plot layers for frequent bus stop distance
#'
#' Returns a list of ggplot2 layers that plot a cumulative distribution of
#' units/permits by distance from a frequent bus stop, coloured by a
#' grouping variable. Mirrors [layers_transit_ecdf()]; expects data prepared
#' with [add_frequent_bus_distance()] and
#' [add_frequent_bus_ecdf_by_distance()].
#'
#' @param colour_var <[`data-masking`][ggplot2::aes]> Variable to colour by.
#'   Defaults to `year`.
#' @param x_max Numeric upper limit for the x axis (km). Defaults to 3.
#' @param show_400m_line Logical; if `TRUE` (default) adds a dashed vertical
#'   line and label at 400 m.
#' @param ref_line_label_y Numeric y position (0-1) for the reference line
#'   label. Defaults to 0.9.
#'
#' @return A list of ggplot2 layers.
#' @export
layers_frequent_bus_ecdf <- function(
  colour_var = year,
  x_max = 3,
  show_400m_line = TRUE,
  ref_line_label_y = 0.9
) {
  layers_ecdf_generic(
    distance_col = "distance_from_frequent_bus",
    colour_var = {{ colour_var }},
    x_max = x_max,
    show_ref_line = show_400m_line,
    ref_line_km = 0.4,
    ref_line_label = "400m",
    ref_line_label_y = ref_line_label_y
  )
}

#' ECDF step plot layers for combined LRT + frequent bus distance ratio
#'
#' Returns a list of ggplot2 layers that plot a cumulative distribution of
#' units/permits by `distance_from_frequent_transit_ratio` (distance to the
#' nearer of LRT or a frequent bus stop, as a share of that mode's own
#' walking threshold), coloured by a grouping variable. Mirrors
#' [layers_transit_ecdf()]; expects data prepared with
#' [add_frequent_transit_distance()] and
#' [add_frequent_transit_ecdf_by_distance()].
#'
#' @param colour_var <[`data-masking`][ggplot2::aes]> Variable to colour by.
#'   Defaults to `year`.
#' @param x_max Numeric upper limit for the x axis (ratio). Defaults to 3.
#' @param show_threshold_line Logical; if `TRUE` (default) adds a dashed
#'   vertical line and label at a ratio of 1 (i.e. at the relevant walking
#'   threshold).
#' @param ref_line_label_y Numeric y position (0-1) for the reference line
#'   label. Defaults to 0.9.
#'
#' @return A list of ggplot2 layers.
#' @export
layers_frequent_transit_ecdf <- function(
  colour_var = year,
  x_max = 3,
  show_threshold_line = TRUE,
  ref_line_label_y = 0.9
) {
  layers_ecdf_generic(
    distance_col = "distance_from_frequent_transit_ratio",
    colour_var = {{ colour_var }},
    x_max = x_max,
    show_ref_line = show_threshold_line,
    ref_line_km = 1,
    ref_line_label = "At threshold\n(800m LRT / 400m bus)",
    ref_line_label_y = ref_line_label_y
  )
}

#' Map base layers (water + roads)
#'
#' Returns a list of ggplot2 layers adding water and road context to a map.
#' Requires the `mountainmathHelpers` package (listed in `Suggests`).
#'
#' @param roads_type `"major"` (default) to show only highways and major roads,
#'   or `"all"` to show all roads at reduced opacity.
#' @param mode One of `"dark"` (default) or `"light"`. Controls the default
#'   road colour: light grey (`"#aaaaaa"`) in dark mode, dark grey (`"#444444"`)
#'   in light mode. Ignored when `roads_colour` is supplied.
#' @param roads_colour Road line colour. Overrides the `mode` default.
#' @param roads_alpha Road line alpha (opacity). Defaults to `0.5`.
#'
#' @return A list of two ggplot2 layers.
#'
#' @examples
#' \dontrun{
#' ggplot(data) +
#'   layers_map_base(mode = "dark") +
#'   geom_sf(aes(colour = units_added)) +
#'   theme_map()
#' }
#' @export
layers_map_base <- function(
  roads_type = c("major", "all"),
  mode = c("dark", "light"),
  roads_colour = NULL,
  roads_alpha = 0.5
) {
  if (!requireNamespace("mountainmathHelpers", quietly = TRUE)) {
    stop("Package 'mountainmathHelpers' is required for layers_map_base().")
  }
  mode <- match.arg(mode)
  if (is.null(roads_colour)) {
    roads_colour <- if (mode == "dark") "#aaaaaa" else "#444444"
  }
  roads_layer <- if (match.arg(roads_type) == "major") {
    mountainmathHelpers::geom_roads(
      transform = function(d) {
        dplyr::filter(d, .data$kind %in% c("highway", "major_road"))
      },
      color = roads_colour,
      alpha = roads_alpha
    )
  } else {
    mountainmathHelpers::geom_roads(alpha = roads_alpha, color = roads_colour)
  }
  list(mountainmathHelpers::geom_water(), roads_layer)
}
