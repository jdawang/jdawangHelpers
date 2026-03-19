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
  show_800m_line = TRUE
) {
  layers <- list(
    ggplot2::geom_step(
      ggplot2::aes(
        x = .data$distance_from_lrt,
        y = .data$ecdf_values,
        colour = factor({{ colour_var }})
      ),
      linewidth = 0.8
    ),
    ggplot2::scale_x_continuous(limits = c(0, x_max), oob = scales::squish),
    ggplot2::scale_y_continuous(labels = scales::label_percent()),
    ggplot2::guides(colour = ggplot2::guide_legend(ncol = 2))
  )
  if (show_800m_line) {
    layers <- c(
      layers,
      list(
        ggplot2::geom_vline(xintercept = 0.8, linetype = "dashed"),
        ggplot2::annotate("text", x = 1.3, y = 0.9, label = "800m")
      )
    )
  }
  layers
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
