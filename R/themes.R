#' Jacob Dawang's base ggplot2 theme
#'
#' A dark theme with the viridis magma palette baked in as the default discrete
#' colour and fill palette (requires ggplot2 >= 4.0.0). Also sets the default
#' ink colour for unmapped geoms to `#663f5f`, eliminating the need to hardcode
#' that value in every `geom_*()` call.
#'
#' @param ... Arguments passed to [ggplot2::theme_dark()].
#'
#' @return A [ggplot2::theme()] object.
#' @export
theme_jd <- function(...) {
  ggplot2::theme_dark(...) +
    ggplot2::theme(
      palette.colour.discrete = \(n) viridis::magma(n),
      palette.fill.discrete   = \(n) viridis::magma(n),
      geom = ggplot2::element_geom(ink = "#663f5f")
    )
}

#' Clean map theme
#'
#' Strips axes, ticks, background rect, and grid lines from a base theme.
#' Uses `%+replace%` so overrides apply cleanly on top of the base.
#'
#' @param base_theme A ggplot2 theme to use as the base. Defaults to
#'   [ggplot2::theme_void()].
#'
#' @return A [ggplot2::theme()] object.
#' @export
theme_map <- function(base_theme = ggplot2::theme_void()) {
  base_theme %+replace%
    ggplot2::theme(
      axis.text  = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      rect       = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank()
    )
}

#' Dark map theme
#'
#' Convenience wrapper combining [theme_map()] on top of [theme_jd()].
#' Gives a dark background with no axes and the magma colour palette.
#'
#' @return A [ggplot2::theme()] object.
#' @export
theme_map_dark <- function() theme_map(theme_jd())
