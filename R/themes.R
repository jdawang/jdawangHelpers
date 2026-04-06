# Internal colour constants — not exported
JD_INK_DARK <- "#d4b0cc" # light mauve — readable on dark backgrounds
JD_INK_LIGHT <- "#663f5f" # brand purple — readable on light backgrounds
JD_PAPER_DARK <- "#1e1e1e"
JD_PAPER_LIGHT <- "#f4eff2"
JD_BORDER_LIGHT <- "#c4a0be"

#' Jacob Dawang's base ggplot2 theme
#'
#' A theme with the viridis magma palette baked in as the default discrete and
#' continuous colour and fill palettes (requires ggplot2 >= 4.0.0). Binned
#' scales inherit the continuous palette. Uses the 4.0.0
#' `ink`/`paper` arguments on the base theme to set geom colour defaults and
#' the panel background blend. Ink is light (`#d4b0cc`) in dark mode and the
#' brand purple (`#663f5f`) in light mode.
#'
#' @param mode One of `"dark"` (default) or `"light"`.
#' @param base_family Base font family. Defaults to `"Source Sans Pro"` to
#'   match the jacobdawang.com website font.
#' @param ... Arguments passed to the underlying base theme
#'   ([ggplot2::theme_dark()] or [ggplot2::theme_light()]).
#'
#' @return A [ggplot2::theme()] object.
#' @export
theme_jd <- function(mode = c("dark", "light"), base_family = "Source Sans Pro", ...) {
  mode <- match.arg(mode)

  if (mode == "dark") {
    base <- ggplot2::theme_dark(
      ink = JD_INK_DARK,
      paper = JD_PAPER_DARK,
      base_family = base_family,
      ...
    )
    border_colour <- JD_INK_DARK
    ink <- JD_INK_DARK
  } else {
    base <- ggplot2::theme_light(
      ink = JD_INK_LIGHT,
      paper = JD_PAPER_LIGHT,
      base_family = base_family,
      ...
    )
    border_colour <- JD_BORDER_LIGHT
    ink <- JD_INK_LIGHT
  }

  base +
    ggplot2::theme(
      panel.border = ggplot2::element_rect(
        colour = border_colour,
        fill = NA,
        linewidth = 0.4
      ),
      palette.colour.discrete = \(n) {
        viridis::magma(n, begin = 0.15, end = 0.85)
      },
      palette.fill.discrete = \(n) viridis::magma(n, begin = 0.15, end = 0.85),
      palette.colour.continuous = scales::colour_ramp(viridis::magma(
        256,
        begin = 0.15,
        end = 0.85
      )),
      palette.fill.continuous = scales::colour_ramp(viridis::magma(
        256,
        begin = 0.15,
        end = 0.85
      )),
      geom = ggplot2::element_geom(ink = ink),
      plot.title = ggplot2::element_text(face = "bold")
    )
}

#' JD map theme
#'
#' Strips axes, ticks, grid lines, and the panel background from [theme_jd()],
#' leaving the panel transparent so tile layers (e.g. `geom_water()`,
#' `geom_roads()`) render correctly. `plot.background` is intentionally *not*
#' overridden, so the mode-appropriate paper colour set by [theme_jd()] shows
#' through automatically (dark in dark mode, light in light mode).
#' Uses `%+replace%` so the overrides apply cleanly; `panel.background` and
#' `panel.border` are overridden explicitly because `%+replace%` does not
#' cascade through the theme inheritance hierarchy.
#'
#' @param mode One of `"dark"` (default) or `"light"`. Passed to [theme_jd()].
#'
#' @return A [ggplot2::theme()] object.
#' @importFrom ggplot2 %+replace%
#' @export
theme_map <- function(mode = c("dark", "light")) {
  mode <- match.arg(mode)
  paper <- if (mode == "dark") JD_PAPER_DARK else JD_PAPER_LIGHT
  theme_jd(mode = mode) %+replace%
    ggplot2::theme(
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      rect = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(fill = paper, colour = NA),
      panel.border = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank()
    )
}
