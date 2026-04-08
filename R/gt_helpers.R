#' Apply Jacob Dawang's theme to a GT table
#'
#' Styles a GT table to coordinate with [theme_jd()]: white (or dark) body
#' with brand-purple column labels and heading, neutral body text, and
#' `"Source Sans Pro"` font. Row striping is on by default.
#'
#' @param data A `gt_tbl` object.
#' @param mode One of `"light"` (default) or `"dark"`.
#' @param add_row_striping Logical; if `TRUE` (default), alternating rows in
#'   the table body are striped.
#'
#' @return A `gt_tbl` object.
#' @export
#'
#' @examples
#' gt::gt(head(mtcars[, 1:4])) |>
#'   gt::tab_header(title = "Motor Trend Cars") |>
#'   opt_stylize_jd()
opt_stylize_jd <- function(data, mode = c("light", "dark"), add_row_striping = TRUE) {
  mode <- match.arg(mode)

  if (mode == "dark") {
    paper <- JD_PAPER_DARK # "#1e1e1e"
    ink <- JD_INK_DARK # "#d4b0cc"
    border <- JD_BORDER_LIGHT # "#c4a0be"
    header_bg <- "#4a2d47"
    hlines <- "#2d2d2d"
    stripe <- "#262626"
    label_text <- JD_INK_DARK
  } else {
    paper <- "#ffffff"
    ink <- NULL # no override — use gt default (near-black)
    border <- JD_BORDER_LIGHT # "#c4a0be"
    header_bg <- JD_INK_LIGHT # "#663f5f" brand purple
    hlines <- "#e0e0e0"
    stripe <- JD_PAPER_LIGHT # "#f4eff2" subtle purple tint
    label_text <- "#ffffff"
  }

  tbl <- data |>
    gt::opt_table_font(font = gt::google_font("Source Sans Pro")) |>
    gt::tab_options(
      # table background & outer borders
      table.background.color = paper,
      table.border.top.color = border,
      table.border.top.style = "solid",
      table.border.bottom.color = border,
      table.border.bottom.style = "solid",
      # heading (title / subtitle)
      heading.background.color = header_bg,
      heading.border.bottom.color = border,
      heading.border.bottom.style = "solid",
      # column labels
      column_labels.background.color = header_bg,
      column_labels.font.weight = "bold",
      column_labels.border.top.color = border,
      column_labels.border.top.style = "solid",
      column_labels.border.bottom.color = border,
      column_labels.border.bottom.style = "solid",
      # table body
      table_body.hlines.color = hlines,
      table_body.hlines.style = "solid",
      table_body.border.bottom.color = border,
      table_body.border.bottom.style = "solid",
      # row striping
      row.striping.background_color = stripe,
      row.striping.include_stub = TRUE,
      row.striping.include_table_body = add_row_striping,
      # source notes & footnotes
      source_notes.font.size = gt::pct(85),
      source_notes.background.color = paper,
      footnotes.background.color = paper
    ) |>
    gt::tab_style(
      style = gt::cell_text(color = label_text, weight = "bold"),
      locations = list(gt::cells_column_labels(), gt::cells_title())
    )

  if (mode == "dark") {
    tbl <- tbl |>
      gt::tab_style(
        style = gt::cell_text(color = ink),
        locations = gt::cells_body()
      )
  }

  tbl
}

#' Finalize a GT table with source note and optional interactivity
#'
#' Adds a source note, replaces missing values with en-dashes, and optionally
#' enables interactive features (search, highlight, sorting).
#'
#' @param gt_tbl A `gt_tbl` object.
#' @param source Character string for the source note. Defaults to
#'   [CAPTION_COE].
#' @param interactive Logical; if `TRUE`, enables `gt::opt_interactive()` with
#'   search, highlight, and sorting. Defaults to `FALSE`.
#'
#' @return A `gt_tbl` object.
#' @export
finalize_gt <- function(gt_tbl, source = CAPTION_COE, interactive = FALSE) {
  gt_tbl <- gt_tbl |>
    gt::tab_source_note(source) |>
    gt::sub_missing()
  if (interactive) {
    gt_tbl <- gt_tbl |>
      gt::opt_interactive(
        use_search = TRUE,
        use_highlight = TRUE,
        use_sorting = TRUE
      )
  }
  gt_tbl
}
