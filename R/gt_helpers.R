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
