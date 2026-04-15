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
