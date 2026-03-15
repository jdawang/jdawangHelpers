#' Add Edmonton neighbourhood type classification
#'
#' Classifies each permit into one of: Downtown, Mature, Between mature and
#' Henday, Outside Henday. Uses two passes:
#' 1. Spatial containment tests against the mature neighbourhood and Henday
#'    boundaries.
#' 2. A name-based fallback for known edge cases that fall outside the spatial
#'    boundaries (e.g. permits geocoded to a road centreline).
#'
#' @param bp An `sf` object of building permits. Must have a `neighbourhood`
#'   character column and point or polygon geometry in the same CRS as
#'   `mature_neighbourhood` and `henday`.
#' @param mature_neighbourhood An `sf` polygon of the mature neighbourhood
#'   boundary. Defaults to the bundled dataset.
#' @param henday An `sf` polygon of the Anthony Henday Drive boundary.
#'   Defaults to the bundled dataset.
#'
#' @return The input with a new `neighbourhood_type` factor column with levels:
#'   `"Downtown"`, `"Mature"`, `"Between mature and Henday"`, `"Outside Henday"`.
#'   The column has a `label` attribute set to `"Neighbourhood type"`.
#' @export
add_edmonton_neighbourhood_type <- function(
    bp,
    mature_neighbourhood = jdawangHelpers::mature_neighbourhood,
    henday               = jdawangHelpers::henday) {

  levels <- c("Downtown", "Mature", "Between mature and Henday", "Outside Henday")

  bp <- bp |> dplyr::mutate(
    .mature  = sf::st_contains(sf::st_union(mature_neighbourhood), sf::st_geometry(dplyr::cur_data()), sparse = FALSE)[1, ] &
               !sf::st_is_empty(sf::st_geometry(dplyr::cur_data())) &
               !is.na(.data$neighbourhood),
    .outside = !sf::st_contains(henday, sf::st_geometry(dplyr::cur_data()), sparse = FALSE)[1, ] &
               !sf::st_is_empty(sf::st_geometry(dplyr::cur_data())),
    .between = !sf::st_is_empty(sf::st_geometry(dplyr::cur_data())) & !.data$.mature & !.data$.outside,
    neighbourhood_type = forcats::fct(
      dplyr::case_when(
        .data$neighbourhood == "DOWNTOWN" ~ "Downtown",
        .data$.mature                     ~ "Mature",
        .data$.outside                    ~ "Outside Henday",
        .data$.between                    ~ "Between mature and Henday",
        .default = NA_character_
      ),
      levels = levels
    )
  ) |>
    dplyr::select(-".mature", -".outside", -".between")

  # Second pass: name-based fallback for permits not captured by spatial test
  outside_henday_names <- c(
    "TRUMPETER", "STARLING", "KINGLET LAKES", "STONE CREEK",
    "EDGEMONT", "DESROCHERS AREA", "RIVERVIEW AREA"
  )
  mature_names <- c("CALLINGWOOD NORTH", "CALLINGWOOD SOUTH")

  bp |> dplyr::mutate(
    neighbourhood_type = structure(
      dplyr::case_when(
        !is.na(.data$neighbourhood_type) ~ as.character(.data$neighbourhood_type),
        .data$neighbourhood %in% outside_henday_names ~ "Outside Henday",
        .data$neighbourhood %in% mature_names ~ "Mature",
        .default = as.character(.data$neighbourhood_type)
      ) |> forcats::fct(levels = levels),
      label = "Neighbourhood type"
    )
  )
}
