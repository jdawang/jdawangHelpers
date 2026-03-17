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
#'   character column. If `bp` is in a different CRS than `mature_neighbourhood`
#'   or `henday`, the boundaries are automatically transformed to match `bp`.
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
  henday = jdawangHelpers::henday
) {
  levels <- c(
    "Downtown",
    "Mature",
    "Between mature and Henday",
    "Outside Henday"
  )

  bp_crs <- sf::st_crs(bp)
  mature_neighbourhood <- sf::st_transform(mature_neighbourhood, bp_crs)
  henday <- sf::st_transform(henday, bp_crs)

  bp_geom <- sf::st_geometry(bp)

  bp <- bp |>
    dplyr::mutate(
      .mature = sf::st_contains(
        sf::st_union(mature_neighbourhood),
        bp_geom,
        sparse = FALSE
      )[1, ] &
        !sf::st_is_empty(bp_geom) &
        !is.na(.data$neighbourhood),
      .outside = !sf::st_contains(henday, bp_geom, sparse = FALSE)[1, ] &
        !sf::st_is_empty(bp_geom),
      .between = !sf::st_is_empty(bp_geom) & !.data$.mature & !.data$.outside,
      neighbourhood_type = forcats::fct(
        dplyr::case_when(
          .data$neighbourhood == "DOWNTOWN" ~ "Downtown",
          .data$.mature ~ "Mature",
          .data$.outside ~ "Outside Henday",
          .data$.between ~ "Between mature and Henday",
          .default = NA_character_
        ),
        levels = levels
      )
    ) |>
    dplyr::select(-".mature", -".outside", -".between")

  # Second pass: name-based fallback for permits not captured by spatial test
  outside_henday_names <- c(
    "TRUMPETER",
    "STARLING",
    "KINGLET LAKES",
    "STONE CREEK",
    "EDGEMONT",
    "DESROCHERS AREA",
    "RIVERVIEW AREA",
    "THE HAMPTONS, GRANVILLE",
    "RUTHERFORD, RUTHERFORD",
    "THE UPLANDS, RIVER'S EDGE",
    "ELLERSLIE INDUSTRIAL"
  )
  between_henday_mature_names <- c(
    "ELSINORE, ELSINORE",
    "MICHAELS PARK",
    "BRANDER GARDENS",
    "PILOT SOUND AREA WEST PORTION, MCCONACHIE",
    "YOUNGSTOWN INDUSTRIAL"
  )
  mature_names <- c(
    "CALLINGWOOD NORTH",
    "CALLINGWOOD SOUTH",
    "CENTRAL MCDOUGALL, QUEEN MARY PARK",
    "WOODCROFT",
    "BOYLE STREET",
    "ELMWOOD"
  )

  bp <- bp |>
    dplyr::mutate(
      neighbourhood_type = structure(
        dplyr::case_when(
          !is.na(.data$neighbourhood_type) ~ as.character(
            .data$neighbourhood_type
          ),
          .data$neighbourhood %in% outside_henday_names ~ "Outside Henday",
          .data$neighbourhood %in%
            between_henday_mature_names ~ "Between mature and Henday",
          .data$neighbourhood %in% mature_names ~ "Mature",
          .default = as.character(.data$neighbourhood_type)
        ) |>
          forcats::fct(levels = levels),
        label = "Neighbourhood type"
      )
    )

  # Third pass: address-level overrides (only when `address` column is present)
  address_mature <- c(
    "8944 - 145 STREET NW",
    "11216 - 122 STREET NW",
    "9509 - 99B STREET NW"
  )
  if ("address" %in% names(bp)) {
    bp <- bp |>
      dplyr::mutate(
        neighbourhood_type = structure(
          dplyr::case_when(
            .data$address %in% address_mature ~ "Mature",
            .default = as.character(.data$neighbourhood_type)
          ) |>
            forcats::fct(levels = levels),
          label = "Neighbourhood type"
        )
      )
  }
  bp
}
