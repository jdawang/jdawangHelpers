# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Package overview

`jdawangHelpers` is an R package of shared utilities for blog posts at jacobdawang.com. It covers Edmonton building permit processing, transit distance calculations, neighbourhood classification, ggplot2 themes, and GT table helpers.

## Common commands

All standard R package development uses `devtools` (run from an R session):

```r
renv::restore()          # restore dependencies
devtools::load_all()     # load all package functions into R session
devtools::document()     # regenerate NAMESPACE and man/ from roxygen2 comments
devtools::check()        # run R CMD check (includes examples)
devtools::install()      # install the package locally
renv::snapshot()         # generate the renv.lock file - always run after installing new packages
```

There are no automated tests (no `tests/` directory).

## Architecture

### Data flow pattern

Most functions follow a pipeline pattern: raw Edmonton Open Data building permits (an `sf` object) → cleaning → enrichment → visualization. Typical pipeline:

```r
bp |>
  clean_edmonton_bp_columns(crs = 4326) |>
  filter_edmonton_residential() |>
  add_edmonton_project_type() |>
  add_edmonton_suite_info() |>
  add_edmonton_neighbourhood_type() |>
  add_transit_distance(transit_stops)
```

### Bundled datasets

Two spatial datasets are bundled in `data/` (not in git for now):
- `mature_neighbourhood` — City of Edmonton mature neighbourhood boundary polygon
- `henday` — Anthony Henday Drive boundary polygon

These are used as defaults in `add_edmonton_neighbourhood_type()`. Regenerate via `data-raw/mature_neighbourhood.R` and `data-raw/henday.R`.

### Column labelling convention

Several `add_*` functions attach a `label` attribute to new columns (e.g., `attr(col, "label") = "Project type"`). This allows ggplot2 to automatically use the label as the axis/legend title without extra `labs()` calls.

### Theme system

- `theme_jd()` — dark base theme; sets viridis magma as default discrete palette and `#663f5f` as default geom ink colour (requires ggplot2 >= 4.0.0)
- `theme_map()` — strips axes/grid from any base theme
- `theme_map_dark()` — convenience wrapper combining both

### Transit ECDF workflow

`layers_transit_ecdf()` expects data pre-processed by `add_transit_distance()` + `add_ecdf_by_distance()`, with columns `distance_from_lrt` and `ecdf_values`.

### External dependency: mountainmathHelpers

`layers_map_base()` requires the `mountainmathHelpers` package (GitHub-only, listed in `Suggests`). It provides `geom_water()` and `geom_roads()`. The function hard-fails if the package is absent.

### Documentation

All exported functions and constants are documented with roxygen2. Run `devtools::document()` after any `@` tag changes; never edit `NAMESPACE` or `man/` by hand.
