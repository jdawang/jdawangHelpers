# jdawangHelpers

Shared utility functions for data processing and visualization in blog posts at [jacobdawang.com](https://jacobdawang.com).

## Overview

This R package provides helper functions for:
- **Edmonton building permits**: Data cleaning, enrichment, and filtering
- **Transit distance calculations**: Computing distances to transit stops
- **Neighbourhood classification**: Adding neighbourhood type attributes
- **ggplot2 themes**: Custom dark theme and map theme
- **GT table helpers**: Enhanced table formatting utilities

## Installation

Install from GitHub:

```r
# Install devtools if needed
install.packages("devtools")

devtools::install_github("jdawang/jdawangHelpers")
```

Or clone and install locally:

```r
# In R
renv::restore()          # Restore dependencies
devtools::install()      # Install package
```

## Quick Start

Most functions follow a pipeline pattern for processing Edmonton building permit data:

```r
library(jdawangHelpers)

# Typical workflow
building_permits |>
  clean_edmonton_bp_columns(crs = 4326) |>
  filter_edmonton_residential() |>
  add_edmonton_project_type() |>
  add_edmonton_suite_info() |>
  add_edmonton_neighbourhood_type() |>
  add_transit_distance(transit_stops)
```

## Development

### Setup

```r
# In an R session
renv::restore()          # Install dependencies
devtools::load_all()     # Load all functions
```

### Common Tasks

```r
devtools::document()     # Regenerate docs from roxygen2 comments
devtools::check()        # Run R CMD check
devtools::test()         # Run tests (if any)
renv::snapshot()         # Update renv.lock after installing new packages
```

### Architecture Notes

- **Column labels**: Many `add_*` functions attach `label` attributes to columns for automatic ggplot2/GT formatting
- **Themes**: `theme_jd()` provides a dark base theme; `theme_map()` strips axes; combine with `theme_map_dark()`
- **Bundled data**: `mature_neighbourhood` and `henday` polygons (regenerated via `data-raw/`)
- **Dependencies**: `mountainmathHelpers` (GitHub-only) provides `geom_water()` and `geom_roads()` in `layers_map_base()`

## License

MIT License — see [LICENSE](LICENSE) file
