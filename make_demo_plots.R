## Demo plots for theme_jd() and theme_map()
## Run from the package root: Rscript make_demo_plots.R

devtools::load_all()
library(ggplot2)
library(sf)
library(gridExtra)
options(nextzen_API_key = Sys.getenv("NEXTZEN_API_KEY"))

# ── theme_jd demo ─────────────────────────────────────────────────────────────
# Uses iris: 3 species → shows the magma discrete palette
# Two panels: dark (default) and light mode

make_jd_plot <- function(mode) {
  title_suffix <- if (mode == "dark") "dark mode" else "light mode"
  ggplot(
    iris,
    aes(x = Sepal.Length, y = Petal.Length, colour = Species, fill = Species)
  ) +
    geom_point(size = 2.5, alpha = 0.8, shape = 21, stroke = 0.5) +
    geom_smooth(method = "lm", se = TRUE, alpha = 0.2, linewidth = 0.8) +
    labs(
      title = paste0("theme_jd() — ", title_suffix),
      subtitle = "Iris sepal vs petal length by species",
      x = "Sepal length (cm)",
      y = "Petal length (cm)",
      colour = "Species",
      fill = "Species",
      caption = "Magma viridis discrete palette · panel border · ink/paper colours"
    ) +
    theme_jd(mode = mode)
}

p_dark <- make_jd_plot("dark")
p_light <- make_jd_plot("light")

png("theme_jd_demo.png", width = 1400, height = 600, res = 120)
grid.arrange(p_dark, p_light, ncol = 2)
dev.off()
message("Saved theme_jd_demo.png")

# ── theme_jd continuous palette demo ──────────────────────────────────────────
# Uses mtcars: hp as continuous colour → shows the magma continuous palette
# Two panels: dark (default) and light mode

make_jd_continuous_plot <- function(mode) {
  title_suffix <- if (mode == "dark") "dark mode" else "light mode"
  ggplot(mtcars, aes(x = wt, y = mpg, colour = hp)) +
    geom_point(size = 3, alpha = 0.9) +
    labs(
      title = paste0("theme_jd() continuous — ", title_suffix),
      subtitle = "Motor Trend cars: weight vs fuel efficiency",
      x = "Weight (1 000 lbs)",
      y = "Miles per gallon",
      caption = "Magma viridis continuous palette"
    ) +
    theme_jd(mode = mode)
}

c_dark <- make_jd_continuous_plot("dark")
c_light <- make_jd_continuous_plot("light")

png("theme_jd_continuous_demo.png", width = 1400, height = 600, res = 120)
grid.arrange(c_dark, c_light, ncol = 2)
dev.off()
message("Saved theme_jd_continuous_demo.png")

# ── theme_jd continuous fill demo ─────────────────────────────────────────────
# Uses diamonds: carat vs price binned → shows the magma continuous fill palette
# Two panels: dark (default) and light mode

make_jd_fill_plot <- function(mode) {
  title_suffix <- if (mode == "dark") "dark mode" else "light mode"
  ggplot(diamonds, aes(x = carat, y = price)) +
    geom_bin2d(bins = 50) +
    labs(
      title = paste0("theme_jd() continuous fill — ", title_suffix),
      subtitle = "Diamond carat vs price density",
      x = "Carat",
      y = "Price (USD)",
      caption = "Magma viridis continuous fill palette · geom_bin2d"
    ) +
    theme_jd(mode = mode)
}

f_dark <- make_jd_fill_plot("dark")
f_light <- make_jd_fill_plot("light")

png("theme_jd_fill_demo.png", width = 1400, height = 600, res = 120)
grid.arrange(f_dark, f_light, ncol = 2)
dev.off()
message("Saved theme_jd_fill_demo.png")

# ── theme_map demo ─────────────────────────────────────────────────────────────
# Uses the built-in North Carolina shapefile from the sf package
# Two panels: dark and light mode; choropleth of SIDS rate

nc <- st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
nc$sids_rate <- nc$SID74 / nc$BIR74 * 1000 # SID deaths per 1 000 births

make_map_plot <- function(mode) {
  title_suffix <- if (mode == "dark") "dark mode" else "light mode"
  ggplot(nc) +
    geom_sf(aes(fill = sids_rate), colour = NA) +
    labs(
      title = paste0("theme_map() — ", title_suffix),
      subtitle = "North Carolina SIDS rate by county, 1974–78",
      caption = "No axes · no ticks · no grid · paper background from theme_jd()"
    ) +
    theme_map(mode = mode)
}

m_dark <- make_map_plot("dark")
m_light <- make_map_plot("light")

png("theme_map_demo.png", width = 1400, height = 600, res = 120)
grid.arrange(m_dark, m_light, ncol = 2)
dev.off()
message("Saved theme_map_demo.png")

# ── theme_map discrete fill demo ──────────────────────────────────────────────
# Bins sids_rate into quartile categories → shows the magma discrete fill palette
# Two panels: dark and light mode

nc$sids_cat <- cut(
  nc$sids_rate,
  breaks = quantile(nc$sids_rate, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE),
  labels = c("Low", "Medium", "High", "Very high"),
  include.lowest = TRUE
)

make_map_discrete_plot <- function(mode) {
  title_suffix <- if (mode == "dark") "dark mode" else "light mode"
  ggplot(nc) +
    geom_sf(aes(fill = sids_cat), colour = NA) +
    labs(
      title = paste0("theme_map() discrete fill — ", title_suffix),
      subtitle = "North Carolina SIDS rate by county (quartile groups), 1974–78",
      fill = "SIDS rate",
      caption = "Magma viridis discrete palette · geom_sf"
    ) +
    theme_map(mode = mode)
}

md_dark <- make_map_discrete_plot("dark")
md_light <- make_map_discrete_plot("light")

png("theme_map_discrete_demo.png", width = 1400, height = 600, res = 120)
grid.arrange(md_dark, md_light, ncol = 2)
dev.off()
message("Saved theme_map_discrete_demo.png")

# ── theme_map binned fill demo ─────────────────────────────────────────────────
# Uses scale_fill_binned() → shows the magma binned fill default
# Two panels: dark and light mode

make_map_binned_plot <- function(mode) {
  ggplot(nc) +
    geom_sf(aes(fill = sids_rate), colour = NA) +
    scale_fill_binned() +
    labs(
      title = paste0(
        "theme_map() binned fill — ",
        if (mode == "dark") "dark mode" else "light mode"
      ),
      subtitle = "North Carolina SIDS rate by county, 1974–78 (binned scale)",
      caption = "Magma viridis binned fill palette · scale_fill_binned()"
    ) +
    theme_map(mode = mode)
}

mb_dark <- make_map_binned_plot("dark")
mb_light <- make_map_binned_plot("light")

png("theme_map_binned_demo.png", width = 1400, height = 600, res = 120)
grid.arrange(mb_dark, mb_light, ncol = 2)
dev.off()
message("Saved theme_map_binned_demo.png")

# ── theme_map with geom_water / geom_roads (Edmonton) ─────────────────────────
# Validates that theme_map() leaves backgrounds transparent so mountainmath
# tile layers (water + roads) are visible.  Uses the bundled henday polygon as
# the data extent so the tile fetch is centred on Edmonton.
# Requires: mountainmathHelpers, jdawangHelpers bundled data

if (requireNamespace("mountainmathHelpers", quietly = TRUE)) {
  library(mountainmathHelpers)

  # Use the Henday boundary (bundled) as the bounding geometry
  edmonton_extent <- sf::st_transform(henday, 4326)

  make_edmonton_map <- function(mode) {
    ggplot(edmonton_extent) +
      layers_map_base(roads_type = "all", mode = mode) +
      geom_sf(fill = NA, colour = "firebrick", linewidth = 0.8) +
      theme_map(mode = mode) +
      labs(
        title = paste0("theme_map() + layers_map_base() — ", mode, " mode"),
        subtitle = "Edmonton (Anthony Henday boundary); water + roads should be visible",
        caption = "mountainmathHelpers tiles · jdawangHelpers::theme_map() + layers_map_base()"
      )
  }

  em_dark <- make_edmonton_map("dark")
  em_light <- make_edmonton_map("light")

  png("theme_map_edmonton_demo.png", width = 1400, height = 700, res = 120)
  grid.arrange(em_dark, em_light, ncol = 2)
  dev.off()
  message("Saved theme_map_edmonton_demo.png")
} else {
  message("Skipping Edmonton tile demo — mountainmathHelpers not available")
}
