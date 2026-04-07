# Packages ---------------------------------------------------------------

library(tidyverse)
library(rnaturalearth)
library(terra)
library(tidyterra)
library(tmap)
library(cols4all)

# Colors ---------------------------------------------------------

palettes <- list(
  paper = "#f8f8f8",
  ink = "#1a1a1a",
  color_sub = "#3d3d3d",
  color_muted = "#787878",
  color_grid = "#e2e2e2",
  color_accent = "#e63946"
)

# Data -------------------------------------------------------------------

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") |>
  dplyr::filter_out(name == "Antarctica")

# pf_rast <- terra::rast(
#   "data26/malariaAtlas/Malaria__202508_Global_Pf_Incidence_Rate_latest__2026_04_02.tiff"
# )

pf_mean <- (pf_rast[[1]] * 1000) |>
  tidyterra::rename(
    incidence = `Malaria__202508_Global_Pf_Incidence_Rate_2024-01-01T00_00_00_1`
  )

# Plot -------------------------------------------------------------------

tm_setting <-
  tmap::tm_crs(4326) +
  tmap::tm_layout(
    legend.text.size = 0.85,
    legend.title.size = 0.95,
    credit.text.size = 0.85,
    legend.title.fontface = "bold",
    legend.bg.color = palettes$paper,
    legend.text.color = palettes$ink,
    bg.color = palettes$paper,
    main.title.size = 1.17,
    main.title.position = tmap::tm_pos_on_top("left", "top", just.h = "left"),
    outer.margins = 0,
    inner.margins = c(0, 0.06, 0, 0.06),
    frame = FALSE,
    legend.frame = FALSE,
    text.fontfamily = "Atkinson Hyperlegible Next",
  )

base_map <- tmap::tm_shape(world) +
  tmap::tm_borders(lwd = 0.4, col = NA) +
  tmap::tm_crs(4326)

tm_equal <-
  tmap::tm_shape(pf_mean) +
  tmap::tm_raster(
    col.scale = tmap::tm_scale_intervals(
      style = "equal",
      values = cols4all::c4a(
        "carto.ag_sunset",
        reverse = TRUE
      )
    ),
    col.legend = tmap::tm_legend(
      title = "Cases per 1,000\n(Equal intervals)",
      position = tmap::tm_pos_in("left", "bottom"),
      show = TRUE
    ),
    col_alpha = 0.8,
  ) +
  tmap::tm_crs(4326) +
  base_map +
  tmap::tm_title(
    "Plasmodium falciparum incidence rate, 2024: two classification approaches",
    color = palettes$ink,
    fontface = "bold",
    fontfamily = "syne",
  ) +
  tmap::tm_title(
    stringr::str_wrap(
      "The equal intervals map (top) divides the incidence range into 5 classes of equal width. The map at the bottom uses the Fisher (1958) algorithm to find optimal breaks that minimize within-class variance.",
      120
    ),
    color = palettes$color_sub,
    fontfamily = "Atkinson Hyperlegible Next",
    size = 1
  ) +
  tm_setting

tm_fisher <-
  tmap::tm_shape(pf_mean) +
  tmap::tm_raster(
    col.scale = tmap::tm_scale_intervals(
      style = "fisher",
      values = cols4all::c4a(
        "carto.ag_sunset",
        reverse = TRUE
      )
    ),
    col.legend = tmap::tm_legend(
      title = "Cases per 1,000\n(Fisher algorithm)",
      position = tmap::tm_pos_in("left", "bottom"),
      show = TRUE
    ),
    col_alpha = 0.8
  ) +
  tmap::tm_crs(4326) +
  base_map +
  tmap::tm_credits(
    "Base map: {rnaturalearth} | projection: EPSG:4326",
    color = palettes$color_muted
  ) +
  tmap::tm_credits(
    "Data source: Malaria Atlas Project (malariaatlas.org) - Malaria 2025-08 Global Pf Incidence Rate | {malariaAtlas}",
    color = palettes$color_muted
  ) +
  tmap::tm_credits(
    "#30DayChartChallenge 2026, Day 07: Distribution - Multiscale | Graphic: Andriambelo Rajo | #rstats",
    color = palettes$color_muted
  ) +
  tm_setting

tm <- tmap::tmap_arrange(
  tm_equal,
  tm_fisher,
  ncol = 1,
  outer.margins = 0.015
)

tmap::tmap_save(
  tm,
  here::here("2026/charts", "2026_day07-multiscale.png"),
  device = ragg::agg_png,
  dpi = 600,
  width = 25,
  height = 25,
  units = "cm",
  background = palettes$paper
)
