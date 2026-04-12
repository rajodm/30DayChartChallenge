# Packages ---------------------------------------------------------------

library(tidyverse)
library(terra)
library(tidyterra)
library(tmap)

# Colors -----------------------------------------------------------------

palettes <- list(
  paper = "#171717",
  ink = "#e6e6e4",
  color_sub = "#969693",
  color_muted = "#5e5e5c",
  color_grid = "#272725",
  color_accent = "#89b4a0"
)

# Data -------------------------------------------------------------------

mga_shp <- rnaturalearth::ne_countries(scale = "large", country = "Madagascar")

# # Download the data
walk_time <- malariaAtlas::getRaster(
  "Accessibility__202001_Global_Walking_Only_Travel_Time_To_Healthcare",
  shp = mga_shp
) |>
  janitor::clean_names()

walk_time <- terra::mask(walk_time, mga_shp) |>
  tidyterra::rename(
    walk_time = "walking_only_travel_time_to_nearest_healthcare_facility_without_access_to_motorized_transport"
  )

# Plot --------------------------------------------------------------------

map <- tmap::tm_shape(walk_time) +
  tmap::tm_raster(
    col.scale = tmap::tm_scale_intervals(
      values = cols4all::c4a("carto.blu_yl"),
      style = "fixed",
      breaks = c(0, 10, 60, 180, 600, 1644),
    ),
    col.chart = tmap::tm_chart_bar(
      position = tmap::tm_pos_on_top("right", "center"),
      plot.axis.x = TRUE,
      plot.axis.y = FALSE,
      width = 32,
      height = 36,
      extra.ggplot2 = list(
        ggplot2::scale_x_discrete(
          labels = c(
            "< 10 min",
            "< 60 min",
            "< 180 min",
            "< 600 min",
            "≥ 600 min"
          )
        ),
        ggplot2::labs(
          title = "The long walk to care",
          subtitle = "Distribution of walking travel time to the nearest healthcare facility, Madagascar (2020)"
        ),
        ggplot2::theme_sub_plot(
          background = ggplot2::element_rect(fill = palettes$paper, color = NA),
          title = ggtext::element_textbox_simple(
            color = palettes$ink,
            family = "syne",
            face = "bold",
            size = 20,
            margin = ggplot2::margin(b = 6)
          ),
          subtitle = ggtext::element_textbox_simple(
            color = palettes$color_sub,
            family = "Atkinson Hyperlegible Next",
            size = 12
          )
        ),
        ggplot2::theme_sub_panel(
          background = ggplot2::element_rect(fill = palettes$paper, color = NA),
          grid.major = ggplot2::element_blank(),
          grid.minor = ggplot2::element_blank()
        ) +
          ggplot2::theme_sub_axis_x(
            text = ggplot2::element_text(
              size = 10,
              color = palettes$ink,
              family = "Atkinson Hyperlegible Next"
            )
          )
      )
    ),
    col.legend = tmap::tm_legend_hide()
  ) +
  tmap::tm_credits(
    "Base map: {rnaturalearth} | projection: EPSG:4326",
    color = palettes$color_muted
  ) +
  tmap::tm_credits(
    "Data source: Malaria Atlas Project (malariaatlas.org) - Global walking only travel time to healthcare 2020 | {malariaAtlas}",
    color = palettes$color_muted
  ) +
  tmap::tm_credits(
    "#30DayChartChallenge 2026, Day 11: Distributions - Physical | Graphic: Andriambelo Rajo | #rstats",
    color = palettes$color_muted
  ) +
  tmap::tm_layout(
    main.title.size = 1.17,
    credit.text.size = 0.85,
    inner.margins = c(0, 0, 0, 1.2),
    outer.margins = 0.02,
    bg.color = palettes$paper,
    chart.bg.color = palettes$paper,
    frame = FALSE,
    chart.frame = FALSE,
    text.fontfamily = "Atkinson Hyperlegible Next"
  )

tmap::tmap_save(
  map,
  here::here("2026/charts", "2026_day11-physical.png"),
  device = ragg::agg_png,
  width = 25,
  height = 21,
  units = "cm",
  background = palettes$paper
)
