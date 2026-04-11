# Packages ---------------------------------------------------------------

library(tidyverse)
library(sf)
library(biscale)
library(patchwork)

# Colors & Theme ---------------------------------------------------------

palettes <- list(
  paper = "#171717",
  ink = "#e6e6e4",
  color_sub = "#969693",
  color_muted = "#5e5e5c",
  color_grid = "#272725",
  color_accent = "#89b4a0"
)

color_blue <- "#d6e4ec"
color_border <- "#7a7d84"
color_gray <- "#d4d6da"

source("theme_30dcc.R")

pal_mat <- cols4all::c4a(
  "bivario.kaleidoscope",
  n = 4,
  reverse = TRUE,
  type = "bivc"
)

pal <- pal_mat |>
  as.vector() |>
  setNames(
    paste0(
      rep(1:nrow(pal_mat), times = ncol(pal_mat)),
      "-",
      rep(1:ncol(pal_mat), each = nrow(pal_mat))
    )
  )
# Data -------------------------------------------------------------------

nga_admin1 <- geodata::gadm("NGA", path = "data26/gadm/") |>
  janitor::clean_names() |>
  sf::st_as_sf(crs = sf::st_crs(4326))

nga <- nga_admin1 |>
  dplyr::mutate(
    name = dplyr::coalesce(varname_1, name_1),
    name = dplyr::replace_values(
      name,
      "Nasarawa" ~ "Nassarawa"
    )
  )

# # Data available on malariaatlas.org
# nigeria_itn <- readr::read_csv(here::here(
#   "data26/malariaAtlas/nigeria-itn-subnational_unit_data-2024.csv"
# )) |>
#   janitor::clean_names()

nigeria_itn_wide <- nigeria_itn |>
  tidyr::pivot_wider(
    names_from = metric,
    values_from = value
  ) |>
  janitor::clean_names()

nga_data <- nga |>
  dplyr::left_join(nigeria_itn_wide, by = dplyr::join_by(name == name))

nga_biv_data <- nga_data |>
  biscale::bi_class(
    x = access,
    y = use,
    style = "quantile",
    dim = 4
  )

legend_breaks <- nga_biv_data |>
  biscale::bi_class_breaks(
    x = access,
    y = use,
    style = "quantile",
    dim = 4,
    dig_lab = c(x = 3, y = 3),
    si_levels = FALSE,
    split = TRUE
  )


# Grids ------------------------------------------------------------------

grid <- sf::st_make_grid(
  nga_biv_data,
  n = c(60, 60)
) |>
  sf::st_sf() |>
  dplyr::mutate(id = dplyr::row_number())

centroids <- grid |>
  sf::st_centroid()

nga_centroids <- centroids |>
  sf::st_intersection(nga_biv_data) |>
  dplyr::rename(geom = sf..st_make_grid.nga_biv_data..n...c.60..60..)

nga_centroids_nogeom <- nga_centroids |>
  sf::st_drop_geometry()

grid_data <- grid |>
  dplyr::left_join(nga_centroids_nogeom) |>
  tidyr::drop_na(name_1)

# Texts ------------------------------------------------------------------

title_text <- "Insecticide-Treated Nets access & use (Nigeria, 2024)"
subtitle_text <- "**Access**: % of population with access to an ITN in their household, **Use**: % of population that sleeps under an ITN"
caption_text <- add_caption(
  source = "Malaria Atlas Project (malariaatlas.org) - ITN Rate",
  title = "Distributions - Pop Culture",
  day = 10
)

# Plots ------------------------------------------------------------------

base_map <- ggplot2::ggplot() +
  ggplot2::geom_sf(
    data = grid_data,
    ggplot2::aes(fill = bi_class),
    color = palettes$color_grid,
    linewidth = 0.6,
    show.legend = FALSE
  ) +
  ggplot2::geom_point(
    data = nga_centroids,
    ggplot2::aes(geometry = geom),
    shape = 21,
    size = 0.9,
    stat = "sf_coordinates",
    fill = colorspace::adjust_transparency("#3d3d3d", 0.2),
    color = colorspace::adjust_transparency(palettes$paper, 0.6),
    stroke = 1.2
  ) +
  ggplot2::labs(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text,
  ) +
  biscale::bi_scale_fill(
    pal = pal,
    dim = 4,
    flip_axes = FALSE,
    rotate_pal = FALSE
  ) +
  ggplot2::coord_sf(expand = FALSE, clip = "off") +
  theme_30dcc(caption.hjust = 0) +
  ggplot2::theme_sub_plot(
    title = ggtext::element_textbox_simple(
      family = "Fredoka",
      margin = ggplot2::margin(b = 6)
    ),
    subtitle = ggtext::element_textbox_simple(
      family = "Atkinson Hyperlegible Mono",
      margin = ggplot2::margin(b = 8)
    ),
    caption = ggtext::element_textbox_simple(
      family = "Atkinson Hyperlegible Mono",
      margin = ggplot2::margin(t = 14),
      size = 9
    ),
    margin = ggplot2::margin(0, 0, 0, 0)
  ) +
  ggplot2::theme_sub_panel(
    grid.major = ggplot2::element_blank(),
    grid.minor = ggplot2::element_blank(),
  ) +
  ggplot2::theme_sub_axis(
    text = ggplot2::element_blank(),
    title = ggplot2::element_blank()
  )


x_labs <- quantile(nga_biv_data$access, probs = c(0, 0.25, 0.5, 0.75, 1)) |>
  round(1)
y_labs <- quantile(nga_biv_data$use, probs = c(0, 0.25, 0.5, 0.75, 1)) |>
  round(1)

legend <- biscale::bi_legend(
  pal = pal,
  pad_color = color_border,
  flip_axes = TRUE,
  rotate_pal = FALSE,
  dim = 4,
  breaks = legend_breaks,
  xlab = "ITN access",
  ylab = "ITN use",
  size = 10,
  base_family = "Atkinson Hyperlegible Mono"
) +
  ggplot2::scale_x_continuous(
    breaks = c(0.5, 1.5, 2.5, 3.5, 4.5),
    labels = scales::percent(x_labs, scale = 1),
    expand = c(0.015, 0.015)
  ) +
  ggplot2::scale_y_continuous(
    breaks = c(0.5, 1.5, 2.5, 3.5, 4.5),
    labels = scales::percent(y_labs, scale = 1),
    expand = c(0.015, 0.015)
  ) +
  theme_30dcc() +
  ggplot2::theme_sub_plot(
    background = ggplot2::element_rect(fill = NA, color = NA),
  ) +
  ggplot2::theme_sub_panel(
    background = ggplot2::element_rect(fill = NA, color = NA),
  ) +
  ggplot2::theme_sub_axis(
    text = ggplot2::element_text(size = 9),
    title = ggplot2::element_text(size = 11),
    ticks = ggplot2::element_line(color = palettes$ink)
  )

final <- base_map +
  patchwork::inset_element(
    legend,
    0.06,
    -0.075,
    1.77,
    0.35,
    align_to = "plot"
  ) +
  patchwork::plot_annotation(
    theme = theme_30dcc() +
      ggplot2::theme_sub_plot(margin = ggplot2::margin(16, 0, 10, 0))
  )

ggh4x::save_plot(
  here::here("2026/charts", "2026_day10-Pop-culture.png"),
  plot = final,
  width = 25,
  height = 21,
  units = "cm",
  dpi = 96,
  bg = palettes$paper
)
