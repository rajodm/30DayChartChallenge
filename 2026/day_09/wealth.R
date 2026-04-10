# Packages ---------------------------------------------------------------

library(tidyverse)
library(sf)
library(biscale)
library(patchwork)

# Colors & Theme ---------------------------------------------------------

palettes <- list(
  paper = "#f2f2f4",
  ink = "#1e1e1e",
  color_sub = "#4a4a4e",
  color_muted = "#7a7a80",
  color_grid = "#e0e0e4",
  color_accent = "#2d4a3e"
)

source("theme_30dcc.R")

color_blue <- "#d6e4ec"
color_border <- "#7a7d84"
color_gray <- "#d4d6da"

nga_admin1 <- geodata::gadm("NGA") |>
  janitor::clean_names() |>
  sf::st_as_sf(crs = sf::st_crs(4326))

nga <- nga_admin1 |>
  mutate(
    name = dplyr::coalesce(varname_1, name_1),
    name = dplyr::replace_values(
      name,
      "Nasarawa" ~ "Nassarawa"
    )
  )

# Resolution must be the same as for nga_admin1
world <- geodata::world(resolution = 1) |>
  janitor::clean_names() |>
  sf::st_as_sf(crs = sf::st_crs(4326)) |>
  sf::st_make_valid()

world <- world |>
  dplyr::filter_out(gid_0 == "NGA")

nga_bbox <- sf::st_bbox(nga_admin1)

# Add some buffer
buffer <- 0.8

new_bb <- nga_bbox + c(-buffer, -buffer, buffer, buffer)

neighbors <- sf::st_crop(world[1], new_bb)

# Data from malariaatlas.org
# nigeria_itn <- read_csv(here::here(
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

nga_biv_dat <- nga_data |>
  biscale::bi_class(
    x = access,
    y = use,
    style = "quantile",
    dim = 4
  )

pal_mat <- cols4all::c4a(
  "bivario.grand_budapest",
  n = 4,
  type = "bivc",
  range = c(0.2, 1)
)

# Transform the palette to a format compatible with biscale
pal <- pal_mat |>
  as.vector() |>
  setNames(
    paste0(
      rep(1:nrow(pal_mat), times = ncol(pal_mat)),
      "-",
      rep(1:ncol(pal_mat), each = nrow(pal_mat))
    )
  )

leg_breaks <- nga_biv_dat |>
  biscale::bi_class_breaks(
    x = access,
    y = use,
    style = "quantile",
    dim = 4,
    dig_lab = c(x = 3, y = 3),
    si_levels = FALSE,
    split = TRUE
  )

# Texts ------------------------------------------------------------------

title_text <- "Insecticide-treated Net access and use across Nigeria"

subtitle_text <- "**Access**: Proportion of population with Access to an ITN in their Household in 2024<br>**Use**: Proportion of population that sleeps under an ITN in 2024"

caption_text <- add_caption(
  source = "Malaria Atlas Project (malariaatlas.org) - ITN Rate",
  title = "Distributions - Wealth",
  day = "09"
)

annotation <- nga_biv_dat |>
  dplyr::filter(name_1 %in% c("Borno", "Ondo")) |>
  dplyr::select(name_1, use, access) |>
  dplyr::mutate(
    pt = sf::st_point_on_surface(geometry),
    label = dplyr::recode_values(
      name_1,
      "Borno" ~ "High access,\nHigh usage",
      "Ondo" ~ "Low access,\nLow usage"
    )
  ) |>
  dplyr::mutate(
    .by = name_1,
    x = sf::st_coordinates(pt)[, 1],
    y = sf::st_coordinates(pt)[, 2]
  )

# Plot -------------------------------------------------------------------

map <- ggplot2::ggplot() +
  ggplot2::geom_sf(
    data = neighbors,
    fill = color_gray,
    color = color_border,
  ) +
  ggplot2::geom_sf(
    data = nga_biv_dat,
    ggplot2::aes(fill = bi_class, geometry = geometry),
    color = color_border,
    show.legend = FALSE
  ) +
  ggspatial::geom_spatial_point(
    data = annotation,
    ggplot2::aes(x = x, y = y),
    color = dplyr::if_else(
      annotation$name_1 == "Ondo",
      palettes$ink,
      "#fcfaf5"
    ),
    crs = 4326
  ) +
  ggspatial::geom_spatial_label_repel(
    data = annotation,
    ggplot2::aes(
      x = x,
      y = y,
      label = label,
    ),
    nudge_y = dplyr::if_else(annotation$name_1 == "Ondo", -1.5, 2.3),
    nudge_x = dplyr::if_else(annotation$name_1 == "Borno", 0.5, -1),
    min.segment.length = 0,
    fill = dplyr::if_else(
      annotation$name_1 == "Ondo",
      pal["1-1"],
      pal["4-4"]
    ),
    color = dplyr::if_else(
      annotation$name_1 == "Ondo",
      palettes$ink,
      "#fcfaf5"
    ),
    family = "Atkinson Hyperlegible Next",
    crs = 4326,
  ) +
  biscale::bi_scale_fill(
    pal = pal,
    dim = 4,
    flip_axes = FALSE,
    rotate_pal = FALSE
  ) +
  ggplot2::labs(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text
  ) +
  ggplot2::coord_sf(expand = FALSE, crs = 4326) +
  theme_30dcc(grid = "none") +
  ggplot2::theme_sub_panel(
    background = ggplot2::element_rect(
      fill = color_blue,
      color = palettes$paper
    ),
  ) +
  ggplot2::theme_sub_plot(
    title = ggtext::element_textbox_simple(
      margin = ggplot2::margin(b = 6)
    ),
    background = ggplot2::element_rect(
      fill = palettes$paper,
      color = palettes$paper
    ),
    margin = ggplot2::margin(0, 0, 0, 0)
  ) +
  ggplot2::theme_sub_axis(
    text = ggplot2::element_blank(),
    title = ggplot2::element_blank()
  )

# Legend
x_labs <- quantile(nga_biv_dat$access, probs = c(0, 0.25, 0.5, 0.75, 1)) |>
  round(1)
y_labs <- quantile(nga_biv_dat$use, probs = c(0, 0.25, 0.5, 0.75, 1)) |>
  round(1)

legend <- biscale::bi_legend(
  pal = pal,
  pad_color = color_border,
  flip_axes = TRUE,
  rotate_pal = FALSE,
  dim = 4,
  breaks = leg_breaks,
  xlab = "ITN access",
  ylab = "ITN use",
  size = 10,
  base_family = "Atkinson Hyperlegible Next"
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
    text = ggplot2::element_text(size = 8),
    title = ggplot2::element_text(size = 10),
    ticks = ggplot2::element_line(color = palettes$ink)
  )

final <- map +
  patchwork::inset_element(legend, 0.06, -0.03, 1.6, 0.33, align_to = "panel") +
  patchwork::plot_annotation(
    theme = theme_30dcc() +
      ggplot2::theme_sub_plot(
        margin = ggplot2::margin(16, 0, 10, 0)
      )
  )

ggh4x::save_plot(
  here::here("2026/charts", "2026_day09-wealth.png"),
  plot = final,
  width = 25,
  height = 21,
  units = "cm",
  bg = palettes$paper
)
