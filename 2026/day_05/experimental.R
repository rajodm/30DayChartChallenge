# Packages ---------------------------------------------------------------

library(tidyverse)
library(sf)
library(cartogram)

# Colors & Theme ---------------------------------------------------------

palettes <- list(
  paper = "#fdf8f3",
  ink = "#2e2118",
  color_sub = "#3d3d3b",
  color_muted = "#737370",
  color_grid = "#ede3d8",
  color_accent = "#c1612f"
)

source("theme_30dcc.R")

# Data -------------------------------------------------------------------

af_sf <- rnaturalearth::ne_countries(
  continent = "Africa",
  returnclass = "sf",
  type = "map_units"
)

# Data from Our World in Data : https://ourworldindata.org/grapher/incidence-of-malaria
# malaria_inc <- read_csv(here::here(
#   "data26/incidence-of-malaria.csv"
# )) |>
#   janitor::clean_names()

malaria_inc_long <- malaria_inc |>
  dplyr::filter(year %in% c(2023, 2024)) |>
  dplyr::rename(
    incidence = incidence_of_malaria_per_1_000_population_at_risk
  ) |>
  tidyr::pivot_wider(
    names_from = year,
    values_from = incidence,
    names_prefix = "inc_"
  ) |>
  dplyr::mutate(
    change = inc_2024 - inc_2023,
    change_direction = dplyr::case_when(
      change <= 10 ~ "decrease",
      change >= 10 ~ "increase",
      TRUE ~ "stable"
    )
  )

af_df_clean <- af_sf |>
  dplyr::left_join(malaria_inc_long, by = dplyr::join_by(iso_a3 == code)) |>
  dplyr::select(inc_2024, iso_a3, entity, change_direction, change) |>
  dplyr::mutate(abs_change = abs(change)) |>
  sf::st_transform(crs = 27701) |>
  dplyr::filter_out(is.na(inc_2024) | inc_2024 == 0)

repel_data <-
  af_df_clean |>
  sf::st_centroid() |>
  dplyr::mutate(
    x = sf::st_coordinates(geometry)[, 1],
    y = sf::st_coordinates(geometry)[, 2],
    entity = dplyr::replace_values(
      entity,
      "Central African Republic" ~ "C. African Rep.",
      "Equatorial Guinea" ~ "Eq. Guinea",
      "Democratic Republic of Congo" ~ "DR Congo",
      "South Sudan" ~ "S. Sudan"
    ),
    entity = stringr::str_wrap(entity, 12),
  ) |>
  sf::st_drop_geometry()

af_carto <- af_df_clean |>
  cartogram::cartogram_ncont(weight = "abs_change")

# Texts ------------------------------------------------------------------

title_text <- "Plasmodium falciparum malaria incidence changed unevenly across Africa between 2023 and 2024"

subtitle_text <- glue::glue(
  "**Country size reflects the absolute change in *P. falciparum* incidence** between 2023 and 2024. **Color indicate direction**:",
  "<span style='color: #6262bb;'> **purple for decline**</span> and ",
  "<span style='color: #b96863;'>**pink for increase**</span>."
)

caption_text <- add_caption(
  source = "World Health Organization (Global Health Observatory), via World Bank (2026) | processed by Our World in Data",
  title = "Comparisons - Experimental",
  day = "05"
)

# Plot -------------------------------------------------------------------

plot <- af_carto |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(ggplot2::aes(fill = change)) +
  ggrepel::geom_text_repel(
    data = repel_data,
    ggplot2::aes(x = x, y = y, label = entity),
    seed = 123,
    box.padding = 0.6,
    min.segment.length = 0.9,
    size = 4.21,
    force = 0.05,
    force_pull = 0.1,
    family = "Atkinson Hyperlegible Next",
    max.overlaps = 20,
    colour = palettes$color_sub,
  ) +
  cols4all::scale_fill_continuous_c4a_div(
    "met.cassatt1",
    reverse = TRUE,
    n_interp = 5,
  ) +
  ggplot2::labs(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text
  ) +
  ggplot2::guides(
    fill = ggplot2::guide_colorsteps(
      title = glue::glue(
        "**Change in incidence**<br>",
        "<span style='color: {palettes$color_sub}; font-size: 11pt;'>cases per 1,000 - 2024 minus 2023</span>"
      ),
      barwidth = 14,
      barheight = 0.7,
      direction = "horizontal"
    )
  ) +
  theme_30dcc() +
  ggplot2::theme_sub_panel(
    grid.major = ggplot2::element_blank(),
    grid.minor = ggplot2::element_blank()
  ) +
  ggplot2::theme_sub_plot(
    subtitle = ggtext::element_textbox_simple(
      margin = ggplot2::margin(b = 6)
    )
  ) +
  ggplot2::theme_sub_axis(
    text = ggplot2::element_blank(),
    title = ggplot2::element_blank()
  ) +
  ggplot2::theme_sub_legend(
    title = ggtext::element_textbox_simple(
      size = 12,
      halign = 0,
      face = "plain",
      lineheight = 0.9,
      margin = ggplot2::margin(b = 2)
    ),
    background = ggplot2::element_blank(),
    position = "inside",
    position.inside = c(0.2, 0.08),
    title.position = "top"
  )


ggh4x::save_plot(
  here::here("2026/charts/2026_day05-experimental.png"),
  plot = plot,
  width = 25,
  height = 25,
  units = "cm",
  dpi = 600
)
