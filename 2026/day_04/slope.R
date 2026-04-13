# Packages ---------------------------------------------------------------

library(tidyverse)
library(ggimage)


# Colors & Theme ---------------------------------------------------------

palettes <- list(
  paper = "#171717",
  ink = "#e6e6e4",
  color_sub = "#969693",
  color_muted = "#5e5e5c",
  color_grid = "#272725",
  color_accent = "#89b4a0"
)

source("theme_30dcc.R")

# Data -------------------------------------------------------------------

af <- rnaturalearth::ne_countries(
  # type = "map_units",
  continent = "Africa",
  returnclass = "sf"
)

# Data from Our World in Data : https://ourworldindata.org/grapher/incidence-of-malaria
# malaria_inc <- read_csv(here::here(
#   "data26/incidence-of-malaria.csv"
# )) |>
#   janitor::clean_names()

af_countries <- af |>
  dplyr::select(name_en, iso_a3, iso_a2) |>
  dplyr::left_join(malaria_inc, dplyr::join_by(iso_a3 == code)) |>
  sf::st_drop_geometry() |>
  tibble::as_tibble() |>
  dplyr::filter(year %in% c(2023, 2024))

af_data <- af_countries |>
  dplyr::mutate(
    new_code = stringr::str_to_lower(iso_a2),
  ) |>
  tidyr::pivot_wider(
    names_from = year,
    values_from = incidence_of_malaria_per_1_000_population_at_risk,
    names_prefix = "year_"
  ) |>
  dplyr::mutate(
    ir_change = (year_2024 - year_2023) >= 10,
  )

increased_inc <- af_data |>
  filter_out(!ir_change)

annotations_df <- tibble::tribble(
  ~x  , ~y  , ~text                   ,
  300 , 100 , "↓ Incidence decreased" ,
  100 , 300 , "↑ Incidence Increased"
)

# Texts ------------------------------------------------------------------

title_text <- "Six African countries saw malaria incidence rise by at least 10 cases per 1,000 population between 2023 and 2024"

subtitle_text <- "Each flag represents a country's *Plasmodium falciparum* incidence rate. Countries above the diagonal line recorded higher incidence in 2024 than in 2023, with 6 showing an increase of at least 10 cases per 1,000 population."

caption_text <- add_caption(
  note = "*P. falciparum* incidence rate based on modelled estimates",
  source = "World Health Organization (Global Health Observatory), via World Bank (2026) | processed by Our World in Data",
  title = "Comparisons - Slope",
  day = "04"
)

# Plot -------------------------------------------------------------------

plot <- af_data |>
  ggplot2::ggplot(ggplot2::aes(year_2023, year_2024)) +
  ggplot2::annotate(
    "polygon",
    x = c(0, 400, 0),
    y = c(0, 400, 400),
    fill = palettes$color_muted,
    alpha = 0.12
  ) +
  ggimage::geom_flag(
    ggplot2::aes(image = new_code),
    size = 0.035,
    by = "height"
  ) +
  ggrepel::geom_marquee_repel(
    data = increased_inc,
    ggplot2::aes(label = name_en),
    size = 4.21,
    seed = 123,
    family = "Atkinson Hyperlegible Next",
    min.segment.length = 0.6,
    box.padding = 0.8,
    nudge_y = 14,
    nudge_x = -38,
    color = palettes$ink
  ) +
  ggplot2::geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "1234",
    linewidth = 0.6
  ) +
  ggtext::geom_richtext(
    data = annotations_df,
    ggplot2::aes(x = x, y = y, label = text),
    family = "syne",
    fontface = "bold",
    fill = palettes$ink,
    color = palettes$color_grid,
    size = 4.21
  ) +
  ggplot2::annotate(
    "text",
    x = 190,
    y = 140,
    label = "No change\nin incidence",
    size = 4.21,
    fontface = "bold",
    hjust = 0
  ) +
  ggarrow::geom_arrow_curve(
    x = 186,
    xend = 160,
    y = 140,
    yend = 158,
    curvature = -0.2,
    linewidth = 0.6,
    arrow_head = ggarrow::arrow_head_minimal(),
    arrow_fins = ggarrow::arrow_fins_minimal()
  ) +
  ggplot2::coord_cartesian(
    # expand = FALSE,
    clip = "off"
  ) +
  ggplot2::labs(
    title = title_text,
    subtitle = subtitle_text,
    x = "\\# *P. falciparum* cases per 1,000 in 2023",
    y = "\\# *P. falciparum* cases per 1,000 in 2024",
    caption = caption_text
  ) +
  theme_30dcc() +
  ggplot2::theme_sub_plot(
    subtitle = ggtext::element_textbox_simple(
      margin = ggplot2::margin(t = 6),
    ),
    caption = ggtext::element_textbox_simple(
      margin = ggplot2::margin(t = 6),
      halign = 1
    )
  ) +
  ggplot2::theme_sub_axis_y(
    title = ggtext::element_textbox_simple(
      halign = 0.5,
      margin = ggplot2::margin(b = 10),
      face = "bold",
      orientation = "left-rotated"
    )
  ) +
  ggplot2::theme_sub_axis_x(
    title = ggtext::element_textbox_simple(
      halign = 0.5,
      margin = ggplot2::margin(t = 10),
      face = "bold"
    )
  )

ggh4x::save_plot(
  here::here("2026/charts", "2026_day04-slope1.png"),
  plot = plot,
  width = 25,
  height = 25,
  units = "cm",
  dpi = 600
)
