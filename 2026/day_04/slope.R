# Packages ---------------------------------------------------------------

library(tidyverse)
library(ggimage)


# Colors & Theme ---------------------------------------------------------

palettes <- list(
  paper = "#171717",
  ink = "#e8e4dc",
  color_sub = "#a89f91",
  color_muted = "#6b6459",
  color_grid = "#2c2925",
  color_accent = "#c8a97e"
)

source("theme_30dcc.R")

# Data -------------------------------------------------------------------

af <- rnaturalearth::ne_countries(
  # type = "map_units",
  continent = "Africa",
  returnclass = "sf"
)

# Data was downloaded directly from MAP website
# available at https://data.malariaatlas.org/trends
# malaria_inc <- read_csv(here::here(
#   "data26/incidence-of-malaria.csv"
# )) |>
#   janitor::clean_names()

af_countries <- af |>
  dplyr::select(name_en, iso_a3, iso_a2) |>
  dplyr::left_join(malaria_inc, dplyr::join_by(iso_a3 == code)) |>
  sf::st_drop_geometry() |>
  tibble::as_tibble() |>
  dplyr::filter(year %in% c(2022, 2024))


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
    ir_change = (year_2024 - year_2022) >= 10,
  )

increased_inc <- af_data |>
  filter_out(!ir_change)

annotations_df <- tibble::tribble(
  ~x  , ~y  , ~text                   ,
  300 , 100 , "↓ Incidence decreased" ,
  100 , 300 , "↑ Incidence Increased"
)

# Texts ------------------------------------------------------------------

title_text <- "Six African countries saw malaria incidence rise by at least 10 cases per 1,000 between 2022 and 2024"

subtitle_text <- "Labelled countries recorded an increase of at least 10 *Plasmodium falciparum* malaria cases per 1,000 people between 2022 and 2024. Other countries recorded smaller increases, decreases, or no meaningful change."

caption_text <- add_caption(
  note = "*Pf* incidence rate based on modelled estimates",
  source = "Malaria Atlas Project (malariaatlas.org) - Pf incidence rate 2022 & 2024",
  title = "Comparisons - Slope",
  day = "04"
)

# Plot -------------------------------------------------------------------

plot <- af_data |>
  ggplot2::ggplot(ggplot2::aes(year_2022, year_2024)) +
  ggplot2::annotate(
    "polygon",
    x = c(0, 425, 0),
    y = c(0, 425, 425),
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
    nudge_y = 10,
    nudge_x = -25,
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
  ggplot2::scale_y_continuous(
    breaks = c(100, 200, 300, 400),
    labels = \(x) {
      dplyr::if_else(
        x == 400,
        paste(x, "cases\nper 1000\nin 2024"),
        (x) |> as.character()
      )
    }
  ) +
  ggplot2::coord_cartesian(
    expand = FALSE,
    clip = "off"
  ) +
  ggplot2::labs(
    title = title_text,
    subtitle = subtitle_text,
    x = "\\# *P. falciparum* cases per 1,000 in 2022",
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
    title = ggplot2::element_blank()
  ) +
  ggplot2::theme_sub_axis_x(
    title = ggtext::element_textbox_simple(
      halign = 0.5,
      margin = ggplot2::margin(t = 10),
      face = "bold"
    )
  )

ggh4x::save_plot(
  here::here("2026/charts", "2026_day04-slope.png"),
  plot = plot,
  width = 25,
  height = 25,
  units = "cm",
  dpi = 600
)
