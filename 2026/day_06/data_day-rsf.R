library(tidyverse)
library(geofacet)


palettes <- list(
  paper = "#fdf8f3",
  ink = "#2e2118",
  color_sub = "#3d3d3b",
  color_muted = "#737370",
  color_grid = "#ede3d8",
  color_accent = "#c1612f"
)

source("theme_30dcc.R")
source(here::here("2026/day_06", "custom_grid.R"))

country_lookup <- tibble::tribble(
  ~from                      , ~to             ,
  "C\xf4te d'Ivoire"         , "Côte d'Ivoire" ,
  "Congo-Brazzaville"        , "R. Congo"      ,
  "Morocco / Western Sahara" , "Morocco"
)

rsf_score22 <- readr::read_csv2(
  "https://rsf.org/sites/default/files/import_classement/2022.csv"
) |>
  janitor::clean_names()

rsf_score25 <- readr::read_csv2(
  "https://rsf.org/sites/default/files/import_classement/2025.csv"
) |>
  janitor::clean_names() |>
  dplyr::rename(
    score = score_2025
  )

rsf_combined <- rsf_score22 |>
  dplyr::inner_join(rsf_score25, by = c("iso"), suffix = c("_2022", "_2025")) |>
  dplyr::mutate(
    score_diff = score_2025 - score_2022,
    change = dplyr::case_when(
      score_diff <= 0 ~ "down",
      score_diff >= 0 ~ "up",
      TRUE ~ "same"
    )
  ) |>
  dplyr::filter(zone_2025 %in% c("Afrique", "MENA")) |>
  dplyr::mutate(
    country = dplyr::recode_values(
      country_en_2025,
      from = country_lookup$from,
      to = country_lookup$to,
      default = country_en_2025
    )
  ) |>
  dplyr::filter_out(
    # Remove countries from the Middle-East Region
    country %in%
      c(
        "Israel",
        "Qatar",
        "Jordan",
        "Lebanon",
        "United Arab Emirates",
        "Kuwait",
        "Oman",
        "Saudi Arabia",
        "Bahrain",
        "Yemen",
        "Palestine",
        "Syria",
        "Iraq",
        "Iran"
      )
  )

# rsf_combined |>
#   count(change)

rsf_score_af <-
  rsf_combined |>
  tidyr::pivot_longer(
    cols = c(score_2022, score_2025),
    names_to = "year",
    values_to = "score",
  ) |>
  dplyr::mutate(year = readr::parse_number(year))

rsf_score_af_data <- rsf_score_af |>
  dplyr::select(iso, year, score, country, score_diff) |>
  dplyr::mutate(country = stringr::str_replace(country, "Republic*", "R\\."))


plot <- rsf_score_af_data |>
  ggplot2::ggplot(ggplot2::aes(year, score, fill = score_diff)) +
  ggplot2::geom_ribbon(
    ggplot2::aes(ymin = 10, ymax = pmax(score)),
    alpha = 0.5,
    show.legend = FALSE
  ) +
  ggplot2::geom_point(size = 1.6) +
  ggplot2::geom_text(
    ggplot2::aes(label = round(score, 0)),
    nudge_y = 10,
    size = 3.15
  ) +
  ggplot2::geom_line(linewidth = 0.6) +
  ggplot2::scale_x_continuous(
    breaks = c(2022, 2025),
    expand = ggplot2::expansion(add = c(0.3, 0.3))
  ) +
  ggplot2::scale_y_continuous(
    breaks = c(10, 50, 100),
    limits = c(10, 100),
    expand = ggplot2::expansion(mult = 0)
  ) +
  cols4all::scale_fill_continuous_c4a_div(
    "scico.vik",
    reverse = TRUE,
    mid = 0,
    n_interp = 5,
    range = c(0.6, 0.9)
  ) +
  ggplot2::coord_cartesian(clip = "off") +
  geofacet::facet_geo(~country, grid = mygrid, label = "name") +
  ggplot2::labs(
    title = "Press freedom declined in most African countries between 2022 and 2025",
    subtitle = glue::glue(
      "The RSF *World Press Freedom Index* measures press freedom for journalist and media in 180 countries and territories.<br>",
      "This chart shows African countries' scores, where **colors indicate the change between 2022 (left) and 2025 (right)**: <span style='color: #02397a;'>**improvement**</span> or <span style='color: #862407;'>**decline**</span>, darker means greater change."
    ),
    caption = add_caption(
      source = "Reporters Without Borders (RSF), World Press Freedom Index 2022 & 2025. rsf.org/en/index",
      "Reporters Without Borders - Data Day",
      note = "Scores are rounded to the nearest whole number for visualization purposes.",
      day = "06"
    )
  ) +
  theme_30dcc() +
  ggplot2::theme_sub_axis(
    text = ggplot2::element_blank(),
    title = ggplot2::element_blank()
  ) +
  ggplot2::theme_sub_legend(
    position = "none"
  ) +
  ggplot2::theme_sub_strip(
    text = ggplot2::element_text(
      size = 9,
      margin = ggplot2::margin_part(b = -3)
    ),
    clip = "off"
  )

ggh4x::save_plot(
  "test.png",
  plot = plot,
  width = 21,
  height = 29.1,
  units = "cm",
  dpi = 600
)
