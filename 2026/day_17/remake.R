# Packages ---------------------------------------------------------------

library(tidyverse)
# ! facets are not correctly aligned when using {geofacet} 0.2.4
# # solution: `pak::pak("hafen/geofacet")` to get v 0.2.5
library(geofacet)
library(gt)
library(fs)
library(magick)

# Necessary for webshot2
# Sys.setenv(CHROMOTE_CHROME = "/usr/bin/brave-browser")

# Colors and theme -------------------------------------------------------

palettes <- list(
  paper = "#fafaf8",
  ink = "#1a1a1a",
  color_sub = "#4a4a4a",
  color_muted = "#8a8a8a",
  color_grid = "#e8e8e4",
  color_accent = "#e8f0f5"
)

source("theme_30dcc.R")
source(here::here("2026/day_06", "custom_grid.R"))

# Data -------------------------------------------------------------------

country_lookup <- tibble::tribble(
  ~from                      , ~to             ,
  "C\xf4te d'Ivoire"         , "Côte d'Ivoire" ,
  "Congo-Brazzaville"        , "R. Congo"      ,
  "Morocco / Western Sahara" , "Morocco"
)

rsf_score25 <- readr::read_csv2(
  "https://rsf.org/sites/default/files/import_classement/2025.csv"
) |>
  janitor::clean_names()

rsf_score26 <- readr::read_csv2(
  "https://rsf.org/sites/default/files/import_classement/2026.csv"
) |>
  janitor::clean_names()

rsf_combined <- rsf_score25 |>
  dplyr::inner_join(rsf_score26, by = c("iso"), suffix = c("_2022", "_2025")) |>
  dplyr::mutate(
    score_diff = score_2026 - score_2025,
    rounded_score = round(score_diff, 0),
    change = dplyr::case_when(
      rounded_score < 0 ~ "Decrease",
      rounded_score > 0 ~ "Improvement",
      TRUE ~ "No Change"
    ),
    change = fct_relevel(change, c("Improvement", "No Change", "Decrease"))
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

rsf_score_af <-
  rsf_combined |>
  tidyr::pivot_longer(
    cols = c(score_2025, score_2026),
    names_to = "year",
    values_to = "score",
  ) |>
  dplyr::mutate(year = readr::parse_number(year))

rsf_score_af_data <- rsf_score_af |>
  dplyr::select(iso, year, score, country, score_diff, change) |>
  dplyr::mutate(country = stringr::str_replace(country, "Republic*", "R\\."))

# Table
change_df <- rsf_score_af_data |>
  slice_head(by = iso) |>
  janitor::tabyl(change) |>
  janitor::adorn_pct_formatting() |>
  gt::gt() |>
  gt::cols_label(
    change = "Change vs 2025",
    percent = "prop"
  ) |>
  gt::fmt_number(columns = 3) |>
  gt::cols_align("left", columns = 1) |>
  gt::tab_style(
    style = gt::cell_text(weight = "bold"),
    locations = gt::cells_column_labels()
  ) |>
  gt::tab_style(
    style = gt::cell_fill(palettes$paper),
    locations = list(gt::cells_body(), gt::cells_column_labels())
  ) |>
  gt::tab_options(
    table.font.names = "Atkinson Hyperlegible Next",
    table.font.size = 12,
    table.border.top.color = palettes$color_sub,
    table.border.bottom.color = palettes$color_sub,
    table.font.color = palettes$ink,
    table.background.color = palettes$paper
  ) |>
  gt::opt_css(
    "body {background-color: #fafaf8 !important; margin: 0; padding: 0;}"
  )

# Plot -------------------------------------------------------------------

plot <- rsf_score_af_data |>
  ggplot2::ggplot(ggplot2::aes(year, score, fill = score_diff)) +
  ggplot2::geom_ribbon(
    ggplot2::aes(ymin = 0, ymax = pmax(score)),
    alpha = 0.5,
    show.legend = FALSE
  ) +
  ggplot2::geom_point(size = 1.6) +
  ggplot2::geom_text(
    ggplot2::aes(label = round(score, 0)),
    nudge_y = 18,
    size = 3.15
  ) +
  ggplot2::geom_line(linewidth = 0.6) +
  ggplot2::scale_x_continuous(
    breaks = c(2025, 2026),
    expand = ggplot2::expansion(add = c(0.1, 0.1))
  ) +
  ggplot2::scale_y_continuous(
    breaks = c(0, 50, 100),
    limits = c(0, 100),
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
    title = "RSF Word Press Freedom Index 2026 vs 2025: Only 45.3% of African countries have shown improvement",
    subtitle = glue::glue(
      "The RSF *World Press Freedom Index* measures press freedom for journalist and media in 180 countries and territories. ",
      "This chart shows African countries' scores, where **colors indicate the change between 2025 (left) and 2026 (right)**: <span style='color: #02397a;'>**improvement**</span> or <span style='color: #862407;'>**decline**</span>, darker means greater change."
    ),
    caption = add_caption(
      source = "Reporters Without Borders (RSF), World Press Freedom Index 2025 & 2026. rsf.org/en/index",
      "Relationships - Remake",
      note = "Scores are rounded to the nearest whole number for visualization purposes.",
      day = "17"
    )
  ) +
  theme_30dcc() +
  ggplot2::theme_sub_plot(
    title = ggtext::element_textbox_simple(
      family = "Domine",
      margin = ggplot2::margin(b = 6)
    )
  ) +
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
      margin = ggplot2::margin_part(b = 2)
    ),
    clip = "off"
  )

ggh4x::save_plot(
  here::here("2026/charts", "2026_day17-remake.png"),
  plot = plot,
  width = 21,
  height = 25,
  units = "cm",
  dpi = 600
)

gt::gtsave(change_df, here::here("2026/charts", "2027_day17-remake_tbl.png"))
plot_img <- magick::image_read(here::here(
  "2026/charts",
  "2026_day17-remake.png"
))
plot_tbl <- magick::image_read(here::here(
  "2026/charts",
  "2027_day17-remake_tbl.png"
))

tbl_img <- magick::image_resize(plot_tbl, "2500x1000")

plot_info <- magick::image_info(plot_img)
tbl_info <- magick::image_info(tbl_img)

x_offset <- plot_info$width - tbl_info$width - 150
y_offset <- plot_info$height - tbl_info$height - 500

final <- magick::image_composite(
  plot_img,
  tbl_img,
  offset = paste0("+", x_offset, "+", y_offset)
)

magick::image_write(
  final,
  here::here("2026/charts", "2027_day17-remake-final.png")
)

# fs::file_delete(here::here("2026/charts", "2027_day17-remake_tbl.png"))
# fs::file_delete(here::here("2026/charts", "2027_day17-remake.png"))
