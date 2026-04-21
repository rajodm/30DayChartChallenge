# Packages ---------------------------------------------------------------

library(tidyverse)
library(readxl)

# Colors & Theme ---------------------------------------------------------

palettes <- list(
  paper = "#f4f6fa",
  ink = "#1c2b4a",
  color_sub = "#1c2b4a",
  color_muted = colorspace::lighten("#1c2b4a", 0.5),
  color_grid = "#e8ecf4",
  color_accent = "#e07b39"
)

color_black <- "#575757"
color_red <- "#AD2323"

source("theme_30dcc.R")

# Data -------------------------------------------------------------------

# Data available here:
# https://data.unicef.org/wp-content/uploads/2026/04/JME_Country_Estimates_March_2026.xlsx

data_path <- here::here("data26/JME_Country_Estimates_March_2026.xlsx")

# World Bank Income Levels to include and levels
wbic <- c(
  "High income",
  "Upper middle income",
  "Lower middle income",
  "Low income"
)

# Get WB income level to combine with the data
wbicountries <- wbstats::wb_countries() |>
  dplyr::select(iso3c, income_level)

# The import all sheets in the file and skip cover & note sheets
data <- readxl::excel_sheets(data_path) |>
  rlang::set_names() |>
  purrr::discard(\(x) {
    stringr::str_detect(x, stringr::regex("cover|note", ignore_case = TRUE))
  }) |>
  purrr::map(read_xlsx, path = data_path, col_types = "text") |>
  purrr::list_rbind(names_to = "sheet")

prevalence_data <-
  data |>
  dplyr::filter(
    Indicator %in% c("Stunting", "Overweight"),
    Measure == "Prevalence"
  ) |>
  janitor::clean_names() |>
  dplyr::mutate(
    dplyr::across(c(year, both_sexes_point_estimates:female_upper_limit), \(x) {
      readr::parse_number(x, na = c("-"))
    })
  )

prev_data_clean <-
  prevalence_data |>
  dplyr::left_join(wbicountries, dplyr::join_by(iso_code == iso3c)) |>
  dplyr::filter_out(is.na(income_level) | income_level == "Not classified") |>
  dplyr::mutate(income_level = forcats::fct_relevel(income_level, wbic)) |>
  tidyr::drop_na(both_sexes_point_estimates)

data_mean_prev <- prev_data_clean |>
  dplyr::summarize(
    .by = c(year, income_level, indicator),
    mean = mean(both_sexes_point_estimates, na.rm = TRUE)
  )

# Texts  -----------------------------------------------------------------

caption_text <- add_caption(
  day = 18,
  title = "Relationships - UNICEF - Data Day",
  source = "UNICEF/WHO/World Bank Joint Child Malnutrition Estimates - Country Estimates, March 2026"
)

title_text <- glue::glue(
  "Child Malnutrition: ",
  "<span style='color: {color_black};'>Overweight is rising slowly everywhere</span>",
  " while ",
  "<span style='color: {color_red};'>stunting is still far too common</span>"
)

subtitle_text <- glue::glue(
  "<span style='color: {color_black};'>**Overweight (high weight for height)**</span> and ",
  "<span style='color: {color_red};'>**stunting (low height for age)**</span> are ",
  "two different faces of malnutrition, both affecting under 5 worldwide and  measured against WHO growth standards. The chart shows how the share of affected children evolved from 2000 to 2024 by country income group. ",
  "<span style='color: {color_black};'>**Overweight on top**</span>, ",
  "<span style='color: {color_red};'>**stunting below**</span>, ",
  "bold lines show the group average."
)

# Chart ------------------------------------------------------------------

plot <-
  prev_data_clean |>
  ggplot2::ggplot(ggplot2::aes(
    year,
    color = indicator
  )) +
  ggplot2::geom_hline(
    yintercept = 0,
    color = palettes$color_grid,
    linewidth = 0.8
  ) +
  ggplot2::geom_line(
    ggplot2::aes(group = country_or_area, y = both_sexes_point_estimates),
    linewidth = 0.25,
    alpha = 0.2
  ) +
  ggplot2::geom_line(aes(y = mean), data = data_mean_prev, linewidth = 0.8) +
  cols4all::scale_colour_discrete_c4a_cat("misc.watlington") +
  ggplot2::scale_x_continuous(
    labels = \(x) glue::glue("\u2019{str_pad(x%%100, 2, pad = '0')}"),
    limits = c(2000, 2025),
    expand = ggplot2::expansion(add = c(1, 0))
  ) +
  ggplot2::scale_y_continuous(
    labels = scales::label_percent(scale = 1),
    expand = ggplot2::expansion(add = c(1, 3))
  ) +
  ggplot2::labs(
    x = "Year",
    y = "Prevalence",
    caption = caption_text,
    title = title_text,
    subtitle = subtitle_text
  ) +
  ggplot2::facet_grid(indicator ~ income_level, scales = "free_y") +
  theme_30dcc() +
  ggplot2::theme_sub_plot(
    title = ggtext::element_textbox_simple(
      family = "DM Serif Display",
      margin = ggplot2::margin(b = 14)
    )
  ) +
  ggplot2::theme_sub_axis(
    text = ggplot2::element_text(
      size = 10.5,
      color = colorspace::lighten(palettes$ink, 0.3)
    )
  ) +
  ggplot2::theme_sub_panel(
    spacing.x = ggplot2::unit(10, "mm"),
    spacing.y = ggplot2::unit(5, "mm")
  ) +
  ggplot2::theme_sub_strip(text.y = ggplot2::element_blank())

ggh4x::save_plot(
  here::here("2026/charts", "2026_day18-UNICEF_data.png"),
  plot = plot,
  height = 25,
  width = 21,
  units = "cm",
  dpi = 300
)
