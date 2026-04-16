# Packages ---------------------------------------------------------------

library(tidyverse)


# Theme ------------------------------------------------------------------

palettes <- list(
  paper = "#f4f6fa",
  ink = "#1c2b4a",
  color_sub = "#1c2b4a",
  color_muted = colorspace::lighten("#1c2b4a", 0.4),
  color_grid = "#dde3ed",
  color_accent = "#e07b39"
)

source("theme_30dcc.R")

# Data -------------------------------------------------------------------

itn_au <- read_csv(here::here(
  "data26/malariaAtlas/itn-a_u_ur-national-2024-map.csv"
)) |>
  janitor::clean_names()

table_class <- itn_au |>
  dplyr::filter(metric == "Use Rate") |>
  dplyr::select(name, use_rate = value)

itn_au_clean <-
  itn_au |>
  dplyr::left_join(table_class) |>
  dplyr::mutate(
    name = dplyr::replace_values(
      name,
      "Central African Republic" ~ "Cent. African R.",
      "Democratic Republic of the Congo" ~ "DR Congo",
      "Equatorial Guinea" ~ "Eq. Guinea"
    )
  )

country_levels <- itn_au_clean |>
  dplyr::filter(metric == "Use Rate") |>
  dplyr::arrange(dplyr::desc(value)) |>
  dplyr::pull(name)

itn_data <- itn_au_clean |>
  dplyr::filter_out(metric == "Use Rate") |>
  dplyr::mutate(
    name = factor(name, levels = country_levels),
    metric = factor(metric, levels = c("Use", "Access"))
  )

# Texts ------------------------------------------------------------------

title_text <- "Mosquito net ownership and use don't always match"
subtitle_text <- "Sleeping under an insecticide-treated net (ITN) is a key strategy to control malaria. Across most African countries, both access and use remain far from widespread, this chart shows the **gap between access & use ordered by use rate** (proportion of people with access who actually sleep under an ITN, shown in parentheses) **in 2024**."
caption_text <- add_caption(
  note = "**Access**= proportion of population with access to an ITN, **Use**= proportion of population that sleeps under an ITN, **Use rate**= proportion of population sleeping under an ITN, among those with access to an ITN",
  day = 14,
  source = "Malaria Atlas Project (malariaatlas.org) - ITN Rate 2024",
  title = "Relationships - Trade"
)

make_custom_title <- function(x, y) {
  purrr::map2(x, y, \(x, y) {
    glue::glue("{x} ({y})")
  })
}

lab_data <- itn_data |>
  dplyr::slice_head(n = 1, by = name) |>
  dplyr::arrange(dplyr::desc(name))

plot <- itn_data |>
  ggplot2::ggplot(ggplot2::aes(value, forcats::fct_rev(name))) +
  ggplot2::geom_vline(
    xintercept = 50,
    linetype = "31",
    color = palettes$color_muted
  ) +
  ggplot2::annotate(
    "rect",
    xmin = -Inf,
    xmax = 50,
    ymin = -Inf,
    ymax = 8.5,
    alpha = 0.2,
    fill = palettes$color_accent
  ) +
  ggplot2::geom_line(
    ggplot2::aes(group = name),
    color = "#e8c98a",
    linewidth = 1.6
  ) +
  ggplot2::geom_point(
    ggplot2::aes(fill = metric),
    color = palettes$paper,
    shape = 21,
    size = 4.2,
    stroke = 1.2
  ) +
  ggplot2::annotate(
    "text",
    x = 12,
    hjust = 0,
    y = "Gabon",
    label = "Only about 10% of the population\nhave access to/use an ITN in Gabon",
    size = 3.86,
    lineheight = 0.95,
  ) +
  ggplot2::annotate(
    "text",
    x = 85,
    hjust = 0.5,
    vjust = 1,
    y = "Zambia",
    label = "85% access\nin Togo\nhighest in\nAfrica",
    size = 3.86,
    lineheight = 0.95,
  ) +
  ggplot2::annotate(
    "text",
    x = 49,
    y = "Madagascar",
    hjust = 1,
    vjust = 0.9,
    lineheight = 0.95,
    label = "Lowest ITN\ncoverage & uptake",
    fontface = "bold",
    size = 3.86,
    color = palettes$color_accent
  ) +
  ggforce::geom_mark_circle(
    data = itn_data |>
      dplyr::filter(name == "Ethiopia"),
    ggplot2::aes(label = "Use rate\nis the lowest\nin Ethiopia"),
    y0 = "Ethiopia",
    x0 = 8,
    label.fontsize = 11,
    label.margin = ggplot2::margin(0, 0, 0, 0),
    label.buffer = ggplot2::unit(0.05, "mm"),
    label.fill = NA,
    label.colour = palettes$ink,
    con.colour = palettes$ink,
    label.lineheight = 0.95,
    label.fontface = "plain",
    con.type = "straight",
    con.cap = ggplot2::unit(1, "mm"),
    expand = ggplot2::unit(2.5, "mm"),
  ) +
  ggforce::geom_mark_circle(
    data = itn_data |>
      dplyr::filter(name == "Niger"),
    ggplot2::aes(label = "High ITN adoption in Niger"),
    y0 = "Malawi",
    x0 = 75,
    label.fontsize = 11,
    label.margin = ggplot2::margin(4, 0, 0, 0),
    label.buffer = ggplot2::unit(0.1, "mm"),
    label.fill = NA,
    label.colour = palettes$ink,
    con.colour = palettes$ink,
    label.lineheight = 0.95,
    label.fontface = "plain",
    con.type = "straight",
    con.cap = ggplot2::unit(0.5, "mm"),
    expand = ggplot2::unit(2.5, "mm"),
  ) +
  ggplot2::scale_y_discrete(
    labels = \(x) make_custom_title(x, round(lab_data$use_rate, 1)),
  ) +
  ggplot2::scale_x_continuous(
    breaks = c(20, 40, 50, 60, 80),
    labels = scales::label_percent(scale = 1),
    position = "top"
  ) +
  cols4all::scale_fill_discrete_c4a_cat(
    "met.juarez",
    reverse = TRUE
  ) +
  ggplot2::coord_cartesian(clip = "off") +
  ggplot2::labs(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text,
    x = "Population reached"
  ) +
  theme_30dcc(caption.hjust = 0) +
  ggplot2::theme_sub_plot(
    subtitle = ggtext::element_textbox_simple(
      margin = ggplot2::margin(b = 8)
    )
  ) +
  ggplot2::theme_sub_axis_y(
    title = ggplot2::element_blank(),
    text = ggtext::element_textbox_simple(halign = 0, hjust = 0)
  ) +
  ggplot2::theme_sub_axis_top(
    title = ggplot2::element_text(
      face = "bold",
      margin = ggplot2::margin(t = 0, b = 5)
    )
  ) +
  ggplot2::theme_sub_legend(
    title = ggplot2::element_blank(),
    position = "top",
    justification.top = 1,
    margin = ggplot2::margin(t = 2, b = 2)
  ) +
  ggplot2::theme_sub_strip(text = ggplot2::element_text(face = "bold"))

ggh4x::save_plot(
  here::here("2026/charts", "2026_day14-trade.png"),
  plot = plot,
  width = 21,
  height = 25,
  units = "cm"
)
