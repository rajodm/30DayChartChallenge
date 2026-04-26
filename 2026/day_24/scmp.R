# Packages ---------------------------------------------------------------

library(tidyverse)
library(ggtext)
library(ggarrow)
library(legendry)

# Colors & Theme ---------------------------------------------------------

palettes <- list(
  paper = "#fafaf8",
  ink = "#1a1a1a",
  color_sub = "#4a4a4a",
  color_muted = "#8a8a8a",
  color_grid = "#e8e8e4",
  color_accent = "#e8f0f5"
)

source("theme_30dcc.R")

# Data -------------------------------------------------------------------

data <- readr::read_csv(here::here("data26/malariaAtlas/pf_pv_pr.csv"))

pv <- data |>
  dplyr::filter(continent_id == "Asia", species == "P. vivax")

plot_data <- pv |>
  dplyr::summarize(
    .by = c(country, year_start),
    w_pr = weighted.mean(pr, w = examined, na.rm = TRUE),
    tt_examined = sum(examined, na.rm = TRUE),
    n_sites = dplyr::n()
  )

# Texts ------------------------------------------------------------------

title_text <- glue::glue(
  "<span style='font-size: 24pt;'>**Vivax malaria in Asia<br>",
  "<span style='color: {palettes$color_sub};'>Reported surveys in<br> the Malaria Atlas Project**</span>",
  "</span>"
)

subtitle_text <- glue::glue(
  "<span style='color: {palettes$color_sub}; font-size: 10pt;'>*P. vivax* is the dominant malaria parasite across of Asia.<br>For each country and year, bubble shows the weighted mean<br>parasite rate (share of people tested positive for *P. vivax*) and<br>size reflects the number of people examined.</span>"
)

caption_text <- add_caption(
  note = "Size legend capped at 20,000 for readability; method used for diagnostic may differ for each site.",
  source = "Malaria Atlas Project (malariaatlas.org) - PR surveys",
  title = "Timeseries - South China Morning Post - Theme Day",
  day = 24
)

# Plot -------------------------------------------------------------------

# helper functions

add_title <- function(label, y, lineheight = 1) {
  ggplot2::annotate(
    "richtext",
    y = I(y),
    x = I(-0.14),
    hjust = 0,
    vjust = 1,
    lineheight = lineheight,
    label.colour = NA,
    fill = NA,
    family = "Source Serif 4",
    label = label
  )
}

add_annotation <- function(
  arrow_point_x,
  arrow_point_y,
  arrow_butt_x,
  arrow_butt_y,
  label = "",
  curvature = 0.2,
  color = "#4a4a4a"
) {
  dir <- arrow_point_x - arrow_butt_x

  x_lab_pos <- if (dir >= 0) {
    arrow_butt_x - 0.5
  } else {
    arrow_butt_x + 0.5
  }

  lab_hjust <- dplyr::if_else(dir >= 0, 1, 0)

  list(
    ggarrow::geom_arrow_curve(
      x = arrow_point_x,
      y = arrow_point_y,
      xend = arrow_butt_x,
      yend = arrow_butt_y,
      arrow_head = ggarrow::arrow_fins_minimal(),
      arrow_fins = ggarrow::arrow_head_minimal(),
      linewidth = 0.5,
      curvature = curvature,
      color = color
    ),
    ggplot2::annotate(
      "richtext",
      label.colour = NA,
      fill = NA,
      x = x_lab_pos,
      y = arrow_butt_y,
      hjust = lab_hjust,
      size = 3.15,
      lineheight = 1.15,
      color = palettes$color_sub,
      label = label
    )
  )
}

# Plot

plot <-
  plot_data |>
  ggplot2::ggplot(ggplot2::aes(year_start, forcats::fct_rev(country))) +
  ggplot2::geom_point(
    ggplot2::aes(size = tt_examined, color = w_pr),
    alpha = 0.8
  ) +
  MetBrewer::scale_color_met_c("Tam") +
  ggplot2::scale_size_continuous(
    transform = "sqrt",
    range = c(1, 17),
    breaks = c(500, 4000, 11000, 20000),
    name = "People examined",
    labels = scales::label_number(scale_cut = scales::cut_short_scale()),
  ) +
  ggplot2::scale_x_continuous(
    expand = ggplot2::expansion(add = c(1, 3)),
    breaks = scales::breaks_pretty(n = 6)
  ) +
  ggplot2::scale_y_discrete(ggplot2::expansion(add = c(0.4, 0))) +
  add_title(title_text, 1.23, 1.6) +
  add_title(subtitle_text, 1.09) +
  ggplot2::labs(caption = caption_text) +
  ggplot2::coord_cartesian(clip = "off") +
  theme_30dcc(base_family = "Inter", caption.hjust = 0) +
  ggplot2::theme_sub_plot(
    margin = ggplot2::margin(20, 42, 8, 42),
    caption.position = "plot",
    caption = ggtext::element_textbox(
      size = 8,
      width = ggplot2::unit(1.1, "npc"),
      fill = palettes$color_accent,
      color = palettes$color_sub,
      padding = ggplot2::margin(6, 0, 6, 4)
    )
  ) +
  ggplot2::theme_sub_axis(text = ggplot2::element_text(size = 9)) +
  ggplot2::theme_sub_axis_y(
    title = ggplot2::element_blank(),
    text = ggplot2::element_text(hjust = 0, color = palettes$color_sub),
  ) +
  ggplot2::theme_sub_axis_x(title = ggplot2::element_blank()) +
  ggplot2::theme_sub_legend(
    position = "top",
    title = ggplot2::element_text(hjust = 1, size = 9),
    text = ggplot2::element_text(size = 7),
    justification.top = 1,
    margin = ggplot2::margin(t = 4, b = 16),
    background = ggplot2::element_blank()
  ) +
  ggplot2::guides(
    color = legendry::guide_colring(
      title = "Parasite rate\n(weighted mean)",
      start = 1.5 * pi,
      end = 2.5 * pi,
      theme = ggplot2::theme_sub_legend(
        margin = ggplot2::margin(r = -24),
        position = "top",
        title.position = "bottom",
        title = element_text(hjust = 0.5),
      )
    ),
    size = legendry::guide_circles(
      text_position = "right",
      theme = ggplot2::theme_sub_legend(
        title.position = "top",
      ),
      override.aes = list(
        size = c(2.687, 7.602, 12.607, 17),
        alpha = 0.2,
        color = palettes$color_sub
      )
    )
  )

final_plot <- plot +
  add_annotation(
    2005,
    21,
    2002,
    19.8,
    "A **2005** survey in Afghanistan<br>examined **40,200** persons,<br>the largest in the dataset",
    curvature = -0.2
  ) +
  add_annotation(
    2001.9,
    17.1,
    1997,
    18.5,
    "Highest PR reported in China,<br>**0.26** in **2002**",
  ) +
  add_annotation(
    1991.2,
    8.9,
    1994,
    8,
    "PR of **0.25** in the Philippines, **1991**"
  ) +
  add_annotation(
    1995,
    16,
    1998,
    13.8,
    "In **1995**, survey in India examined<br>**16,958** peoples with a PR of **0.12**"
  ) +
  add_annotation(2018, 20, 2017, 21, "Only survey<br>reported after 2015") +
  add_annotation(2015, 16, 2016.4, 14, curvature = -0.2) +
  add_annotation(
    2015,
    15,
    2016.4,
    14,
    "**Most surveyed<br>countries**<br>India (**28** surveys)<br>& Indonesia (**27**)",
    curvature = -0.2
  )

ggh4x::save_plot(
  here::here("2026/charts", "2026_day24-scmp.png"),
  plot = final_plot,
  width = 21,
  height = 25,
  units = "cm",
  dpi = 600
)
