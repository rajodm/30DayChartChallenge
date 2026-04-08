# Packages ---------------------------------------------------------------

library(tidyverse)

# Colors & Themes -----------------------------------------------------------------

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

# # Data can be downloaded from malariaatlas.org
# mr <- readr::read_csv(here::here(
#   "data26/malariaAtlas/mortality_rate_national.csv"
# )) |>
#   janitor::clean_names()

mr10y <- mr |>
  dplyr::filter_out(year < 2014 | name == "Mali") |>
  dplyr::arrange(dplyr::desc(year), dplyr::desc(value)) |>
  dplyr::mutate(name = forcats::fct_inorder(name))


# Texts ------------------------------------------------------------------

caption_text <- add_caption(
  source = "Malaria Atlas Project (malariaatlas.org) | *Pf* Rate - Mortality Rate, National Units, 2024",
  title = "Distribution - Circular",
  day = "08"
)

subtitle_text <- "*Plasmodium falciparum* death rates per 100,000 population across the 9 hardest-hit African countries, 2014-2024. Each slice represents on year, each shade its mortality rate."

title_text <- "In Africa's Deadliest Countries for Malaria, the Burden Endures"

# Plot -------------------------------------------------------------------

plot <- mr10y |>
  ggplot2::ggplot(ggplot2::aes(year, value, fill = value)) +
  ggplot2::geom_vline(
    xintercept = 2024,
    color = palettes$color_muted,
    linewidth = 0.5,
    linetype = "1234"
  ) +
  ggplot2::geom_col(width = 1, color = palettes$color_grid, linewidth = 0.3) +
  ggplot2::coord_radial(
    clip = "off",
    expand = FALSE,
    inner.radius = 0,
    r.axis.inside = FALSE
  ) +
  cols4all::scale_fill_continuous_c4a_seq(
    "meteo.sunshine_9lev",
    breaks = c(
      round(min(mr10y$value), 1),
      seq(100, 150, 25),
      round(max(mr10y$value), 1)
    )
  ) +
  ggplot2::scale_x_continuous(
    breaks = seq(2014, 2024, 2),
    labels = \(x) {
      dplyr::if_else(
        x %in% c(2014, 2024),
        as.character(x),
        glue::glue("\u2019{str_sub(x, 3, 4)}")
      )
    }
  ) +
  ggplot2::scale_y_continuous(
    expand = ggplot2::expansion(0)
  ) +
  ggplot2::facet_wrap(~name, ncol = 3) +
  ggplot2::guides(
    fill = ggplot2::guide_colorbar(
      display = "gradient",
      title = "Deaths per 100k, 2024",
      barwidth = 14,
      barheight = 0.5,
      nbin = 100
    )
  ) +
  ggplot2::labs(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text
  ) +
  theme_30dcc() +
  ggplot2::theme_sub_plot(
    subtitle = ggtext::element_textbox_simple(
      margin = ggplot2::margin(t = 6)
    )
  ) +
  ggplot2::theme_sub_axis(
    title = ggplot2::element_blank()
  ) +
  ggplot2::theme_sub_axis_y(
    text = ggplot2::element_blank()
  ) +
  ggplot2::theme_sub_axis_x(
    text = ggplot2::element_text(
      color = palettes$color_sub,
    )
  ) +
  ggplot2::theme_sub_strip(
    text = ggplot2::element_text(
      face = "bold",
      size = 14
    ),
    clip = "off"
  ) +
  ggplot2::theme_sub_legend(
    position = "top",
    justification.top = 1,
    title.position = "top",
    title = ggplot2::element_text(hjust = 1),
    ticks = ggplot2::element_line(color = palettes$color_grid)
  ) +
  ggplot2::theme_sub_panel(
    spacing.x = grid::unit(1.8, "cm")
  )


ggh4x::save_plot(
  here::here("2026/charts", "2026_day08-circular.png"),
  plot = plot,
  width = 21,
  height = 25,
  units = "cm"
)
