# Packages -------------------------------------------------------------------

library(tidyverse)
library(countrycode)
library(ggbeeswarm)
library(patchwork)


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

pf_inc <- read_csv(here::here("data26/malariaAtlas/map_pf_ir_2024.csv")) |>
  janitor::clean_names() |>
  filter_out(value == 0)

stat_pf_inc <- pf_inc$value |>
  skimr::skim()

# Texts ------------------------------------------------------------------

caption_text <- add_caption(
  source = "Malaria Atlas Project (malariaatlas.org) | *Pf* Rate 2024",
  day = 12,
  title = "Distributions - FlowingData - Theme Day"
)

label_text <- glue::glue(
  "MEDIAN NUMBER OF CASES\n(countries with Pf cases detected)\n{round(stat_pf_inc$numeric.p50, 2)} per 1,000"
)
# Plots -----------------------------------------------------------------

plot <- pf_inc |>
  ggplot(aes(value, y = 0)) +
  ggbeeswarm::geom_beeswarm(aes(color = value), size = 2.8) +
  geom_vline(
    xintercept = stat_pf_inc$numeric.p50,
    linetype = "dashed",
    color = palettes$ink,
    alpha = 0.8
  ) +
  annotate(
    "text",
    x = stat_pf_inc$numeric.p50 + 5,
    y = Inf,
    label = label_text,
    vjust = 1,
    hjust = 0
  ) +
  ggforce::geom_mark_circle(
    data = pf_inc |>
      slice_max(value),
    aes(label = glue::glue("{name}\n{round(value, 2)} cases")),
    con.colour = palettes$ink,
    label.fill = "transparent",
    label.colour = palettes$ink,
    label.fontface = "plain",
    label.family = "Atkinson Hyperlegible Mono",
    con.type = "straight"
  ) +
  cols4all::scale_color_continuous_c4a_seq("gmt.panoply") +
  labs(x = "Cases<br>per 1,000 →") +
  theme_30dcc(grid = "none", base_family = "Atkinson Hyperlegible Mono") +
  theme(aspect.ratio = 0.618) +
  theme_sub_panel(
    grid.major.x = element_line(linewidth = 0.5, linetype = "31")
  ) +
  theme_sub_axis_y(title = element_blank(), text = element_blank()) +
  theme_sub_axis_x(
    title = ggtext::element_textbox_simple(
      halign = 0.01,
      margin = margin(t = 3)
    )
  )

spacer <- plot_spacer() +
  theme_30dcc()

final_plot <- spacer /
  plot /
  spacer +
  plot_layout(heights = c(0.2, 1.6, 0.2)) +
  plot_annotation(
    title = "Plasmodium falciparum incidence 2024",
    caption = caption_text,
    theme = theme_30dcc(base_family = "Atkinson Hyperlegible Mono") +
      theme_sub_plot(
        background = element_rect(fill = palettes$paper, color = NA),
        title = ggtext::element_textbox_simple(halign = 0.5),
        caption = ggtext::element_textbox_simple(halign = 0.5)
      )
  )

ggh4x::save_plot(
  here::here("2026/charts", "2026_day12-theme_day-flowing_data.png"),
  plot = final_plot,
  width = 25,
  height = 25,
  units = "cm",
  dpi = 600
)
