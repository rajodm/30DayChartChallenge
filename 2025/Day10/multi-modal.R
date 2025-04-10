# Packages ---------------------------------------------------------------

library(tidyverse)
library(ggridges)
library(ggtext)
library(showtext)

# Fonts ------------------------------------------------------------------

# font_add_google("Work Sans", "workS")
font_add(
  "workS",
  "fonts/WorkSans-Regular.ttf",
  "fonts/WorkSans-Bold.ttf"
)

font_add(
  "fa6-brands",
  "fonts/Font Awesome 6 Brands-Regular-400.otf"
)

showtext_auto()
showtext_opts(dpi = 300)

# Colors -----------------------------------------------------------------

color_bg <- "#f4f0ec"
color_black <- "#101a24"
color_teal <- "#5c9da3"
color_indigo <- "#6c75a4"
color_cap <- "#74777b"
color_grey <- "#474c5e"
pal <- c(
  N = color_teal,
  S = color_indigo
)

# Texts ------------------------------------------------------------------

# Text
title <- glue::glue(
  "Seasonal Variability in Sea Ice Extent across<br>",
  "<span style = 'color: {color_indigo}'>Southern</span>",
  "<span style = 'font-size: 14pt'> and </span>",
  "<span style = 'color: {color_teal}'>Northern</span>",
  "<span style = 'font-size: 14pt'> Regions</span>"
)

subtitle <- glue::glue(
  "Monthly sea ice extent distributions in the <span style = 'color: {color_indigo}'>**Antarctic**</span> (South) and <span style = 'color: {color_teal}'>**Arctic**</span> (North)."
)

# Captions
data <- glue::glue(
  "**Data**: Sea Ice Index, National Snow and Ice Data Center."
)
chart <- glue::glue(
  "**#30DayChartChallenge 2025, Day 10**: Distributions - Multi-modal"
)
bsky <- glue::glue("<span style='font-family:fa6-brands'>&#xe671;</span>")
author <- glue::glue("**Graphic**: {bsky} @rajodm")

caption_text <- glue::glue("{chart}<br>{data} | {author} | #rstats")

# Data -------------------------------------------------------------------

sea_ice <-
  read_csv(here::here("2025/data/sea_ice.csv")) |>
  # drop_na(extent) |>
  mutate(
    month = month(mo, label = TRUE),
  )

# Plot -------------------------------------------------------------------

theme_set(theme_minimal(base_size = 14, base_family = "workS"))

plot <- sea_ice |>
  ggplot(aes(x = extent, y = fct_rev(month), fill = region, color = region)) +
  geom_density_ridges(
    alpha = 0.8,
    scale = 0.9,
    rel_min_height = 0.01,
    linewidth = 0.5,
    show.legend = FALSE
  ) +
  scale_fill_manual(values = pal) +
  scale_color_manual(values = colorspace::darken(pal)) +
  scale_x_continuous(
    breaks = scales::breaks_pretty(),
    labels = scales::label_number(suffix = "M")
  ) +
  coord_cartesian(
    clip = "off",
    expand = FALSE
  ) +
  labs(
    title = title,
    subtitle = subtitle,
    caption = caption_text,
    x = "Sea Ice Extent (km²)"
  ) +
  theme(
    text = element_text(color = color_black, family = "workS"),
    plot.background = element_rect(fill = color_bg, color = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(
      linewidth = 0.1,
      linetype = "dashed",
      color = color_cap
    ),
    panel.grid.major.y = element_line(linewidth = 0.3, color = color_cap),
    axis.title.x = element_text(
      size = 13,
      margin = margin(t = 6),
      color = color_grey,
      hjust = 1
    ),
    axis.title.y = element_blank(),
    axis.text.y = element_text(hjust = 0),
    plot.title.position = "plot",
    plot.title = element_textbox_simple(
      color = color_black,
      size = 22,
      halign = 0.5,
      face = "bold",
      lineheight = 1.2
    ),
    plot.subtitle = element_textbox_simple(
      size = 13,
      color = color_grey,
      halign = 0.5,
      margin = margin(t = 4, b = 18)
    ),
    plot.caption.position = "plot",
    plot.caption = element_textbox_simple(
      color = color_cap,
      size = 9,
      halign = 0,
      margin = margin(t = 10)
    ),
    plot.margin = margin(25, 25, 10, 25)
  )

# Saving the plot --------------------------------------------------------

ggsave(
  "2025/charts/day10-multi_modal.png",
  plot,
  height = 11,
  width = 8.5,
  units = "in"
)
