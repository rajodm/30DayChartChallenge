# Packages ---------------------------------------------------------------

library(tidyverse)
library(ggtext)
library(showtext)

# Fonts ------------------------------------------------------------------

font_add(
  "lato",
  "fonts/Lato-Regular.ttf",
  "fonts/Lato-Bold.ttf"
)

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
showtext_opts(dpi = 320)

# Caption -------------------------------------------------------------------

data <- glue::glue(
  "**Data**: Energy Institute - Statistical Review of World Energy (2024) – with major processing by Our World in Data"
)
chart <- glue::glue(
  "**#30DayChartChallenge 2025, Day 21**: Time Series - Fossils"
)
bsky <- glue::glue("<span style='font-family:fa6-brands'>&#xe671;</span>")
author <- glue::glue("**Graphic**: {bsky} @rajodm")
caption_text <- glue::glue("{data}<br>{chart} | {author} | #rstats")

# Data -------------------------------------------------------------------

fossil_fuel <- read_csv(here::here(
  "2025/data/fossil-fuel-consumption-by-type.csv"
)) |>
  janitor::clean_names()

events <- tibble(
  year = c(1973, 1979, 1986, 2020),
  event = c(
    "OPEC oil embargo",
    "Iran-Iraq war",
    "Oil price collapse",
    "COVID-19"
  ),
  just = c(0.5, 0.5, 0.55, 0.75)
)

plot_data <- fossil_fuel |>
  filter(str_detect(entity, "countries")) |>
  pivot_longer(
    cols = 4:6,
    names_to = "fuel",
    names_pattern = "(.*)_con.*",
    values_to = "consumption_t_wh"
  ) |>
  mutate(
    fuel = str_to_title(fuel),
    entity = str_to_title(entity),
    entity = factor(
      entity,
      levels = c(
        "Lower-Middle-Income Countries",
        "Upper-Middle-Income Countries",
        "High-Income Countries"
      )
    )
  )

# Colors -----------------------------------------------------------------

color_bg <- "#f4f0ec"
color_grid <- "#d7d2c2"
color_black <- "#101a24"
color_black1 <- "#464850"
color_grey <- "#74777b"
color_grey1 <- colorspace::adjust_transparency(color_grey, 0.8)
color_blue <- "#1a4f8c"
color_blue1 <- "#66a0c8"
color_blue2 <- "#abcfe2"
color_cap <- "#928b82"

pals <- c(
  "High-Income Countries" = color_blue,
  "Upper-Middle-Income Countries" = color_blue1,
  "Lower-Middle-Income Countries" = color_blue2
)

# Text -------------------------------------------------------------------

subtitle <- glue::glue(
  "Coal, Gas, and Oil Usage in ",
  "<span style='color: {color_blue}'>**High**</span>, ",
  "<span style='color: {color_blue1}'>**Upper-Middle**</span>, and ",
  "<span style='color: {color_blue2}'>**Lower-Middle**</span> Income Countries (1965-2023)"
)

# Plot -------------------------------------------------------------------

theme_set(theme_minimal(base_size = 10, base_family = "workS"))

p <- plot_data |>
  ggplot(aes(x = year, y = consumption_t_wh, color = entity, label = entity)) +
  geomtextpath::geom_textvline(
    data = events,
    aes(xintercept = year, label = event, hjust = just),
    color = color_grey1,
    alpha = .6,
    size = 2.6,
    linewidth = .3,
    linetype = "31"
  ) +
  geom_line(linewidth = .8, alpha = .8, show.legend = FALSE) +
  scale_y_continuous(
    labels = scales::number_format(scale_cut = scales::cut_short_scale()),
  ) +
  scale_x_continuous(
    breaks = scales::breaks_pretty()
  ) +
  scale_color_manual(values = pals) +
  facet_wrap(~fuel, nrow = 1) +
  coord_cartesian(
    clip = "off",
  ) +
  labs(
    title = "Global Fossil Fuel Consumption Trends",
    subtitle = subtitle,
    caption = caption_text,
    y = "Consumption in terawatt-hours"
  ) +
  theme(
    aspect.ratio = 1.618,
    text = element_text(color = color_black),
    plot.background = element_rect(fill = color_bg, color = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(
      linewidth = .2,
      color = color_grid,
      linetype = "31"
    ),
    axis.text = element_text(color = color_grey),
    axis.title.x = element_blank(),
    axis.title.y = element_text(
      size = 10,
      color = color_black1,
      margin = margin(r = 4)
    ),
    strip.background = element_rect(fill = "#eae6e2", color = NA),
    strip.text.x = element_text(
      size = 10,
      face = "bold",
      family = "lato",
      color = color_black
    ),
    legend.position = "bottom",
    plot.title.position = "plot",
    plot.title = element_textbox_simple(
      size = 16,
      face = "bold",
      color = color_black,
      family = "lato",
      margin = margin(b = 4)
    ),
    plot.subtitle = element_textbox_simple(
      family = "workS",
      size = 10,
      color = color_black1,
      margin = margin(b = 12)
    ),
    plot.caption.position = "plot",
    plot.caption = element_textbox_simple(
      color = color_cap,
      size = 5.6,
      halign = 1,
      margin = margin(t = 12)
    ),
    plot.margin = margin(16, 16, 8, 16)
  )

ggsave(
  "2025/charts/day21-fossils.png",
  p,
  height = 5.25,
  width = 8.5,
  units = "in",
  dpi = 320
)
