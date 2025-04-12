# Packages ---------------------------------------------------------------

library(tidyverse)
library(patchwork)
library(showtext)
library(ggtext)

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
showtext_opts(dpi = 640)

# Colors -----------------------------------------------------------------

color_black <- "#101a24"
color_black1 <- "#3d4154"
color_cap <- "#74777b"
color_dark2 <- "#686D7ECC"
color_red <- "#c17a63"
color_bg <- "#f4f0ec"
color_white <- "#fcfcfa"
color_blue <- "#768fad"

# Texts ------------------------------------------------------------------
# Text
title <- "Arctic and Antarctic Sea Ice Extent (1978-2024)"

subtitle <- glue::glue(
  "Deviations from the 1981-2010 climatological baselines.<br>",
  "The color scale ranges from <span style = 'color: {color_blue}'>**blue (more extent than average)**</span> through **white (near average)**, to <span style = 'color: {color_red}'>**red (below average)**</span>, with color intensity indicating the magnitude of deviation.<br>"
)

# Caption
data <- glue::glue(
  "**Data**: Sea Ice Index, National Snow and Ice Data Center."
)
chart <- glue::glue(
  "**#30DayChartChallenge 2025, Day 11**: Distributions - Stripes"
)
bsky <- glue::glue("<span style='font-family:fa6-brands'>&#xe671;</span>")
author <- glue::glue("**Graphic**: {bsky} @rajodm")
caption_text <- glue::glue("{chart} | {author} | #rstats<br>{data}")

# Data -------------------------------------------------------------------

sea_ice <- read_csv(here::here("2025/data/sea_ice.csv")) |>
  mutate(
    month = month(mo, label = TRUE)
  )

# Compute the baseline for each regions
baseline <- sea_ice |>
  filter(year %in% 1981:2010) |>
  summarise(
    .by = region,
    baseline = median(extent, na.rm = TRUE)
  ) |>
  pivot_wider(
    names_from = region,
    values_from = baseline
  )

plot_data <- sea_ice |>
  filter(year < 2025) |>
  summarize(
    .by = c(region, year),
    extent = median(extent, na.rm = TRUE)
  )

# A function to create the plot
stripe_chart <- function(
  df,
  midpoint,
  low = color_red,
  mid = color_white,
  high = color_blue,
  region = ""
) {
  df |>
    ggplot(aes(x = year, y = 0, fill = extent)) +
    geom_tile(show.legend = FALSE, alpha = 0.8) +
    scale_fill_gradient2(
      low = low,
      mid = mid,
      high = high,
      midpoint = midpoint
    ) +
    annotate(
      "text",
      x = 1978,
      y = .46,
      label = glue::glue("{region}, baseline: {round(midpoint, 2)}M km²"),
      color = color_dark2,
      fontface = "bold",
      size = 4.8,
      hjust = 0
    ) +
    scale_x_continuous(
      breaks = c(1978, 2024),
      labels = c("1978", "2024")
    ) +
    coord_cartesian(
      clip = "off",
      expand = FALSE
    ) +
    theme_minimal(base_family = "workS", base_size = 12) +
    theme(
      axis.text.y = element_blank(),
      axis.text.x = element_text(size = 12, color = color_cap),
      axis.title = element_blank(),
      plot.margin = margin(2, 0, 2, 0)
    )
}

# Stripe Chart for the northern region
p1 <- plot_data |>
  filter(region == "N") |>
  stripe_chart(midpoint = baseline$N, region = "Northern") +
  theme(
    axis.text.x = element_blank()
  )

# Stripe Chart for the southern region
p2 <- plot_data |>
  filter(region == "S") |>
  stripe_chart(midpoint = baseline$S, region = "Southern")

final_plot <- p1 /
  p2 +
  plot_annotation(
    title = title,
    subtitle = subtitle,
    caption = caption_text,
    theme = theme(
      plot.title.position = "plot",
      plot.title = element_textbox_simple(
        color = color_black1,
        halign = 0.5,
        size = 22,
        width = 72,
        face = "bold",
        margin = margin(b = 8)
      ),
      plot.subtitle = element_textbox_simple(
        color = color_black1,
        halign = 0.5,
        size = 13,
        lineheight = 1.3,
        margin = margin(t = 8, b = 32)
      ),
      plot.caption.position = "plot",
      plot.caption = element_textbox_simple(
        size = 9,
        color = color_cap,
        margin = margin(t = 22),
        halign = 0.5,
        lineheight = 1.2
      ),
      plot.margin = margin(25, 0, 25, 0)
    )
  ) &
  theme(
    aspect.ratio = 0.618,
    text = element_text(family = "workS", size = 14),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = color_bg, color = NA),
    panel.background = element_rect(fill = color_bg, color = NA),
  )

ggsave(
  "2025/charts/day11-stirpes.png",
  final_plot,
  height = 11,
  width = 8.5,
  units = "in",
  dpi = 640
)
