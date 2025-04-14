# Packages ---------------------------------------------------------------

library(tidyverse)
library(ggtext)
library(showtext)

# Fonts ------------------------------------------------------------------

# font_add_google("Lora", "lora")
font_add(
  "lora",
  "fonts/Lora-Regular.ttf",
  "fonts/Lora-Bold.ttf"
)

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

# Caption ----------------------------------------------------------------

data <- glue::glue(
  "**Data**: Sea Ice Index, National Snow and Ice Data Center."
)
chart <- glue::glue(
  "**#30DayChartChallenge 2025, Day 16**: Relationships - Negative"
)
bsky <- glue::glue("<span style='font-family:fa6-brands'>&#xe671;</span>")
author <- glue::glue("**Graphic**: {bsky} @rajodm")
caption_text <- glue::glue("{chart} | {author} | #rstats<br>{data}")

# Colors -----------------------------------------------------------------

color_white <- "#d4d2d8"
color_bg <- "#1e1d24"
color_grey <- "#8a8895"
color_grey1 <- "#3a3842"

pal <- c(
  "Gain" = "#71a4c4",
  "Loss" = "#d07880"
)


# Data -------------------------------------------------------------------

sea_ice <- read_csv(here::here("2025/data/sea_ice.csv")) |>
  mutate(month = fct_rev(month(mo, label = TRUE)))

# Compute baseline values grouped by regions and months
baselines <- sea_ice |>
  filter(year %in% 1981:2010) |>
  summarize(
    .by = c(region, month),
    baseline_ext = median(area, na.rm = TRUE)
  )

sea_ice_base <- sea_ice |>
  left_join(baselines, by = c("region", "month"))

sea_ice_negative <- sea_ice_base |>
  mutate(
    area_diff = area - baseline_ext
  )

annual_negative <- sea_ice_negative |>
  filter(year < 2025) |>
  summarize(
    .by = c(region, year),
    median_diff = median(area_diff, na.rm = TRUE),
  ) |>
  mutate(
    cat = if_else(median_diff < 0, "Loss", "Gain"),
    reg = if_else(region == "N", "Arctic", "Antarctic"),
    reg = factor(reg, levels = c("Arctic", "Antarctic"))
  )

theme_set(theme_minimal(base_size = 14, base_family = "workS"))

plot <- annual_negative |>
  ggplot(aes(x = year, y = median_diff, fill = cat)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = color_white) +
  geom_area(alpha = .8, color = NA) +
  scale_fill_manual(
    values = pal,
    labels = c("Loss" = "Ice Loss", "Gain" = "Ice Gain"),
    name = "Sea Ice Trends"
  ) +
  facet_wrap(~reg, ncol = 1, scales = "free_y") +
  coord_cartesian(
    clip = "off",
    expand = FALSE
  ) +
  labs(
    title = "Melting Poles",
    subtitle = "Arctic and Antarctic Sea Ice Area Anomalies Relative to 1981-2010 baseline (1978-2024)",
    caption = caption_text,
    y = "Difference from Baseline (Mkm²)"
  ) +
  theme(
    text = element_text(color = color_white),
    plot.background = element_rect(fill = color_bg, color = NA),
    strip.text = element_text(
      color = color_white,
      face = "bold",
      size = 14,
      hjust = 0
    ),
    axis.text = element_text(color = color_white),
    axis.title = element_text(color = color_white),
    axis.title.x = element_blank(),
    axis.title.y = element_text(margin = margin(r = 8)),
    plot.title.position = "plot",
    plot.title = element_textbox_simple(
      family = "lora",
      face = "bold",
      size = 22,
      margin = margin(b = 8)
    ),
    plot.subtitle = element_textbox_simple(
      color = color_white,
      size = 13,
      margin = margin(b = 24)
    ),
    plot.caption.position = "plot",
    plot.caption = element_textbox_simple(
      color = color_grey,
      size = 9,
      halign = 0,
      margin = margin(t = 12)
    ),
    panel.grid = element_line(
      color = color_grey1,
      linetype = "31",
      linewidth = 0.5
    ),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.key.size = unit(16, "points"),
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 12),
    plot.margin = margin(25, 25, 10, 25)
  )

ggsave(
  "2025/charts/day16-negative.png",
  plot,
  height = 11,
  width = 8.5,
  units = "in"
)
