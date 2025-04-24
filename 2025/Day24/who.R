# Packages ---------------------------------------------------------------

library(tidyverse)
library(readxl)
library(showtext)
library(geomtextpath)
library(ggtext)

# Fonts ------------------------------------------------------------------

# font_add_google("Inter", "inter")
font_add(
  "inter",
  "fonts/Inter_18pt-Regular.ttf",
  "fonts/Inter_18pt-Bold.ttf"
)

font_add(
  "fa6-brands",
  "fonts/Font Awesome 6 Brands-Regular-400.otf"
)

showtext_auto()
showtext_opts(dpi = 320)

# Texts ------------------------------------------------------------------

# Caption
data <- glue::glue(
  "**Data**: WHO Health Inequality Data Repository - WHO Global Health Observatory"
)
chart <- glue::glue(
  "**#30DayChartChallenge 2025, Day 24**: Timeseries - WHO (data day)"
)
bsky <- glue::glue("<span style='font-family:fa6-brands'>&#xe671;</span>")
author <- glue::glue("**Graphic**: {bsky} @rajodm")
caption_text <- glue::glue("{data}<br>{chart} | {author} | #rstats")

# Text
title <- "Gender and Income Disparities in Physical Inactivity"

subtitle <- "This chart shows the prevalence of insufficient physical activity among adults (18+, age-standardized), segmented by income groups and gender. Women consistently show higher inactivity rates across all income levels. Inactivity is lowest and decreasing in lower-income countries, increase gradually in upper-middle-income countries, and remain stable in the other groups."

# Data -------------------------------------------------------------------

data <- read_csv(here::here("2025/data/who_data.csv"))


income_order <- c(
  "High Income",
  "Upper Middle Income",
  "Lower Middle Income",
  "Low Income"
)

plot_data <- data |>
  filter(
    indicator_name ==
      "Insufficient physical activity among adults aged 18+ years (age-standardized) (%)"
  ) |>
  summarize(
    .by = c(wbincome2024, subgroup, date),
    estimate = median(estimate, na.rm = TRUE)
  ) |>
  drop_na(wbincome2024) |>
  mutate(
    wbincome2024 = str_to_title(wbincome2024),
    wbincome2024 = factor(wbincome2024, levels = income_order)
  )

# Colors -----------------------------------------------------------------

color_teal <- "#3AA99F"
color_indigo <- "#6c75a4"
color_gray <- "#74777b"
color_gray1 <- "#2d3741"
color_black <- "#101a24"
color_black1 <- "#3d4154"
color_bg <- "#f4f0ec"
color_cap <- "#928b82"

pal <- c(
  "Female" = color_indigo,
  "Male" = color_teal
)

# Chart ------------------------------------------------------------------

plot <- plot_data |>
  ggplot(aes(x = date, y = estimate, color = subgroup)) +
  geom_textline(
    aes(label = subgroup),
    linewidth = 1.3,
    size = 4.6,
    show.legend = FALSE
  ) +
  facet_wrap(~wbincome2024) +
  scale_color_manual(values = pal) +
  scale_x_continuous(
    expand = c(.02, .02)
  ) +
  coord_cartesian(
    ylim = c(0, max(plot_data$estimate)),
    clip = "off"
  ) +
  labs(
    title = title,
    subtitle = subtitle,
    caption = caption_text,
    y = "Prevalence of insufficient physical activity (Median, %)"
  ) +
  theme_minimal(base_size = 14, base_family = "inter") +
  theme(
    text = element_text(color = color_black),
    plot.background = element_rect(fill = color_bg, color = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(
      linetype = "31",
      color = "#d7d2cd",
      linewidth = .4
    ),
    axis.text = element_text(color = color_gray, size = 10),
    axis.title = element_text(color = color_gray1, size = 12),
    axis.title.x = element_blank(),
    strip.background = element_rect(fill = "#eae6e2", color = NA),
    strip.text = element_text(face = "bold"),
    plot.title.position = "plot",
    plot.title = element_textbox_simple(
      color = color_black,
      family = "inter",
      face = "bold",
      size = 22,
      margin = margin(b = 2)
    ),
    plot.subtitle = element_textbox_simple(
      color = color_gray1,
      size = 11,
      margin = margin(b = 16)
    ),
    plot.caption.position = "plot",
    plot.caption = element_textbox_simple(
      size = 8,
      color = color_cap,
      margin = margin(t = 12),
      lineheight = 1.3,
      halign = 1
    ),
    plot.margin = margin(.8, .8, .4, .8, "cm")
  )

ggsave(
  "2025/charts/day24-who.png",
  plot,
  height = 21,
  width = 29.7,
  units = "cm",
  dpi = 320
)
