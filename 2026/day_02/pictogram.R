# Packages ---------------------------------------------------------------

library(tidyverse)
library(waffle)
library(systemfonts)

# Colors & Theme ---------------------------------------------------------

palettes <- list(
  paper = "#f8f8f8",
  ink = "#1a1a1a",
  color_sub = "#3d3d3d",
  color_muted = "#787878",
  color_grid = "#e2e2e2",
  color_accent = "#e63946"
)

systemfonts::register_variant(
  "Fa7",
  "Font Awesome 7 Free",
  "heavy",
)

source("theme_30dcc.R")


# Data -------------------------------------------------------------------

pf_pr <- malariaAtlas::getPR(
  country = "ALL",
  continent = "Africa",
  species = "Pf"
)

# dhs_id is empty
# pf_pr <- pf_pr |>
#   select(!dhs_id)

pr_last_af <- pf_pr |>
  filter(
    # Filter to children aged 2-10
    lower_age >= 2,
    upper_age <= 10,
    !is.na(examined) # Remove the survey if noone was tested
  ) |>
  summarize(
    .by = c(country, year_start),
    pr_weighted = weighted.mean(pr, w = examined),
    total = sum(examined)
  ) |>
  slice_max(year_start, n = 1, by = country) |>
  filter_out(pr_weighted <= 0.005) # Remove parasite rate lower than 0.5%

# Texts ------------------------------------------------------------------
title_text <- "Malaria burden in African children varies widely from less than 1% to over 60%"

subtitle_text <- glue::glue(
  "<span style='color: {palettes$color_accent};'>**Red figures show children who tested positive for *Plasmodium falciparum***</span>",
  " among every 100 surveyed aged 2-10 (Pf PR<sub>2-10</sub>).",
  " Country name, survey year and positivity rate are shown above each panel."
)

add_caption(
  note = "**Survey years vary (1985-2022). Most recent available Pf PR<sub>2-10</sub> survey per country shown**",
  "Malaria Atlas Project (malariaatlas.org) - *P. falciparum* parasite rate survey | {malariaAtlas} R package",
  "Comparisons - Pictogram",
  day = "02"
)

# Plot -------------------------------------------------------------------

plot <- pr_last_af |>
  mutate(
    pr_prop = ceiling(pr_weighted * 100), # round up to ge a full icon
    not_infc = 100 - pr_prop,
    prop_labs = scales::percent(pr_weighted, 0.1)
  ) |>
  arrange(desc(pr_prop)) |>
  mutate(
    country = replace_values(
      country,
      "Central African Republic" ~ "C. African Rep."
    ),
    labs = glue::glue("{country}<br>{year_start} \\- {prop_labs}"),
    labs = fct_inorder(labs)
  ) |>
  pivot_longer(
    cols = c(pr_prop, not_infc),
    names_to = "status",
    values_to = "prop"
  ) |>
  ggplot() +
  geom_pictogram(
    aes(label = status, values = prop, color = status),
    size = 2.63,
    flip = TRUE,
    show.legend = FALSE,
    family = "Fa7" # Use solid icon from FA
  ) +
  scale_color_manual(
    values = c(
      pr_prop = palettes$color_accent,
      not_infc = palettes$color_muted
    )
  ) +
  scale_label_pictogram(
    name = NULL,
    values = c("child", "user"),
  ) +
  facet_wrap(~labs, ncol = 5) +
  coord_cartesian(expand = FALSE, clip = "off") +
  labs(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text
  ) +
  theme_30dcc() +
  theme_sub_panel(grid.major = element_blank()) +
  theme_sub_plot(
    title = ggtext::element_textbox_simple(
      margin = margin(b = 4)
    ),
    caption = ggtext::element_textbox_simple(
      halign = 0,
      margin = margin(t = 6)
    ),
    margin = margin(10, 16, 10, 16)
  ) +
  ggplot2::theme_sub_strip(
    text = ggtext::element_textbox_simple(
      size = 10,
      face = "bold",
      halign = 0.5,
      margin = ggplot2::margin_part(b = 2),
    ),
    clip = "off"
  ) +
  theme_enhance_waffle()


ggh4x::save_plot(
  here::here("2026/charts", "2026_day02-pictogram.png"),
  plot = plot,
  height = 25,
  width = 21,
  units = "cm",
  dpi = 600
)
