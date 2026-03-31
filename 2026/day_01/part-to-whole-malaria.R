# Packages ---------------------------------------------------------------

library(malariaAtlas)
library(tidyverse)
library(terra)
library(sf)
library(rnaturalearth)

# Colors & theme ---------------------------------------------------------

palettes <- list(
  paper = "#f8f8f8",
  ink = "#1a1a1a",
  color_sub = "#3d3d3d",
  color_muted = "#787878",
  color_grid = "#e2e2e2",
  color_accent = "#e63946"
)

color_pv <- "#127088"
color_pf <- "#c85729"

source("theme_30dcc.R")

# Data -------------------------------------------------------------------

# # Download the incidence data from MAP
# pf_inc_count_r2025 <- getRaster("Malaria__202508_Global_Pf_Incidence_Count")
# pv_inc_count_r2025 <- getRaster("Malaria__202508_Global_Pv_Incidence_Count")

# pv_inc_count <- terra::rast("pv_incidence_2025.tif")
# pf_inc_count <- terra::rast("pf_incidence_2025.tif")

# Needed because the raster data doesn't have regions
world <- rnaturalearth::ne_countries(returnclass = "sf")

world_vect <- terra::vect(world)

extracted_pf <- terra::extract(
  pf_inc_count,
  world_vect,
  fun = mean,
  na.rm = TRUE
)
extracted_pv <- terra::extract(
  pv_inc_count,
  world_vect,
  fun = mean,
  na.rm = TRUE
)

world_df <- world |>
  dplyr::mutate(
    pf = extracted_pf[, 2],
    pv = extracted_pv[, 2]
  ) |>
  sf::st_drop_geometry()


world_df_summary <- world_df |>
  dplyr::summarize(
    .by = continent,
    pf = sum(pf, na.rm = TRUE),
    pv = sum(pv, na.rm = TRUE)
  ) |>
  janitor::adorn_totals("col") |>
  dplyr::filter_out(Total <= 0) |>
  dplyr::mutate(
    pf_share = pf / Total,
    pv_share = pv / Total
  )

# Texts ------------------------------------------------------------------

title_text <- glue::glue(
  "Two Plasmodium parasites, two geographic patterns: <br>",
  "<span style='color: {color_pf};'>Falciparum</span> in Africa & North America, <span style='color: {color_pv};'>Vivax</span> elsewhere"
)

subtitle_text <- glue::glue(
  "<span style='color: {color_pf};'>***P. falciparum***</span> species accounts for over 98% of malaria cases in Africa and 73% in North America, while <span style='color: {color_pv};'>***P. vivax***</span> represents the majority in Asia, Oceania and South America."
)

caption_text <- add_caption(
  source = "Malaria Atlas Project (malariaatlas.org) - Malaria Global Incidence Count 2025 (PF & PV), accessed via {malariaAtlas} R package - continents from {rnaturalearth}",
  title = "Comparisons - Part-to-Whole",
  day = "01"
)

# Plot -------------------------------------------------------------------

plot_data <-
  world_df_summary |>
  tidyr::pivot_longer(
    cols = pf_share:pv_share,
    names_to = "Parasite",
    values_to = "prop",
    names_pattern = "(.*)_.*"
  ) |>
  dplyr::arrange(continent, dplyr::desc(Parasite)) |>
  dplyr::mutate(
    .by = continent,
    lab_y = cumsum(prop) - prop / 2
  )

plot <- plot_data |>
  ggplot2::ggplot(ggplot2::aes(continent, prop, fill = Parasite)) +
  ggplot2::geom_col(
    position = "fill",
    stat = "identity",
    color = "#f8f8f8",
    width = 0.5,
    linewidth = 0.5
  ) +
  ggplot2::geom_text(
    ggplot2::aes(
      label = scales::percent(prop, accuracy = 0.01),
      y = lab_y,
      color = dplyr::if_else(prop <= 0.02, "#1a1a1a", "#f8f8f8")
    ),
    fontface = "bold"
  ) +
  ggplot2::scale_fill_manual(
    values = c(
      pv = color_pv,
      pf = color_pf
    )
  ) +
  ggplot2::scale_color_identity() +
  ggplot2::facet_wrap(~continent, scales = "free_y", ncol = 1) +
  ggplot2::coord_flip(expand = FALSE, clip = "off") +
  ggplot2::labs(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text
  ) +
  theme_30dcc() +
  ggplot2::theme_sub_plot(
    margin = ggplot2::margin(12, 25, 12, 25),
    title = ggtext::element_textbox_simple(
      margin = margin(b = 14)
    ),
    caption = ggtext::element_textbox_simple(
      hjust = 0,
      margin = margin(t = 12)
    )
  ) +
  ggplot2::theme_sub_axis(
    text = ggplot2::element_blank(),
    title = ggplot2::element_blank()
  ) +
  theme_sub_panel(
    spacing = unit(0.2, "lines")
  ) +
  ggplot2::theme_sub_strip(
    text = ggplot2::element_text(
      size = 14,
      face = "bold",
      margin = ggplot2::margin_part(b = 2, l = 0),
      hjust = 0
    ),
    clip = "off"
  )

ggh4x::save_plot(
  here::here("2026/charts", "2026_day01-part-to-whole.png"),
  plot = plot,
  width = 21,
  height = 25,
  units = "cm",
  dpi = 600
)
