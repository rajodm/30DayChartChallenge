# Packages ---------------------------------------------------------------

library(tidyverse)
library(marimekko)
library(malariaAtlas)
library(terra)
library(sf)

# Colors & Theme ---------------------------------------------------------

palettes <- list(
  paper = "#f8f8f8",
  ink = "#1a1a1a",
  color_sub = "#3d3d3d",
  color_muted = "#787878",
  color_grid = "#e2e2e2",
  color_accent = "#e63946"
)

color_lwacc <- "#c85727"
color_midacc <- "#404040"
color_hgacc <- "#127088"

source("theme_30dcc.R")

# Download the data ------------------------------------------------------

# itn_access <- malariaAtlas::getRaster(
#   "Interventions__202508_Africa_Insecticide_Treated_Net_Access",
#   file_path = "data26/"
# )

# pf_incidence_rate <- malariaAtlas::getRaster(
#   "Malaria__202508_Global_Pf_Incidence_Rate",
#   file_path = "data26/malariaAtlas/"
# )

# Data -------------------------------------------------------------------

# itn_access <- terra::rast(
#   "data26/malariaAtlas/Interventions__202508_Africa_Insecticide_Treated_Net_Access_latest__2026_04_01.tiff"
# )

# pf_incidence_rate <- terra::rast(
#   "data26/malariaAtlas/Malaria__202508_Global_Pf_Incidence_Rate_latest__2026_04_02.tiff"
# )

af <- rnaturalearth::ne_countries(
  scale = "large",
  continent = "Africa",
  type = "map_units"
)

af_vect <- terra::vect(af)

extracted_itn_access <- terra::extract(
  itn_access,
  af_vect,
  fun = mean,
  na.rm = TRUE
)

extracted_pf_ir <- terra::extract(
  pf_incidence_rate,
  af_vect,
  fun = mean,
  na.rm = TRUE
)

af_itn_ir <- af |>
  dplyr::mutate(
    int_acc = extracted_itn_access[, 2],
    pfr = extracted_pf_ir[, 2]
  ) |>
  sf::st_drop_geometry()

af_itn_ir <- af_itn_ir |>
  dplyr::select(name_long, int_acc, pfr) |>
  dplyr::filter_out(dplyr::if_any(c(int_acc, pfr), is.nan))

af_itn_cat <- af_itn_ir |>
  dplyr::mutate(
    access_cat = dplyr::case_when(
      int_acc < 0.20 ~ "Low < 20%",
      int_acc < 0.60 ~ "Medium 20-60%",
      TRUE ~ "High > 60%"
    ),
    pf_incr = dplyr::case_when(
      pfr < 0.06 ~ "Low\n< 60 per 1,000",
      pfr <= 0.21 ~ "Medium\n60-210 per 1,000",
      TRUE ~ "High\n> 210 per 1,000"
    ),
    access_cat = forcats::fct_relevel(
      access_cat,
      c("Low < 20%", "Medium 20-60%", "High > 60%")
    ),
    pf_incr = forcats::fct_relevel(
      pf_incr,
      c(
        "Low\n< 60 per 1,000",
        "Medium\n60-210 per 1,000",
        "High\n> 210 per 1,000"
      )
    )
  )


# Texts ------------------------------------------------------------------

caption_text <- add_caption(
  note = "Estimates derived by spatial mean aggregation",
  source = "Malaria Atlas Project (malariaatlas.org) - ITN access & Pf Incidence rate | {malariaAtlas} & {rnaturalearth}",
  title = "Comparisons - Mosaic",
  day = "03"
)

title_text <- "African countries with the highest malaria burden are also those with the greatest ITN access"

subtitle_text <- glue::glue(
  "**Insecticide-Treated-Nets (ITNs)** are key malaria prevention tool. The chart shows the distribution of African countries with ",
  "<span style='color: {color_lwacc};'>**low (< 20%)**</span>, ",
  "<span style='color: {color_midacc};'>**medium (20-60%)**</span> or ",
  "<span style='color: {color_hgacc};'>**high (> 60%)**</span> ",
  "population access to an ITN in their household, by *P. falciparum* incidence rate category."
)

# Helper function for labels

label_country <- function(prop, n) {
  count <- prop * n
  dplyr::if_else(
    count == 1,
    paste(count, "country"),
    paste(count, "countries")
  )
}

# Plot -------------------------------------------------------------------

plot <- af_itn_cat |>
  ggplot2::ggplot() +
  marimekko::geom_marimekko(
    aes(fill = access_cat),
    formula = ~ pf_incr | access_cat,
    show_percentages = TRUE
  ) +
  marimekko::geom_marimekko_text(
    aes(
      # The {marimekko} package have to be loaded to use .marginal
      label = label_country(after_stat(.marginal), nrow(af_itn_cat))
    ),
    size = 4.21,
    fontface = "bold",
    colour = palettes$paper
  ) +
  ggplot2::scale_fill_manual(
    values = c(color_lwacc, color_midacc, color_hgacc)
  ) +
  ggplot2::labs(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text,
    x = "*P. falciparum* incidence rate, 2025",
  ) +
  ggplot2::coord_cartesian(expand = c(bottom = FALSE, top = FALSE)) +
  theme_30dcc(caption.hjust = 0) +
  ggplot2::theme_sub_axis_y(
    title = ggplot2::element_blank(),
    text = ggplot2::element_blank()
  ) +
  ggplot2::theme_sub_axis_x(
    title = ggtext::element_textbox_simple(
      face = "bold",
      size = 14,
      margin = ggplot2::margin(t = 12),
      halign = 0.5
    )
  ) +
  ggplot2::theme_sub_legend(position = "none")

ggh4x::save_plot(
  here::here("2026/charts", "2026_day03-mosaic.png"),
  plot = plot,
  width = 21,
  height = 25,
  units = "cm",
  dpi = 600
)
