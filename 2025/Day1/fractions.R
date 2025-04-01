# Packages ---------------------------------------------------------------

library(tidyverse)
library(treemapify)
library(showtext)
library(ggtext)
library(patchwork)

# Data -------------------------------------------------------------------

novels <- read_csv(here::here("2025/data/agatha_novels.csv")) |>
  # The function definition can be found in the .Rprofile file
  clean_agatha_cols()

shorts <- read_csv(here::here("2025/data/agatha_short_stories.csv")) |>
  clean_agatha_cols()

collections <- read_csv(here::here(
  "2025/data/agatha_short_story_collections.csv"
)) |>
  clean_agatha_cols()

# Just to avoid duplication
summarize_series <- function(df) {
  df |>
    summarize(
      .by = series,
      count = n()
    ) |>
    mutate(
      series = str_replace_all(series, "& ", "&\n"),
    ) |>
    arrange(desc(count)) |>
    mutate(
      series = as_factor(series),
      pct = round(count / sum(count), 2)
    )
}

# Validating series using short collection
validated_collections <- collections |>
  # Filter collections with about only one series
  # Remove series with values like Hercule Poirot (2)...
  filter(str_detect(series, "\\(", negate = TRUE)) |>
  select(name, series)

shorts <- shorts |>
  left_join(
    validated_collections,
    by = c("collection" = "name"),
    suffix = c("", "_coll1")
  ) |>
  left_join(
    validated_collections,
    by = c("collection_2" = "name"),
    suffix = c("", "_coll2")
  ) |>
  left_join(
    validated_collections,
    by = c("collection_3" = "name"),
    suffix = c("", "_coll3")
  )

shorts <- shorts |>
  mutate(
    series = coalesce(
      series,
      series_coll1,
      series_coll2,
      series_coll3
    )
  ) |>
  select(!matches("coll[0-9]$"))


novels_data <- novels |>
  summarize_series()

shorts_data <- shorts |>
  mutate(
    series = replace_na(series, "Not part of a series")
  ) |>
  summarize_series()

all_books <- novels_data |>
  full_join(shorts_data)
# Texts ------------------------------------------------------------------

title <- "Agatha Christie's Novels and Short Stories"
subtitle <- glue::glue(
  "Distribution of {sum(all_books$count)} Agatha Christie's Books ({count(novels)} novels and {count(shorts)} short stories) across major characters, standalone stories, and works published under the pseudonym of Mary Westmacott."
)

showtext_auto()
showtext_opts(dpi = 320)

# Captions ---------------------------------------------------------------

data <- glue::glue("**Data**: Agatha Christie's bibliography by Nicole Mark")
chart <- glue::glue(
  "<b>#30DayChartChallenge 2025, Day 1</b>: Comparisons - Fractions"
)
bsky <- glue::glue("<span style='font-family:fa6-brands'>&#xe671;</span>")
author <- glue::glue("**Graphic**: {bsky} @rajodm")

caption_text <- glue::glue("{chart}<br>{data}<br>{author} | #rstats")

# Colors -----------------------------------------------------------------
# Colors taken from {hrbrthemes::flexoki_extended}

color_black <- "#101a24"
color_black1 <- colorspace::lighten(color_black)
color_white <- "#faf0ec"
fonts_white <- "#f4f4f4"


pal <- c(
  "Hercule Poirot" = "#133051",
  "Not part of a series" = "#fcc192",
  "Miss Marple" = "#d14d41",
  "Mary Westmacott" = "#4385be",
  "Hercule Poirot &\nAriadne Oliver" = "#87d3c3",
  "Tommy &\nTuppence" = "#4f3685",
  "Ariadne Oliver" = "#1c6c66",
  "Harley Quin" = "#dfb431",
  "Parker Pyne" = "#768d21"
)


colors <- c(
  "Hercule Poirot" = fonts_white,
  "Not part of a series" = color_black,
  "Miss Marple" = fonts_white,
  "Mary Westmacott" = fonts_white,
  "Hercule Poirot &\nAriadne Oliver" = color_black,
  "Tommy &\nTuppence" = fonts_white,
  "Ariadne Oliver" = fonts_white,
  "Harley Quin" = color_black,
  "Parker Pyne" = fonts_white
)

# Fonts ------------------------------------------------------------------

# font_add_google("Libre Baskerville", "libreB")
font_add(
  "libreB",
  regular = "fonts/LibreBaskerville-Regular.ttf",
  bold = "fonts/LibreBaskerville-Bold.ttf"
)


# font_add_google("Merriweather", "merriW")

font_add(
  "merriW",
  regular = "fonts/Merriweather-Regular.ttf",
  bold = "fonts/Merriweather-Bold.ttf"
)


font_add(
  "fa6-brands",
  here::here(
    "fonts/Font Awesome 6 Brands-Regular-400.otf"
  )
)

# Plot -------------------------------------------------------------------
theme_set(theme_minimal(base_size = 14, base_family = "merriW"))

make_tmap <- function(df, title) {
  df |>
    ggplot(aes(area = count, fill = series, color = series)) +
    geom_treemap(
      show.legend = FALSE,
      color = NA,
    ) +
    geom_treemap_text(
      aes(label = series),
      place = "topleft",
      padding.x = unit(4, units = "pt"),
      padding.y = unit(6, units = "pt"),
      reflow = TRUE,
      family = "merriW",
      fontface = "bold",
      size = 18,
      alpha = .84
    ) +
    geom_treemap_text(
      aes(label = scales::percent(pct)),
      place = "bottomleft",
      size = 24,
      min.size = 24,
      padding.x = unit(6, units = "pt"),
      padding.y = unit(6, units = "pt"),
      family = "merriW",
      fontface = "bold",
      alpha = .7
    ) +
    geom_treemap_text(
      aes(label = count),
      place = "bottomright",
      size = 12,
      padding.x = unit(4, units = "pt"),
      padding.y = unit(6, units = "pt"),
      family = "merriW",
      reflow = TRUE,
      fontface = "bold",
      alpha = .7
    ) +
    labs(title = title) +
    scale_fill_manual(values = pal) +
    scale_color_manual(values = colors) +
    coord_equal(
      expand = FALSE,
      clip = "off"
    ) +
    theme(
      plot.title.position = "plot",
      plot.title = element_textbox_simple(
        color = color_black,
        family = "libreB",
        face = "bold",
        size = 21,
        halign = 0,
        hjust = 0,
        margin = margin(b = 4)
      )
    )
}

p1 <- make_tmap(novels_data, "Novels")
p2 <- make_tmap(shorts_data, "Short Stories")

final_plot <- p1 +
  p2 +
  plot_annotation(
    title = title,
    subtitle = subtitle,
    caption = caption_text,
    theme = theme(
      plot.caption.position = "plot",
      plot.caption = element_textbox_simple(
        color = color_black,
        size = 8,
        halign = 1,
        hjust = 1,
        lineheight = 1.3,
        margin = margin(t = 12, b = 0)
      ),
      plot.title.position = "plot",
      plot.title = element_textbox_simple(
        family = "libreB",
        color = color_black,
        size = 32,
        face = "bold",
        halign = 0.5,
        hjust = 0.5,
        margin = margin(b = 4)
      ),
      plot.subtitle = element_textbox_simple(
        family = "merriW",
        color = color_black1,
        size = 14,
        halign = 0.5,
        hjust = 0.5,
        lineheight = 1.3,
        maxwidth = 70,
        margin = margin(b = 12)
      )
    )
  ) +
  theme(
    plot.background = element_rect(fill = fonts_white, color = fonts_white),
    text = element_text(color = color_black),
    plot.margin = margin(10, 8, 0, 8)
  )

ggsave(
  here::here("2025/charts/day1_fractions.png"),
  final_plot,
  height = 8.5,
  width = 11,
  units = "in",
  dpi = 320
)
