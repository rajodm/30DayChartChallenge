# Packages ---------------------------------------------------------------

library(tidyverse)
library(circlize)
library(showtext)

# Fonts ------------------------------------------------------------------

# font_add_google("Pres Start 2P", "psp")
font_add(
  "psp",
  "fonts/PressStart2P-Regular.ttf"
)

# font_add_google("Quick Sand", "quickS")
font_add(
  "quickS",
  regular = "fonts/Quicksand-Regular.ttf",
  bold = "fonts/Quicksand-Bold.ttf"
)

font_add(
  "fa6-brands",
  "fonts/Font Awesome 6 Brands-Regular-400.otf"
)

# Data -------------------------------------------------------------------

pokemon <- read_csv(here::here("2025/data/pokedex.csv")) |>
  janitor::clean_names()

pokemon <- pokemon |>
  # create a column for primary and secondary type
  mutate(
    type = str_remove_all(type, "[{}]")
  ) |>
  separate_wider_delim(
    cols = type,
    delim = ",",
    names = c("type_1", "type_2"),
    too_few = "align_start"
  ) |>
  mutate(
    across(
      starts_with("type"),
      str_to_title
    )
  )

pokemon_types <- pokemon |>
  select(id, starts_with("type")) |>
  # Remove pokémon with only a primary type
  drop_na(type_2) |>
  mutate(
    # * if we want to show all pokémons
    # type_2 = if_else(is.na(type_2), type_1, type_2),
    type_1 = str_to_title(type_1),
    type_2 = str_to_title(type_2),
  )

type_order <- pokemon_types |>
  select(starts_with("type")) |>
  pivot_longer(
    cols = everything(),
    values_drop_na = TRUE
  ) |>
  count(value, sort = TRUE) |>
  pull(value)


chord_data <- pokemon_types |>
  summarize(
    .by = c(type_1, type_2),
    comb = n(),
  ) |>
  mutate(
    type_1 = factor(type_1, levels = type_order),
    type_2 = factor(type_2, levels = type_order),
  )

# Texts ------------------------------------------------------------------

# Caption
data <- glue::glue("Data: Pokédex For All 1025 Pokémon, curated by Robert Giza")
chart <- glue::glue(
  "#30DayChartChallenge 2025, Day 15: Relationships - Complicated"
)
author <- glue::glue("Visualization: @rajodm.bsky.social")

caption_text <- glue::glue(
  "{chart} | {data}\n{author} | #rstats - {{circlize}}"
)

# Texts
title <- "Pokémon Type Combinations"
subtitle <- glue::glue(
  "Of the {length(pokemon$name)} Pokémon from Gen I to Gen IX, {sum(!is.na(pokemon$type_2))} have a secondary type.\nThis chord diagram maps the connections between primary and secondary types\nacross the franchise."
)

showtext_auto()
showtext_opts(dpi = 320)

# Colors -----------------------------------------------------------------

pal <- c(
  "Bug" = "#a9cc47",
  "Dark" = "#403237",
  "Dragon" = "#204e63",
  "Electric" = "#e5c531",
  "Fairy" = "#f1b1e8",
  "Fighting" = "#cb5f48",
  "Fire" = "#ea7a3c",
  "Flying" = "#b1cfee",
  "Ghost" = "#7b6292",
  "Grass" = "#5bb051",
  "Ground" = "#cc9f4f",
  "Ice" = "#70cbd4",
  "Normal" = "#bcbbb0",
  "Poison" = "#bd52a8",
  "Psychic" = "#ff5f8f",
  "Rock" = "#b2a061",
  "Steel" = "#6f6e68",
  "Water" = "#1a5db0"
)

color_white <- "#f4f0ec"
color_black <- "#101a24"
color_cap <- colorspace::darken(color_white, 0.45)

# Plot -------------------------------------------------------------------

# Start saving the plot
ragg::agg_png(
  "day15-complicated.png",
  width = 8.5,
  height = 11,
  units = "in",
  res = 320,
  background = color_white
)

par(
  family = "quickS",
  bg = color_white,
  cex = 1.2,
  col = color_black,
  mar = c(1, 1, 4, 1)
)

chordDiagramFromDataFrame(
  chord_data,
  order = type_order,
  link.border = color_white,
  link.lwd = 0.8,
  grid.col = pal,
  transparency = 0.45,
  annotationTrack = c("name", "grid"),
  annotationTrackHeight = mm_h(c(2, 3)),
  link.largest.ontop = TRUE
)
# Add Title
mtext(
  title,
  side = 3,
  line = 1.4,
  cex = 1.7,
  font = 2,
  family = "psp",
)
# Add Subtitle
mtext(
  subtitle,
  side = 3,
  line = -1.3,
  cex = 1.05,
  font = 1,
  family = "quickS",
)
# Add Caption
mtext(caption_text, side = 1, line = -1, cex = 0.7, adj = 0.5, col = color_cap)

# End
invisible(dev.off())
