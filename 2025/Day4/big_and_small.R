# Packages ---------------------------------------------------------------

library(tidyverse)
library(showtext)
library(ggtext)
library(magick)

# Data -------------------------------------------------------------------

pokemon <- read_csv(here::here("2025/data/pokemon_df.csv")) |>
  drop_na(generation_id)

# Colors -----------------------------------------------------------------

color_black <- "#15151c"
color_black1 <- colorspace::lighten(color_black, 0.4)
color_black2 <- colorspace::lighten(color_black, 0.7)
color_white <- "#f4f0ec"
color_white1 <- colorspace::darken(color_white, 0.06)

pal <- c(
  "bug" = "#a9cc47",
  "dark" = "#403237",
  "dragon" = "#204e63",
  "electric" = "#e5c531",
  "fairy" = "#f1b1e8",
  "fighting" = "#cb5f48",
  "fire" = "#ea7a3c",
  "flying" = "#b1cfee",
  "ghost" = "#7b6292",
  "grass" = "#5bb051",
  "ground" = "#cc9f4f",
  "ice" = "#70cbd4",
  "normal" = "#bcbbb0",
  "poison" = "#bd52a8",
  "psychic" = "#ff5f8f",
  "rock" = "#b2a061",
  "steel" = "#6f6e68",
  "water" = "#1a5db0"
)

# Fonts ------------------------------------------------------------------

font_add_google("Press Start 2P", "psp")
font_add_google("Quick Sans", "quickS")

# font_add(
#   "psp",
#   "fonts/PressStart2P-Regular.ttf"
# )

# font_add(
#   "quickS",
#   regular = "fonts/Quicksand-Regular.ttf",
#   bold = "fonts/Quicksand-Bold.ttf"
# )

font_add(
  "fa6-brands",
  "fonts/Font Awesome 6 Brands-Regular-400.otf"
)

showtext_auto()
showtext_opts(dpi = 320)

# Text -------------------------------------------------------------------

# Caption
data <- glue::glue("**Data**: {{pokemon}}")
chart <- glue::glue(
  "<b>#30DayChartChallenge 2025, Day 4</b>: Comparisons - Big or Small"
)
bsky <- glue::glue("<span style='font-family:fa6-brands'>&#xe671;</span>")
author <- glue::glue("**Graphic**: {bsky} @rajodm")

caption_text <- glue::glue("{chart} | {data} | {author} | #rstats")

# Text
title <- "Pokémon Heights & Weights from Gen. 1 to Gen. 7"

# icon
make_icon <- function(img_path, pixel_size = 4) {
  # make the icon look retro
  img <- image_read(img_path)
  orig_width <- image_info(img)$width
  orig_height <- image_info(img)$height

  pixel_width <- ceiling(orig_width / pixel_size)
  pixel_height <- ceiling(orig_height / pixel_size)

  new_img <- img |>
    image_resize(
      glue::glue("{pixel_width}x{pixel_height}!"),
      filter = "point"
    ) |>
    image_quantize(max = 256, dither = TRUE) |>
    image_resize(glue::glue("{orig_width}x{orig_height}!"), filter = "point") |>
    image_modulate(brightness = 110, saturation = 125) |>
    image_contrast(sharpen = 1)

  new_img |>
    grid::rasterGrob(interpolate = TRUE)
}

icon1 <- make_icon("2025/assets/icon1.png")
icon2 <- make_icon("2025/assets/icon2.png")
icon3 <- make_icon("2025/assets/icon3.png")

# A function to add icon to the ggplot
add_icon <- function(icon, xmin, ymin = -0.5, ymax) {
  annotation_custom(
    icon,
    xmin = xmin,
    ymin = ymin,
    ymax = ymax,
  )
}
# Plot -------------------------------------------------------------------

theme_set(theme_minimal(base_size = 14, base_family = "quickS"))

p <- pokemon |>
  ggplot(aes(x = generation_id, y = height, color = type_1)) +
  geom_point(
    aes(size = weight),
    position = position_jitter(width = 0.26, seed = 123)
  ) +
  scale_color_manual(values = pal) +
  scale_size_continuous(
    breaks = scales::breaks_pretty()
  ) +
  scale_x_continuous(
    breaks = c(1:7),
    labels = c("I", "II", "III", "IV", "V", "VI", "VII")
  ) +
  scale_y_continuous(breaks = scales::breaks_pretty()) +
  labs(
    title = title,
    color = "Primary type",
    size = "Weight (kg)",
    y = "Height (m)",
    x = "Generation",
    caption = caption_text
  ) +
  coord_cartesian(
    clip = "off",
    ylim = c(0, 15),
    expand = FALSE
  ) +
  add_icon(icon2, xmin = 9.5, ymax = 2.12) +
  add_icon(icon1, xmin = 9, ymax = 0.5) +
  add_icon(icon3, xmin = 12.5, ymax = 0.59) +
  theme(
    plot.background = element_rect(fill = color_white, color = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(
      linetype = "dashed",
      linewidth = 0.4,
      color = color_black2
    ),
    plot.title.position = "plot",
    plot.title = element_textbox_simple(
      family = "psp",
      size = 22,
      color = color_black,
      lineheight = 1.3,
      maxwidth = 98,
      margin = margin(b = 32)
    ),
    axis.text = element_text(family = "psp", size = 9),
    axis.title = element_text(family = "psp", size = 10),
    axis.title.x = element_text(margin = margin(t = 10)),
    legend.title = element_text(family = "psp", size = 10, face = "bold"),
    legend.location = "plot",
    legend.position = "right",
    legend.justification = c(1, 1),
    legend.title.position = "top",
    legend.text = element_text(size = 14),
    plot.caption.position = "plot",
    plot.caption = element_textbox_simple(
      color = color_black1,
      size = 9,
      halign = 0.5,
      margin = margin(t = 18, b = 15)
    ),
    plot.margin = margin(25, 25, 20, 25)
  ) +
  guides(
    color = guide_legend(
      ncol = 2,
      order = 1,
      override.aes = list(
        shape = 15,
        size = 4.2
      )
    ),
    size = guide_legend(
      ncol = 2,
      order = 2,
      override.aes = list(
        color = color_black2
      ),
      theme = theme(
        legend.key.spacing.x = unit(10, units = "mm")
      )
    )
  )

# Saving the plot --------------------------------------------------------

ggsave(
  "2025/charts/day4-big_or_small.png",
  p,
  width = 8.5,
  height = 11,
  units = "in",
  dpi = 320
)
