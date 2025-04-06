# Packages ---------------------------------------------------------------

library(tidyverse)
library(showtext)
library(ggtext)
library(ggarrow)
library(colorspace)

# Data -------------------------------------------------------------------

# Levels to arrange the order of each series in the plot
char_level <- c(
  "Ariadne Oliver",
  "Tommy &<br>Tuppence",
  "Hercule Poirot &<br>Ariadne Oliver",
  "Mary Westmacott",
  "Miss Marple",
  "Hercule Poirot",
  "Not part of a series"
)

novels <- read_csv(here::here("2025/data/agatha_novels.csv")) |>
  # Can be found in .Rprofile
  clean_agatha_cols() |>
  rename(pub_year = year_of_first_publication) |>
  mutate(
    decade = pub_year %/% 10 * 10,
    # Add line break after the '&'
    series = str_replace_all(series, "& ", "&<br>"),
    series = factor(series, levels = fct_relevel(char_level))
  )

last_book <- novels |>
  filter(pub_year == max(pub_year)) |>
  pull(name)

last_pub <- novels |>
  filter(name == last_book) |>
  pull(pub_year)

plot_data <- novels |>
  summarize(
    .by = c(decade, series),
    count = n()
  )

# Colors -----------------------------------------------------------------

pal <- c(
  "Hercule Poirot" = "#133051",
  "Not part of a series" = "#fcc192",
  "Miss Marple" = "#d14d41",
  "Mary Westmacott" = "#4385be",
  "Hercule Poirot &<br>Ariadne Oliver" = "#87d3c3",
  "Tommy &<br>Tuppence" = "#4f3685",
  "Ariadne Oliver" = "#1c6c66",
  "Harley Quin" = "#dfb431",
  "Parker Pyne" = "#768d21"
)

color_black <- "#333333"
color_black1 <- lighten(color_black, 0.2)
color_black2 <- lighten(color_black, 0.6)
color_white <- "#f8f4ed"

# Fonts ------------------------------------------------------------------

font_add_google("Lora", "lora")
# font_add(
#   "lora",
#   regular = "fonts/Lora-Regular.ttf",
#   bold = "fonts/Lora-Bold.ttf"
# )

font_add_google("Playfair Display", "playFD")
# font_add(
#   "playFD",
#   regular = "fonts/PlayfairDisplay-Regular.ttf",
#   bold = "fonts/PlayfairDisplay-Bold.ttf"
# )

font_add(
  "fa6-brands",
  "fonts/Font Awesome 6 Brands-Regular-400.otf"
)

showtext_auto()
showtext_opts(dpi = 320)

# Texts ------------------------------------------------------------------

# Caption
data <- glue::glue("**Data**: Agatha Christie's bibliography by Nicole Mark")
chart <- glue::glue(
  "**#30DayChartChallenge 2025, Day 6**: Comparisons - Florence Nightingale"
)
bsky <- glue::glue("<span style='font-family:fa6-brands'>&#xe671;</span>")
author <- glue::glue("**Graphic**: {bsky} @rajodm")

caption_text <- glue::glue("{chart} | {data}<br>{author} | #rstats")

# Text
title <- "Agatha Christie\\'s Novels by Series and Decades"

# Plot -------------------------------------------------------------------

theme_set(theme_minimal(base_size = 14, base_family = "lora"))

p <- plot_data |>
  ggplot(aes(x = decade, y = count, fill = series)) +
  geom_bar(
    stat = "identity",
    width = 10,
    color = color_black,
    linewidth = 0.15
  ) +
  scale_fill_manual(values = pal) +
  scale_x_continuous(
    breaks = scales::breaks_pretty(),
    minor_breaks = scales::breaks_pretty()
  ) +
  scale_y_continuous(
    # Remove the 0
    breaks = seq(5, 20, 5)
  ) +
  coord_radial(
    clip = "off",
    expand = FALSE,
    # Show axis text inside the plot
    r.axis.inside = TRUE
  ) +
  annotate_arrow(
    x = c(1920.5, 1923.5),
    y = c(21.3, 21.3),
    arrow_head = arrow_head_line(),
    arrow_fins = arrow_fins_minimal(angle = 0),
    linewidth = 0.4,
    color = color_black2
  ) +
  annotate_arrow(
    x = c(2010, 1992),
    y = c(1.1, 8),
    arrow_head = arrow_head_minimal(),
    arrow_fins = arrow_fins_minimal(),
    linewidth = 0.4,
    color = color_black2
  ) +
  geom_textbox(
    data = novels |>
      filter(pub_year == max(pub_year)),
    aes(
      x = 1988,
      y = 10,
      label = glue::glue(
        "**Last Novel**:<br>{name};<br>Published in {pub_year}"
      )
    ),
    family = "lora",
    colour = color_black1,
    # Remove the fill and color of the box
    fill = NA,
    box.colour = NA,
    # Useless because we removed the fill and color of the box
    # box.r = unit(0, "pt")
  ) +
  labs(
    title = title,
    fill = "Book Series",
    caption = caption_text
  ) +
  theme(
    text = element_text(color = color_black),
    plot.background = element_rect(fill = color_white, color = color_white),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(
      linewidth = 0.18,
      linetype = "dashed",
      color = color_black2
    ),
    plot.title.position = "plot",
    plot.title = element_textbox_simple(
      family = "playFD",
      size = 22,
      face = "bold",
      halign = 0.5
    ),
    plot.caption.position = "plot",
    plot.caption = element_textbox_simple(
      family = "lora",
      size = 8,
      color = color_black1,
      halign = 0.5,
      lineheight = 1.3,
      margin = margin(t = 12)
    ),
    axis.text = element_text(family = "lora", color = color_black2),
    axis.text.x = element_text(
      family = "lora",
      color = color_black1
    ),
    axis.title = element_blank(),
    legend.position = "inside",
    legend.position.inside = c(1.04, 0.22),
    legend.key.spacing.y = unit(1.8, "mm"),
    legend.title = element_markdown(
      size = 13,
      face = "bold",
      family = "playFD"
    ),
    legend.text = element_markdown(
      size = 11,
      lineheight = 1.2,
      family = "lora"
    ),
    plot.margin = margin(25, 25, 10, 25)
  )

# Saving the plot --------------------------------------------------------

ggsave(
  "2025/charts/day6-Florence_Nightingale.png",
  p,
  height = 8.5,
  width = 11,
  units = "in",
  dpi = 320
)
