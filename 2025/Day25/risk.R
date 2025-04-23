# Packages ---------------------------------------------------------------

library(tidyverse)
library(ggsankey)
library(showtext)
library(ggtext)

# Data -------------------------------------------------------------------

danger_level <- c(
  "Venomous Bite - Extremely Dangerous",
  "Venomous Bite - Very Dangerous",
  "Venomous Bite - Dangerous",
  "Venomous Bite - Potentially Dangerous",
  "Venomous Bite - Unknown Dangerousness",
  "Venomous Bite - Harmless To Humans",
  "Unclear Toxicity",
  "Non-Venomous Constrictor - Extremely Dangerous",
  "Non-Venomous Constrictor - Very Dangerous",
  "Non-Venomous Constrictor - Dangerous",
  "Non-Venomous Constrictor - Potentially Dangerous",
  "Non-Venomous Constrictor - Harmless To Humans",
  "Non-Venomous",
  "Unknown"
)

venom_toxicity <- read_csv(here::here(
  "2025/data/Toxicity by Species  snakeDB.csv"
)) |>
  janitor::clean_names() |>
  drop_na(family) |>
  mutate(
    toxicity_dangerousness = str_to_title(str_replace_all(
      toxicity_dangerousness,
      "–",
      "-"
    )),
    toxicity_dangerousness = replace_na(toxicity_dangerousness, "Unknown")
  )

plot_data <- venom_toxicity |>
  # Create the data for ggsankey
  make_long(family, toxicity_dangerousness) |>
  mutate(
    # Useful for the colors
    fill_color = case_when(
      node %in% venom_toxicity$toxicity_dangerousness ~ node,
      TRUE ~ "family_color"
    ),
    # Arrange toxicity and dangerousness order
    node = fct_rev(fct_relevel(node, danger_level))
  ) |>
  arrange(node)

# Fonts ------------------------------------------------------------------

font_add(
  "fa6-brands",
  "fonts/Font Awesome 6 Brands-Regular-400.otf"
)

font_add(
  "inter",
  "fonts/Inter_18pt-Regular.ttf",
  "fonts/Inter_18pt-Bold.ttf"
)

showtext_auto()
showtext_opts(dpi = 600)

# Colors -----------------------------------------------------------------

color_bg <- "#f4f0ec"
color_black <- "#101a24"
color_title <- "#2d3741"
color_cap <- "#928b82"
color_gray <- "#3c4a56"
color_red <- "#8c2e2e"
color_blue <- "#6694bc"
color_green <- "#5c9864"

pal <- c(
  "Unclear Toxicity" = color_blue,
  "Non-Venomous" = color_green,
  "Venomous Bite - Extremely Dangerous" = color_red,
  "Venomous Bite - Very Dangerous" = "#b23b3b",
  "Venomous Bite - Dangerous" = "#d35f5f",
  "Venomous Bite - Potentially Dangerous" = "#e08080",
  "Venomous Bite - Unknown Dangerousness" = "#eaa7a7",
  "Venomous Bite - Harmless To Humans" = "#f8dad9",
  "Non-Venomous Constrictor - extremely Dangerous" = color_red,
  "Non-Venomous Constrictor - Very Dangerous" = "#b23b3b",
  "Non-Venomous Constrictor - Dangerous" = "#d35f5f",
  "Non-Venomous Constrictor - Potentially Dangerous" = "#e08080",
  "Non-Venomous Constrictor - Harmless To Humans" = "#f8dad9",
  "Unknown" = "#9a9a9a",
  "family_color" = "#9a9a9a"
)

# Texts ------------------------------------------------------------------

# Caption
data <- glue::glue(
  "**Data**: Snake Toxicity & Dangerousness by Species - https:\\/\\/snakeDB.org"
)
chart <- glue::glue(
  "**#30DayChartChallenge 2025, Day 25**: Uncertainties - Risk"
)
bsky <- glue::glue("<span style='font-family:fa6-brands'>&#xe671;</span>")
author <- glue::glue("**Graphic**: {bsky} @rajodm")
caption_text <- glue::glue("{data}<br>{chart} | {author} | #rstats")

# Text
title <- "Snake Species: Taxonomic Families and Risk Profiles"
subtitle <- glue::glue(
  "This diagram illustrates how snakes from 32 taxonomic families vary in their risk to humans. ",
  "Some are <span style='color: {color_red}'>**venomous**</span>, ",
  "others <span style='color: {color_red}'>**hazardous because of their size once fully grown**</span> (non-venomous constrictors),",
  " and some have <span style='color: {color_blue}'>**uncertain toxicity profiles**</span>.",
  " Some species, however, have <span style='color: {color_green}'>**minimal impact on human safety**</span> (non-venomous/harmless to humans)"
)

# plot -------------------------------------------------------------------

plot <- plot_data |>
  ggplot(aes(
    x = x,
    next_x = next_x,
    node = node,
    next_node = next_node,
    fill = fill_color,
    flow.fill = "#bcbcbc"
  )) +
  geom_sankey(
    flow.alpha = .6,
    alpha = .8,
    width = .5,
    smooth = 8,
    space = 380,
    color = "transparent",
    show.legend = FALSE
  ) +
  geom_sankey_text(
    aes(label = node),
    size = 3.8,
    space = 380,
    color = color_black,
  ) +
  scale_fill_manual(values = pal) +
  scale_x_discrete(
    labels = c("Family", "Toxicity & Dangerousness"),
    position = "top",
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_y_discrete(expand = expansion(mult = c(0.007, 0.007))) +
  coord_cartesian(
    clip = "off",
    expand = FALSE
  ) +
  labs(
    title = title,
    subtitle = subtitle,
    caption = caption_text
  ) +
  theme_sankey(base_family = "inter", base_size = 14) +
  theme(
    text = element_text(color = color_black),
    plot.background = element_rect(fill = color_bg, color = NA),
    panel.background = element_rect(fill = color_bg, color = NA),
    plot.margin = margin(1.2, 1, .5, 1, "cm"),
    axis.title.x = element_blank(),
    axis.text.x = element_markdown(
      family = "inter",
      size = 11,
      color = color_gray,
      margin = margin(b = 1)
    ),
    plot.title.position = "plot",
    plot.title = element_textbox_simple(
      color = color_black,
      face = "bold",
      size = 22,
      margin = margin(b = 4),
      halign = .5
    ),
    plot.subtitle = element_textbox_simple(
      family = "inter",
      color = color_title,
      size = 12,
      halign = .5,
      margin = margin(b = 14)
    ),
    plot.caption.position = "plot",
    plot.caption = element_textbox_simple(
      color = color_cap,
      size = 7,
      halign = 1,
      lineheight = 1.3
    )
  )

ggsave(
  "2025/charts/day25-risk.png",
  plot,
  width = 29.7,
  height = 21,
  units = "cm",
  dpi = 600
)
