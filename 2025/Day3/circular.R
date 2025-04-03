# Packages ---------------------------------------------------------------

library(tidyverse)
library(ggradar)
library(ggtext)
library(patchwork)
library(showtext)

# Data -------------------------------------------------------------------

palmtrees <- palmtrees::palmtrees

plot_data <- palmtrees |>
  # Remove missing values from stem's characteristics
  select(palm_subfamily, understorey_canopy, stem_armed, leaves_armed) |>
  filter(if_all(
    c(understorey_canopy, stem_armed, leaves_armed),
    \(x) !is.na(x)
  )) |>
  summarize(
    .by = palm_subfamily,
    "Stem height > 5m" = mean(understorey_canopy %in% c("canopy", "both")),
    "Armed\nstem" = mean(stem_armed == "armed"),
    "Armed\nleaves" = mean(leaves_armed == "armed"),
    # na.rm is not necessary here
    "Stem height ≤ 5m" = mean(
      understorey_canopy == "understorey"
    ),
    "Non-armed\nstem" = mean(stem_armed == "non-armed"),
    "Non-armed\nleaves" = mean(leaves_armed == "non-armed"),
    species_count = n(),
  ) |>
  mutate(
    across(
      !c(palm_subfamily, species_count),
      \(x) scales::rescale(x, to = c(0, 1))
    )
  ) |>
  nest(data = !c(palm_subfamily, species_count)) |>
  arrange(desc(species_count)) |>
  nest(subfamilies = !data)

# Colors -----------------------------------------------------------------

color_green <- "#5d9865"
color_green_darker <- "#1a3f2c"
color_black <- "#080813"
color_black1 <- colorspace::lighten(color_black, 0.7)
color_white <- "#f8f4ed"

# Fonts ------------------------------------------------------------------

font_add_google("Space Grotesk", family = "spaceG")

font_add(
  "fa6-brands",
  here::here(
    "fonts/Font Awesome 6 Brands-Regular-400.otf"
  )
)

showtext_auto()
showtext_opts(dpi = 320)

# Texts ------------------------------------------------------------------

chart_title <- "Palm Structure: Height and Armature"


chart_subtitle <- "Each chart displays height and defensive features (spines on stems or leaves) across five palm subfamilies. Only species with complete data for both characteristics are represented."

data <- glue::glue("**Data**: {{palmtrees}}")
chart <- glue::glue(
  "<b>#30DayChartChallenge 2025, Day 3</b>: Comparisons - Circular"
)
bsky <- glue::glue("<span style='font-family:fa6-brands'>&#xe671;</span>")
author <- glue::glue("**Graphic**: {bsky} @rajodm")

caption_text <- glue::glue("{chart} | {data} | {author} | #rstats")
# Plot -------------------------------------------------------------------

theme_set(theme_minimal(base_family = "spaceG"))

make_radar <- function(dataset, ...) {
  # ggradar specs
  ggradar(
    dataset,
    font.radar = "spaceG",
    base.size = 12,
    grid.line.width = 0.3,
    axis.line.colour = color_black1,
    axis.label.offset = 1.23,
    axis.label.size = 3.8,
    plot.extent.y.sf = 1.5,
    legend.position = "none",
    label.gridline.min = FALSE,
    label.gridline.mid = FALSE,
    label.gridline.max = FALSE,
    background.circle.colour = color_white,
    gridline.min.colour = color_black1,
    gridline.mid.colour = color_black1,
    gridline.max.colour = color_black1,
    group.line.width = 0.8,
    group.point.size = 2.3,
    background.circle.transparency = 0,
    fill = TRUE,
    ...
  ) +
    # !important for the result
    coord_equal(
      clip = "off"
    ) +
    theme(
      plot.background = element_rect(color = color_white, fill = color_white),
      panel.background = element_rect(color = color_white, fill = color_white),
      plot.title.position = "plot",
      plot.title = element_textbox_simple(
        face = "bold",
        color = color_green_darker,
        lineheight = 1,
        halign = 0.5,
        size = 18
      ),
      plot.margin = margin(8, 8, 8, 8)
    )
}

plots <- plot_data |>
  mutate(
    plot = map2(data, subfamilies, function(df, grp) {
      df |>
        mutate(
          x = row_number(),
          .before = 1
        ) |>
        make_radar(
          plot.title = glue::glue(
            "{grp$palm_subfamily}<br>",
            "<span style='font-size:12pt'>({grp$species_count} species)</span>"
          ),
          group.colours = color_green
        )
    })
  ) |>
  pull(plot)


update_geom_defaults(
  "text",
  list(
    color = color_green_darker,
    lineheight = 1,
    family = "spaceG"
  )
)

# A layout for the final graph
layout <- "
AABBCC
AABBCC
#DDEE#
#DDEE#
"

final_plot <- wrap_plots(plots) +
  plot_annotation(
    title = chart_title,
    subtitle = chart_subtitle,
    caption = caption_text,
    theme = theme(
      plot.title.position = "plot",
      plot.title = element_textbox_simple(
        face = "bold",
        halign = 0.5,
        size = 28,
        color = color_green_darker,
        margin = margin(b = 6)
      ),
      plot.subtitle = element_textbox_simple(
        color = color_green_darker,
        halign = 0.5,
        size = 12,
        lineheight = 1.3,
        margin = margin(b = 12)
      ),
      plot.caption.position = "plot",
      plot.caption = element_textbox_simple(
        color = color_green_darker,
        size = 9,
        halign = 0.5,
        margin = margin(t = 12)
      ),
      plot.margin = margin(20, 20, 20, 20)
    )
  ) +
  plot_layout(
    design = layout
  )


# Saving the plot --------------------------------------------------------

ggsave(
  here::here("2025/charts/day3_circular.png"),
  final_plot,
  height = 8.5,
  width = 11,
  units = "in",
  dpi = 320
)
