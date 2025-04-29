# Packages ---------------------------------------------------------------

library(tidyverse)
library(ggdist)
library(ggtext)
library(showtext)

# Data -------------------------------------------------------------------

reg <- read_csv(here::here("2025/data/SDG_REGION.csv")) |>
  janitor::clean_names()

income_group <- reg |>
  filter(str_detect(region_id, "^(UIS)")) |>
  select(region_id, country_id) |>
  filter(!str_detect(region_id, "World")) |>
  separate_wider_regex(
    cols = region_id,
    patterns = c(
      "UIS: ",
      uis_region = "\\w.*"
    )
  ) |>
  mutate(
    uis_region = str_replace_all(uis_region, "and ", "&\n")
  )

prep_future <- read_csv("2025/data/math_gpia.csv")

plot_data <- prep_future |>
  left_join(
    income_group,
    by = "country_id"
  ) |>
  arrange(country_id, indicator_id, desc(year)) |>
  slice_head(
    n = 1,
    by = c(country_id, indicator_id)
  ) |>
  separate_wider_regex(
    indicator_id,
    patterns = c(
      "PREP.*",
      educ_level = "[12]",
      ".*"
    )
  ) |>
  mutate(
    educ_level = case_when(
      educ_level == 1 ~ "Primary Education",
      TRUE ~ "Lower Secondary Education"
    ),
    educ_level = fct_rev(educ_level)
  )

# Fonts ------------------------------------------------------------------

font_add_google("Lato", "lato")

# font_add(
#   "lato",
#   "fonts/Lato-Regular.ttf",
#   "fonts/Lato-Bold.ttf"
# )

font_add_google("Work Sans", "workS")

# font_add(
#   "workS",
#   "fonts/WorkSans-Regular.ttf",
#   "fonts/WorkSans-Bold.ttf"
# )

font_add(
  "fa6-brands",
  "fonts/Font Awesome 6 Brands-Regular-400.otf"
)

showtext_auto()
showtext_opts(dpi = 300)


# Colors -----------------------------------------------------------------

color_bg <- "#f8f7f5"
color_grid <- "#e2e3e7"
axis_text <- "#7e8087"
color_sub <- "#4b4d54"
color_title <- "#1a1c23"
color_cap <- "#9a9da5"
color_blue <- "#c6dde8"
color_red <- "#e7756d"
color_blue1 <- "#71a4f7"
# Texts ------------------------------------------------------------------

# Subtitle
subtitle <- glue::glue(
  "This Chart shows **Gender Parity Index (GPI)** for children/young people **completing an education level and achieving minimum math proficiency**. A <span style= 'color: { color_blue1 }'>**GPI of 0.97 to 1.03 reflects gender parity**</span>.<br>Individual gray points correspond to countries' most recent data, with red dots showing regional medians GPI and horizontal bars representing the distribution (interquartile range)."
)

# Caption
data <- glue::glue(
  "**Data**: Sustainable Development Goal 4 - UNESCO Institute of Statistics"
)
chart <- glue::glue(
  "**#30DayChartChallenge 2025, Day 28**: Uncertainties - Inclusion"
)
bsky <- glue::glue("<span style='font-family:fa6-brands'>&#xe671;</span>")
author <- glue::glue("**Graphic**: {bsky} @rajodm")
caption_text <- glue::glue("{data}<br>{chart} | {author} | #rstats")

# Plot -------------------------------------------------------------------

# Annotation
indication <-
  tibble(
    x = c(0.90, 1.06),
    y = c(8.7, 8.7),
    label = c("← Advantage to Boys", "Advantage to Girls →")
  )

plot <- plot_data |>
  ggplot(aes(x = value, y = uis_region)) +
  geom_rect(
    aes(xmin = 0.93, xmax = 1.03, ymin = -Inf, ymax = Inf),
    fill = color_blue,
    alpha = .06
  ) +
  ggbeeswarm::geom_quasirandom(
    width = 0.16,
    size = 2.4,
    color = axis_text,
    alpha = .8
  ) +
  stat_pointinterval(
    color = color_title,
    point_color = color_red,
    point_alpha = .8,
    point_size = 4.6,
    shape = 18,
    alpha = .7
  ) +
  geom_text(
    data = indication,
    aes(x = x, y = y, label = label, hjust = if_else(x < 1, 1, 0)),
    color = axis_text,
    alpha = .8
  ) +
  facet_wrap(~educ_level) +
  scale_y_discrete(
    expand = expansion(mult = c(0.01, 0.12))
  ) +
  labs(
    title = "Gender Parity Index: Math Proficiency in Primary and Secondary Education",
    subtitle = subtitle,
    x = "Gender Parity Index",
    caption = caption_text
  ) +
  theme_minimal(base_size = 12, base_family = "workS") +
  theme(
    plot.background = element_rect(fill = color_bg, color = NA),
    panel.background = element_rect(fill = color_bg, color = NA),
    panel.grid = element_line(
      color = color_grid,
      linetype = "31",
      linewidth = .5
    ),
    panel.grid.minor = element_blank(),
    axis.text = element_text(color = axis_text),
    axis.title.x = element_text(
      color = color_sub,
      face = "bold",
      size = 10,
      hjust = 1,
      margin = margin(t = 8)
    ),
    axis.title.y = element_blank(),
    strip.text = element_text(
      color = color_sub,
      size = 11,
      hjust = 0,
      family = "lato",
      face = "bold"
    ),
    plot.title.position = "plot",
    plot.title = element_textbox_simple(
      family = "lato",
      size = 22,
      color = color_title,
      face = "bold",
      margin = margin(b = 6)
    ),
    plot.subtitle = element_textbox_simple(
      color = color_sub,
      size = 12,
      lineheight = 1.2,
      margin = margin(b = 12)
    ),
    plot.caption.position = "plot",
    plot.caption = element_textbox_simple(
      size = 8,
      color = color_cap,
      margin = margin(t = 12),
      halign = 0
    ),
    plot.margin = margin(.8, 1, .6, 1, "cm")
  )

ggsave(
  "2025/charts/day28-inclusion.png",
  plot,
  height = 21,
  width = 29.7,
  units = "cm"
)
