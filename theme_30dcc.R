add_caption <- function(source, title, note = NULL, year = 2026, day) {
  author <- "Andriambelo Rajo"
  glue::glue(
    if_else(!is.null(note), "**Note**: {note}<br>", ""),
    "**Data source**: {source}<br>",
    "**#30DayChartChallenge {year}, Day {day}**: {title} | ",
    "**Graphic**: {author} | #rstats"
  )
}

theme_30dcc <- function(
  base_size = 14,
  grid = "both",
  base_family = "Atkinson Hyperlegible Next",
  caption.hjust = 1,
  pal = palettes
) {
  gg <- ggplot2::theme_minimal(
    base_size = base_size,
    base_family = base_family,
    ink = pal$ink,
    paper = pal$paper
  ) +
    ggplot2::theme_sub_panel(
      background = ggplot2::element_rect(fill = pal$paper, color = NA),
      grid.major = ggplot2::element_line(
        color = pal$color_grid,
        linewidth = 0.3,
        linetype = "31"
      ),
      grid.minor = ggplot2::element_blank()
    ) +
    ggplot2::theme_sub_plot(
      background = element_rect(fill = pal$paper, color = NA),
      title.position = "plot",
      title = ggtext::element_textbox_simple(
        size = base_size * 1.3,
        face = "bold",
        family = "Syne",
        color = pal$ink,
        lineheight = 1.1,
        margin = ggplot2::margin(b = 8)
      ),
      subtitle = ggtext::element_textbox_simple(
        size = base_size,
        color = pal$color_sub,
        margin = ggplot2::margin(b = 12)
      ),
      caption.position = "plot",
      caption = ggtext::element_textbox_simple(
        size = base_size * 0.75,
        color = pal$color_muted,
        halign = caption.hjust,
        margin = ggplot2::margin(t = 14)
      ),
      margin = ggplot2::margin(16, 16, 16, 16)
    ) +
    ggplot2::theme_sub_axis(
      text = ggplot2::element_text(
        size = base_size * 0.85,
        color = pal$ink
      ),
      title = ggplot2::element_text(
        size = base_size * 0.9,
        color = pal$ink
      )
    ) +
    ggplot2::theme_sub_legend(
      text = ggplot2::element_text(
        size = base_size * 0.85,
      ),
      title = ggplot2::element_text(
        size = base_size * 0.9,
        face = "bold"
      ),
      key.size = ggplot2::unit(0.9, "lines"),
      background = ggplot2::element_rect(fill = pal$paper, color = NA)
    ) +
    ggplot2::theme_sub_legend(
      position = "none"
    )

  if (grid == "y") {
    gg <- gg + ggplot2::theme_sub_panel(grid.major.x = ggplot2::element_blank())
  } else if (grid == "x") {
    gg <- gg + ggplot2::theme_sub_panel(grid.major.y = ggplot2::element_blank())
  } else if (grid == "none") {
    gg <- gg +
      ggplot2::theme_sub_panel(
        grid.major.y = ggplot2::element_blank(),
        grid.major.x = ggplot2::element_blank()
      )
  }

  gg
}
