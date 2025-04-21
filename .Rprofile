options(
  tibble.print_max = 30,
  tibble.print_min = 30
)

clean_agatha_cols <- function(df) {
  df |>
    janitor::clean_names() |>
    rename_with(
      .cols = starts_with("title_"),
      \(x) str_remove(x, "title_")
    )
}
