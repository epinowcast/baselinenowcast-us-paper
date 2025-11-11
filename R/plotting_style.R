#' Get standardized plot theme to add to figures
#' @param dates Boolean indicating whether the x-axis represents dates.
#' @returns a theme object to add to a [ggplot2::ggplot()] object
#' @autoglobal
#' @importFrom ggplot2 theme element_rect element_text
#' @importFrom cowplot theme_half_open background_grid
get_plot_theme <- function(dates = TRUE) {
  plot_theme <- cowplot::theme_half_open() +
    cowplot::background_grid() +
    theme(
      plot.background = element_rect(fill = "white"),
      legend.text = element_text(size = 16),
      plot.title = element_text(size = 20),
      legend.title = element_text(size = 16),
      axis.text.x = element_text(size = 16),
      axis.text.y = element_text(size = 16),
      axis.title = element_text(size = 16),
      strip.text = element_text(size = 16),
      strip.background = element_rect(fill = "white")
    )
  if (isTRUE(dates)) {
    plot_theme <- plot_theme +
      theme(
        axis.text.x = element_text(
          vjust = 1,
          hjust = 1,
          angle = 45,
          size = 11
        )
      )
  }

  return(plot_theme)
}

#' Get plot components (colors and shapes)
#'
#' @returns a list of the model colors to be passed to `scale_fill_manual` and
#'    `scale_color_manual`
#' @autoglobal
#' @importFrom RColorBrewer brewer.pal
plot_components <- function() {
  pal_age_groups <- brewer.pal(6, "Spectral")
  # nolint start
  age_colors <- c(
    "00+" = "black",
    "00-04" = pal_age_groups[1],
    "05-17" = pal_age_groups[2],
    "18-44" = pal_age_groups[3],
    "45-64" = pal_age_groups[5],
    "65+" = pal_age_groups[6],
    "Unknown" = "gray"
  )
  # nolint end

  plot_comp_list <-
    list(
      age_colors = age_colors
    )
  return(plot_comp_list)
}
