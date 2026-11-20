#' Get standardized plot theme to add to figures
#' @param dates Boolean indicating whether the x-axis represents dates.
#' @returns a theme object to add to a [ggplot2::ggplot()] object
#' @autoglobal
#' @importFrom ggplot2 theme element_rect element_text
#' @importFrom cowplot theme_half_open background_grid
get_plot_theme <- function(dates = TRUE) {
  plot_theme <- theme_half_open() +
    background_grid() +
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
#' @returns a list of the model components
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
  model_colors <- c(
    "preliminary data" = "gray",
    "baselinenowcast daily" = "purple4",
    "baselinenowcast weekly" = "magenta3",
    "baselinenowcast weekly reference daily reports" = "salmon2",
    "baselinenowcast" = "purple4",
    "DPH method (real-time)" = "green4",
    "DPH method (retrospective)" = "olivedrab3",
    "DPH our implementation orig" = "red4",
    "DPH revised" = "orange2",
    "baselinenowcast strata sharing" = "turquoise4",
    "baselinenowcast strata sharing weekly" = "skyblue3",
    "baselinenowcast strata sharing weekly reference daily reports" = "blue4",
    "baselinenowcast strata sharing daily" = "turquoise4",
    "baselinenowcast base" = "purple4",
    "baselinenowcast base weekly" = "magenta3"
  )
  # Sequential purples (distinct from the age group colors), darkest for the
  # evaluation season
  season_colors <- c(
    "2023-2024" = "#B2ABD2",
    "2024-2025" = "#8073AC",
    "2025-2026" = "#2D004B"
  )
  score_alpha <- c(
    "overprediction" = 0.5,
    "underprediction" = 0.8,
    "dispersion" = 0.2
  )
  # nolint end

  plot_comp_list <-
    list(
      age_colors = age_colors,
      season_colors = season_colors,
      score_alpha = score_alpha,
      model_colors = model_colors
    )
  return(plot_comp_list)
}

#' Format age group labels for display
#'
#' Age groups are stored with zero-padded lower bounds (e.g. "00-04") so that
#' they sort correctly, and all ages combined are stored as "00+". This
#' converts them to readable labels for display in figures.
#'
#' @param x Character vector of age groups.
#'
#' @returns Character vector of age group labels, with "00+" shown as
#'   "All Ages", "00-04" as "0-4" and "05-17" as "5-17". Other values are
#'   returned unchanged.
format_age_group <- function(x) {
  display_labels <- c(
    "00+" = "All Ages", "00-04" = "0-4", "05-17" = "5-17"
  )
  out <- as.character(x)
  to_relabel <- out %in% names(display_labels)
  out[to_relabel] <- display_labels[out[to_relabel]]
  return(out)
}

#' Larger axis and legend text for multi-panel figures
#'
#' Applied to every panel of a patchwork figure with `&`. Only sets sizes of
#' elements that are already shown, so panels with hidden x axis text or
#' strips keep them hidden.
#'
#' @returns a theme object
#' @importFrom ggplot2 theme element_text
larger_axis_text <- function() {
  return(theme(
    axis.text.y = element_text(size = 20),
    axis.title = element_text(size = 22),
    legend.text = element_text(size = 20),
    legend.title = element_text(size = 20)
  ))
}

#' Format model names for display
#'
#' Models are stored with short names (e.g. "DPH method (real-time)"). This
#' converts them to the names used in the manuscript.
#'
#' @param x Character vector of model names.
#' @param wrap Boolean indicating whether to put the real-time/retrospective
#'   qualifier on a second line, for use in legends. Default is TRUE.
#'
#' @returns Character vector of model labels, with "DPH method" shown as
#'   "DPH multiplier method". Other values are returned unchanged.
format_model_label <- function(x, wrap = TRUE) {
  out <- sub("^DPH method", "DPH multiplier method", as.character(x))
  if (isTRUE(wrap)) {
    out <- sub("^(DPH multiplier method) \\(", "\\1\n(", out)
  }
  return(out)
}
