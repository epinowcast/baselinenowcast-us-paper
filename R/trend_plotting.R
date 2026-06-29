#' Plot trend accuracy by model and pathogen
#'
#' @param accuracy_data Data frame from calculate_trend_accuracy with columns:
#'   pathogen, pathogen_name, model, accuracy
#' @param title Character string for plot title
#' @param fig_file_name Character string for output filename (without extension)
#' @param fig_file_dir Character string for output directory
#' @importFrom ggplot2 ggplot aes geom_bar facet_wrap scale_fill_manual
#'   labs theme element_blank ggsave geom_text
#' @importFrom fs dir_create
#' @importFrom glue glue
#' @return ggplot object
#' @autoglobal
plot_trend_accuracy <- function(accuracy_data,
                                title = "Trend Prediction Accuracy",
                                fig_file_name = NULL,
                                fig_file_dir = file.path("output", "figs", "supp")) { # nolint
  plot_comps <- plot_components()

  p <- ggplot(accuracy_data) +
    geom_bar(
      aes(x = model, y = accuracy, fill = model),
      stat = "identity"
    ) +
    geom_text(
      aes(x = model, y = accuracy, label = sprintf("%.1f%%", accuracy)),
      vjust = -0.5,
      size = 3
    ) +
    facet_wrap(~pathogen_name, scales = "free_y") +
    get_plot_theme() +
    scale_fill_manual(
      name = "Model",
      values = plot_comps$model_colors
    ) +
    labs(
      title = title,
      x = "",
      y = "Accuracy (%)"
    ) +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      legend.position = "top"
    ) +
    guides(
      fill = guide_legend(
        title.position = "top",
        title.hjust = 0.5,
        nrow = 2
      )
    )

  if (!is.null(fig_file_name)) {
    dir_create(fig_file_dir)
    ggsave(
      plot = p,
      filename = file.path(fig_file_dir, glue("{fig_file_name}.png")),
      width = 12,
      height = 8,
      dpi = 600
    )
    ggsave(
      plot = p,
      filename = file.path(fig_file_dir, glue("{fig_file_name}.tiff")),
      device = "tiff",
      dpi = 600,
      compression = "lzw",
      type = "cairo",
      width = 12,
      height = 8
    )
  }

  return(p)
}

#' Plot trend accuracy stratified by trend category
#'
#' @param accuracy_by_category Data frame from
#'   calculate_trend_accuracy_by_category
#' @param title Character string for plot title
#' @param fig_file_name Character string for output filename
#' @param fig_file_dir Character string for output directory
#' @importFrom ggplot2 ggplot aes geom_bar facet_grid scale_fill_manual
#'   labs theme element_blank position_dodge ggsave
#' @importFrom fs dir_create
#' @importFrom glue glue
#' @return ggplot object
#' @autoglobal
plot_trend_accuracy_by_category <- function(
  accuracy_by_category,
  title = "Trend Prediction Accuracy by Category",
  fig_file_name = NULL,
  fig_file_dir = file.path("output", "figs", "supp")
) {
  plot_comps <- plot_components()

  # Ensure trend_observed is a factor with consistent ordering
  accuracy_by_category <- accuracy_by_category |>
    mutate(
      trend_observed = factor(
        trend_observed,
        levels = c("decreasing", "stable", "increasing")
      )
    )

  p <- ggplot(accuracy_by_category) +
    geom_bar(
      aes(x = trend_observed, y = accuracy, fill = model),
      stat = "identity",
      position = position_dodge(width = 0.8)
    ) +
    facet_wrap(~pathogen_name) +
    get_plot_theme() +
    scale_fill_manual(
      name = "Model",
      values = plot_comps$model_colors
    ) +
    labs(
      title = title,
      x = "Observed Trend",
      y = "Accuracy (%)"
    ) +
    theme(
      legend.position = "top",
      axis.text.x = element_text(angle = 45, hjust = 1)
    ) +
    guides(
      fill = guide_legend(
        title.position = "top",
        title.hjust = 0.5,
        nrow = 2
      )
    )

  if (!is.null(fig_file_name)) {
    dir_create(fig_file_dir)
    ggsave(
      plot = p,
      filename = file.path(fig_file_dir, glue("{fig_file_name}.png")),
      width = 12,
      height = 8,
      dpi = 600
    )
    ggsave(
      plot = p,
      filename = file.path(fig_file_dir, glue("{fig_file_name}.tiff")),
      device = "tiff",
      dpi = 600,
      compression = "lzw",
      type = "cairo",
      width = 12,
      height = 8
    )
  }

  return(p)
}

#' Plot confusion matrix heatmap for trend predictions
#'
#' @param confusion_matrix Data frame from create_trend_confusion_matrix
#' @param pathogen_filter Character string to filter to specific pathogen,
#'   NULL for all
#' @param title Character string for plot title
#' @param fig_file_name Character string for output filename
#' @param fig_file_dir Character string for output directory
#' @importFrom ggplot2 ggplot aes geom_tile geom_text facet_wrap
#'   scale_fill_gradient labs theme element_text coord_fixed ggsave
#' @importFrom fs dir_create
#' @importFrom glue glue
#' @importFrom dplyr filter
#' @return ggplot object
#' @autoglobal
plot_trend_confusion_matrix <- function(
  confusion_matrix,
  pathogen_filter = NULL,
  title = "Trend Prediction Confusion Matrix",
  fig_file_name = NULL,
  fig_file_dir = file.path("output", "figs", "supp")
) {
  data_to_plot <- confusion_matrix

  if (!is.null(pathogen_filter)) {
    data_to_plot <- data_to_plot |>
      filter(pathogen == pathogen_filter)
  }

  # Ensure consistent factor ordering
  data_to_plot <- data_to_plot |>
    mutate(
      trend_predicted = factor(
        trend_predicted,
        levels = c("increasing", "stable", "decreasing")
      ),
      trend_observed = factor(
        trend_observed,
        levels = c("increasing", "stable", "decreasing")
      )
    )

  p <- ggplot(data_to_plot) +
    geom_tile(
      aes(
        x = trend_observed,
        y = trend_predicted,
        fill = count
      ),
      color = "white"
    ) +
    geom_text(
      aes(
        x = trend_observed,
        y = trend_predicted,
        label = count
      ),
      color = "black",
      size = 4
    ) +
    facet_wrap(~ pathogen_name + model, ncol = 4) +
    scale_fill_gradient(
      low = "white",
      high = "steelblue",
      name = "Count"
    ) +
    get_plot_theme() +
    labs(
      title = title,
      x = "Observed Trend",
      y = "Predicted Trend"
    ) +
    coord_fixed() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "right"
    )

  if (!is.null(fig_file_name)) {
    dir_create(fig_file_dir)
    ggsave(
      plot = p,
      filename = file.path(fig_file_dir, glue("{fig_file_name}.png")),
      width = 16,
      height = 12,
      dpi = 600
    )
    ggsave(
      plot = p,
      filename = file.path(fig_file_dir, glue("{fig_file_name}.tiff")),
      device = "tiff",
      dpi = 600,
      compression = "lzw",
      type = "cairo",
      width = 16,
      height = 12
    )
  }

  return(p)
}

#' Plot trend accuracy over time
#'
#' @param accuracy_over_time Data frame with accuracy by nowcast_date
#' @param pathogen_filter Character string to filter to specific pathogen,
#'   NULL for all
#' @param title Character string for plot title
#' @param fig_file_name Character string for output filename
#' @param fig_file_dir Character string for output directory
#' @importFrom ggplot2 ggplot aes geom_line facet_wrap scale_color_manual
#'   labs theme scale_x_date ggsave geom_hline
#' @importFrom fs dir_create
#' @importFrom glue glue
#' @importFrom dplyr filter
#' @return ggplot object
#' @autoglobal
plot_trend_accuracy_over_time <- function(
  accuracy_over_time,
  pathogen_filter = NULL,
  title = "Trend Prediction Accuracy Over Time",
  fig_file_name = NULL,
  fig_file_dir = file.path("output", "figs", "supp")
) {
  data_to_plot <- accuracy_over_time

  if (!is.null(pathogen_filter)) {
    data_to_plot <- data_to_plot |>
      filter(pathogen == pathogen_filter)
  }

  plot_comps <- plot_components()

  p <- ggplot(data_to_plot) +
    geom_line(
      aes(x = nowcast_date, y = accuracy, color = model),
      linewidth = 1
    ) +
    geom_hline(
      yintercept = 33.33,
      linetype = "dashed",
      color = "gray50",
      alpha = 0.5
    ) +
    facet_wrap(~pathogen_name, ncol = 1, scales = "free_y") +
    get_plot_theme(dates = TRUE) +
    scale_x_date(
      date_breaks = "1 month",
      date_labels = "%b %Y"
    ) +
    scale_color_manual(
      name = "Model",
      values = plot_comps$model_colors
    ) +
    labs(
      title = title,
      x = "Nowcast Date",
      y = "Accuracy (%)"
    ) +
    theme(legend.position = "top") +
    guides(
      color = guide_legend(
        title.position = "top",
        title.hjust = 0.5,
        nrow = 2
      )
    )

  if (!is.null(fig_file_name)) {
    dir_create(fig_file_dir)
    ggsave(
      plot = p,
      filename = file.path(fig_file_dir, glue("{fig_file_name}.png")),
      width = 12,
      height = 10,
      dpi = 600
    )
    ggsave(
      plot = p,
      filename = file.path(fig_file_dir, glue("{fig_file_name}.tiff")),
      device = "tiff",
      dpi = 600,
      compression = "lzw",
      type = "cairo",
      width = 12,
      height = 10
    )
  }

  return(p)
}
