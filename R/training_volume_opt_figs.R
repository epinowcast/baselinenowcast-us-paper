#' Get bar charts of WIS breakdown by pathogen and scale factor/prop_delay
#'
#' @param scores_su Data.frame of scores by pathogen, nowcast date, model,
#'   and reference date
#' @importFrom scoringutils summarise_scores
#' @importFrom ggplot2 geom_bar scale_alpha_manual facet_wrap scale_fill_manual
#'  scale_alpha_manual guides guide_legend xlab ylab theme element_blank
#' @importFrom scoringutils summarise_scores
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr distinct pull
#' @returns ggplot object
#' @autoglobal
get_bar_chart_tv_scores <- function(scores_su) {
  summary_scores <- scores_su |>
    summarise_scores(by = c(
      "prop_delay", "scale_factor",
      "pathogen_name", "pathogen"
    )) |>
    pivot_longer(cols = c("overprediction", "underprediction", "dispersion")) |>
    mutate(
      name = factor(name, levels = c(
        "overprediction",
        "dispersion",
        "underprediction"
      )),
      training_volume = glue::glue("prop_delay:{prop_delay}_scale_factor:{scale_factor}") # nolint
    )
  plot_comps <- plot_components()
  p <- ggplot(summary_scores) +
    geom_bar(
      aes(
        x = training_volume, y = value,
        alpha = name
      ),
      stat = "identity",
      position = "stack"
    ) +
    facet_wrap(~pathogen_name, scales = "free_y") +
    get_plot_theme() +
    scale_alpha_manual(
      name = "WIS breakdown",
      values = plot_comps$score_alpha
    ) +
    guides(
      # Can used fill = "none" if we want to remove color
      alpha = guide_legend(
        title.position = "top",
        title.hjust = 0.5,
        nrow = 3
      ),
      fill = guide_legend(
        title.position = "top",
        title.hjust = 0.5,
        nrow = 3
      )
    ) +
    xlab("") +
    ylab("WIS")


  return(p)
}

#' Get heatmap of WIS scores by proportion use for delay and scale factor on
#'   maximum delay
#'
#' @param scores_su Data.frame of scores by pathogen, nowcast date, model,
#'   and reference date
#' @param plot_title Character string indicating title of plot
#' @importFrom scoringutils summarise_scores
#' @importFrom ggplot2 geom_tile scale_alpha_manual facet_wrap
#'  scale_fill_viridis_c xlab ylab theme
#' @importFrom scoringutils summarise_scores
#' @importFrom dplyr distinct pull group_by mutate ungroup filter
#' @returns ggplot object
#' @autoglobal
get_plot_tv_scores <- function(scores_su,
                               title) {
  summary_scores <- scores_su |>
    summarise_scores(by = c(
      "prop_delay", "scale_factor",
      "pathogen_name", "pathogen"
    )) |>
    mutate(
      training_volume = glue::glue("prop_delay:{prop_delay}_scale_factor:{scale_factor}") # nolint
    ) |>
    group_by(pathogen) |>
    mutate(wis_scaled = (wis - min(wis)) / (max(wis) - min(wis))) |>
    ungroup()

  ggplot(summary_scores) +
    geom_tile(aes(x = prop_delay, y = scale_factor, fill = wis_scaled)) +
    scale_fill_viridis_c(name = "Relative WIS\n(within pathogen)") +
    get_plot_theme() +
    facet_wrap(~pathogen_name, scales = "free") +
    xlab("Proportion used for\ndelay estimation") +
    ylab("Scale factor on\nmaximum delay") +
    ggtitle(glue::glue("{title}"))
}

#' Get table of the minimum wis
#'
#' @param scores_su Data.frame of scores by pathogen, nowcast date, model,
#'   and reference date
#' @importFrom dplyr distinct pull group_by mutate ungroup filter
#' @importFrom scoringutils summarise_scores
get_table_min_wis <- function(scores_su) {
  summary_scores <- scores_su |>
    summarise_scores(by = c(
      "prop_delay", "scale_factor",
      "pathogen_name", "pathogen"
    )) |>
    mutate(
      training_volume = glue::glue("prop_delay:{prop_delay}_scale_factor:{scale_factor}") # nolint
    ) |>
    group_by(pathogen) |>
    filter(wis == min(wis))
  return(summary_scores)
}
