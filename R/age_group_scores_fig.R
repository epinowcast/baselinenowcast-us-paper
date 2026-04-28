#' Get a plot illustrating nowcasts at certain dates for each age group for
#'   a specific pathogen
#'
#'
#' @param nowcasts Dataframe of the combined quantiles across
#'    horizons and nowcast dates
#' @param all_data Dataframe of just initial and final data
#' @param max_delay Integer indicating the maximum delay, used to create the
#'   evaluation data.
#' @param pathogen_i Character string indicating pathogen to plot.
#' @param nowcast_dates_to_plot Vector of character strings of the dates you
#'   wish to plot, default is `NULL` which will plot all of them
#' @importFrom glue glue
#' @importFrom ggplot2 aes ggplot ggtitle xlab ylab geom_line geom_ribbon
#'    facet_wrap scale_color_manual scale_fill_manual guide_legend
#'     scale_linetype_manual
#' @importFrom dplyr filter
#' @importFrom tidyr pivot_wider
#' @returns ggplot object
#' @autoglobal
get_plot_ag_nowcasts_vs_data <- function(nowcasts,
                                         all_data,
                                         max_delay,
                                         pathogen_i,
                                         nowcast_dates_to_plot = NULL) {
  nowcast_date_range <- c(
    min(nowcasts$nowcast_date),
    max(nowcasts$nowcast_date)
  )

  nc <- nowcasts |>
    filter(
      is.null(nowcast_dates_to_plot) |
        nowcast_date %in% c(nowcast_dates_to_plot),
      pathogen == pathogen_i
    ) |>
    mutate(nowcast_date_model = glue("{nowcast_date}-{model}")) |>
    pivot_wider(
      id_cols = c(
        "reference_date", "pathogen", "nowcast_date",
        "final_count", "initial_count", "model",
        "nowcast_date_model", "age_group"
      ),
      names_from = quantile_level,
      values_from = quantile_value,
      names_prefix = "q_"
    )
  data_only <- all_data |>
    filter(
      delay <= max_delay,
      pathogen == pathogen_i
    ) |>
    group_by(pathogen, end_of_week_reference_date, age_group) |>
    summarise(final_count = sum(count)) |>
    filter(
      end_of_week_reference_date <= nowcast_date_range[2],
      end_of_week_reference_date >= nowcast_date_range[1]
    )
  pathogen_name <- all_data |>
    filter(pathogen == pathogen_i) |>
    distinct(pathogen_name) |>
    pull(pathogen_name)
  plot_comps <- plot_components()
  n_age_groups <- all_data |>
    distinct(age_group) |>
    nrow()
  p <- ggplot() +
    geom_line(
      data = nc,
      aes(
        x = reference_date, y = `q_0.5`,
        color = model, group = nowcast_date_model
      )
    ) +
    geom_line(
      data = nc,
      aes(
        x = reference_date, y = initial_count,
        group = nowcast_date,
        linetype = "Data as of nowcast date"
      ),
      color = "gray",
      linewidth = 1
    ) +
    geom_vline(
      data = nc,
      aes(
        xintercept = nowcast_date,
        linetype = "Date of nowcast"
      ),
      color = "black"
    ) +
    geom_ribbon(
      data = nc,
      aes(
        x = reference_date,
        ymin = `q_0.025`,
        ymax = `q_0.975`, fill = model,
        group = nowcast_date_model,
        alpha = "95%"
      )
    ) +
    geom_line(
      data = data_only,
      aes(
        x = end_of_week_reference_date, y = final_count,
        linetype = "Final evaluation data"
      ),
      color = "red", linewidth = 0.5
    ) +
    facet_wrap(~age_group, nrow = n_age_groups, scales = "free_y") +
    get_plot_theme() +
    scale_x_date(
      date_breaks = "1 month",
      date_labels = "%b %Y"
    ) +
    scale_color_manual(
      name = "Model Specification",
      values = plot_comps$model_colors
    ) +
    # Add scale for the reference lines
    scale_linetype_manual(
      name = "Observed data",
      values = c(
        "Final evaluation data" = "solid",
        "Data as of nowcast date" = "solid",
        "Date of nowcast" = "dashed"
      ),
      breaks = c(
        "Final evaluation data",
        "Data as of nowcast date",
        "Date of nowcast"
      ),
      guide = guide_legend(
        override.aes = list(
          color = c(
            "Final evaluation data" = "red",
            "Data as of nowcast date" = "gray",
            "Date of nowcast" = "black"
          ),
          linewidth = 1
        )
      )
    ) +
    scale_fill_manual(
      name = "Model Specification",
      values = plot_comps$model_colors
    ) +
    scale_alpha_manual(
      name = "Prediction intervals",
      values = c(
        "95%" = 0.4,
        "50%" = 0.4
      ),
      guide = guide_legend(
        override.aes = list(
          alpha = c(
            "95%" = 0.4,
            "50%" = 0.4
          )
        )
      )
    ) +
    xlab("") +
    ylab(glue::glue("ED visits")) +
    ggtitle(glue::glue("Nowcasted ED visits due to {pathogen_name}")) +
    guides(
      color = guide_legend(
        title.position = "top",
        nrow = 3
      ),
      fill = guide_legend(
        title.position = "top",
        nrow = 3
      ),
      linetype = guide_legend(
        title.position = "top",
        nrow = 3
      ),
      alpha = guide_legend(title.position = "top")
    ) +
    theme(legend.position = "top")

  return(p)
}

#' Make age group nowcast comparison figure
#'
#' @param nowcasts_vs_data1 plot A
#' @param nowcasts_vs_data2 plot B
#' @param nowcasts_vs_data3 plot C
#' @param nowcasts_vs_data4 plot D
#' @param bar_chart_scores1 plot E
#' @param bar_chart_scores2 plot F
#' @param bar_chart_scores3 plot G
#' @param bar_chart_scores4 plot H
#' @param fig_file_name name of figure
#' @param fig_file_dir filepath to save figure
#'
#' @returns patchwork fig
#' @importFrom patchwork plot_annotation plot_layout
#' @importFrom fs dir_create
#' @autoglobal
make_ag_nowcast_comp_fig <- function(
  nowcasts_vs_data1,
  nowcasts_vs_data2,
  nowcasts_vs_data3,
  nowcasts_vs_data4,
  bar_chart_scores1,
  bar_chart_scores2,
  bar_chart_scores3,
  bar_chart_scores4,
  fig_file_name = NULL,
  fig_file_dir = file.path("output", "figs")
) {
  fig_layout <- "
  AABB
  CCDD
  EEFF
  GGHH
  "

  fig <- nowcasts_vs_data1 +
    nowcasts_vs_data2 +
    nowcasts_vs_data3 +
    nowcasts_vs_data4 +
    bar_chart_scores1 +
    bar_chart_scores2 +
    bar_chart_scores3 +
    bar_chart_scores4 +

    plot_layout(
      design = fig_layout,
      axes = "collect",
      guides = "collect"
    ) +
    plot_annotation(
      tag_levels = "A",
      tag_sep = "",
      theme = theme(
        legend.position = "top",
        legend.title = element_text(hjust = 0.5),
        plot.title = element_text(size = 20),
        legend.justification = "left",
        plot.tag = element_text(size = 20)
      )
    )
  if (!is.null(fig_file_name)) {
    dir_create(fig_file_dir)
    ggsave(
      plot = fig,
      filename = file.path(
        fig_file_dir,
        glue("{fig_file_name}.tiff")
      ),
      device = "tiff",
      dpi = 600,
      compression = "lzw",
      type = "cairo",
      width = 20,
      height = 12
    )
    ggsave(
      plot = fig,
      filename = file.path(
        fig_file_dir,
        glue("{fig_file_name}.png")
      ),
      width = 20,
      height = 12,
      dpi = 600
    )
  }

  return(fig)
}

#' Get a plot of decomposed WIS by age group for each model permutation
#'
#' @param scores Dataframe of the scores by age group
#'    and model
#' @param pathogen Character string indicating pathogen to plot
#' @param fig_file_name name of figure
#' @param fig_file_dir filepath to save figure
#' @autoglobal
#' @importFrom ggplot2 ggplot geom_bar aes labs scale_fill_manual
#'    geom_hline scale_y_continuous facet_grid
#' @importFrom dplyr mutate select
#' @importFrom tidyr pivot_wider
#' @importFrom fs dir_create
#' @returns ggplot object
get_bar_chart_by_ag <- function(scores,
                                pathogen,
                                fig_file_name = NULL,
                                fig_file_dir = file.path("output", "figs", "supp")) { # nolint
  scores_sum <- scores |>
    scoringutils::summarise_scores(by = c(
      "pathogen",
      "pathogen_name",
      "model",
      "age_group"
    )) |>
    pivot_longer(cols = c(
      "overprediction",
      "underprediction",
      "dispersion"
    )) |>
    mutate(name = factor(name, levels = c(
      "overprediction",
      "dispersion",
      "underprediction"
    ))) |>
    filter(pathogen == !!pathogen)

  pathogen_name <- scores_sum |>
    distinct(pathogen_name) |>
    pull()

  plot_comps <- plot_components()
  p <- ggplot(
    scores_sum,
    aes(
      x = model, y = value,
      alpha = name,
      fill = model
    )
  ) +
    geom_bar(stat = "identity", position = "stack") +
    scale_fill_manual(
      name = "Model",
      values = plot_comps$model_colors
    ) +
    get_plot_theme() +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      strip.placement = "outside",
      strip.background = element_rect(color = NA, fill = NA),
      legend.position = "bottom"
    ) +
    facet_grid(. ~ age_group, switch = "x", scales = "free_y") +
    scale_alpha_manual(
      name = "WIS breakdown",
      values = plot_comps$score_alpha
    ) +
    labs(x = "", y = "WIS") +
    guides(
      fill = guide_legend(
        nrow = 3,
        title.position = "top"
      ),
      alpha = guide_legend(
        nrow = 3,
        title.position = "top"
      )
    ) +
    ggtitle(glue::glue("{pathogen_name}"))

  if (!is.null(fig_file_name)) {
    dir_create(fig_file_dir)
    ggsave(
      plot = p,
      filename = file.path(
        fig_file_dir,
        glue("{fig_file_name}.png")
      ),
      width = 16,
      height = 8
    )
  }

  return(p)
}

#' Get a plot of the nowcasts over time by horizon and age group for a specific
#'   pathogen
#'
#' @param nowcasts Age group specific nowcasts
#' @param horizon_to_plot Integer indicating horizon (in weeks) to plot
#' @param age_group_to_plot Character string indicating the age group to plot
#' @param pathogen_to_plot Character string indicating what pathogen to plot
#' @param fig_file_name Character string indicating name of the figure
#' @param fig_file_dir Filepath to save figure
#'
#' @autoglobal
#' @returns ggplot object faceted by model showing nowcasts for the chosen
#'  horizon and age group
get_plot_nowcasts_over_time <- function(nowcasts,
                                        horizon_to_plot,
                                        age_group_to_plot = "00-04",
                                        pathogen_to_plot = "bar",
                                        fig_file_name = NULL,
                                        fig_file_dir = file.path(
                                          "output",
                                          "figs",
                                          "supp"
                                        )) {
  nc <- nowcasts |>
    mutate(
      horizon = as.integer(reference_date - nowcast_date) / 7
    ) |>
    filter(
      horizon == horizon_to_plot,
      age_group == age_group_to_plot,
      pathogen == pathogen_to_plot
    ) |>
    mutate(nowcast_date_model = glue("{nowcast_date}-{model}")) |>
    pivot_wider(
      id_cols = c(
        "reference_date", "pathogen", "nowcast_date",
        "final_count", "initial_count", "model",
        "nowcast_date_model", "age_group", "pathogen_name"
      ),
      names_from = quantile_level,
      values_from = quantile_value,
      names_prefix = "q_"
    )
  pathogen_name <- nc |>
    distinct(pathogen_name) |>
    pull()


  plot_colors <- plot_components()
  p <- ggplot(nc) +
    geom_ribbon(
      aes(
        x = reference_date,
        ymin = `q_0.025`,
        ymax = `q_0.975`,
        fill = model,
        alpha = "95%"
      )
    ) +
    geom_line(
      aes(
        x = reference_date, y = `q_0.5`,
        color = model
      ),
      show.legend = FALSE,
      linewidth = 1
    ) +
    geom_line(
      aes(
        x = reference_date, y = final_count,
        linetype = "Final evaluation data"
      ),
      color = "red",
      linewidth = 1
    ) +
    geom_line(
      aes(
        x = reference_date, y = initial_count,
        linetype = "Data as of nowcast date"
      ),
      color = "gray",
      linewidth = 1
    ) +
    scale_alpha_manual(
      name = "Prediction intervals",
      values = c(
        "95%" = 0.2
      ),
      guide = guide_legend(
        nrow = 2,
        title.position = "top"
      )
    ) +
    facet_wrap(~model, nrow = 3) +
    get_plot_theme() +
    scale_x_date(
      date_breaks = "1 month",
      date_labels = "%b %Y"
    ) +
    scale_color_manual(values = plot_colors$model_colors) +
    scale_fill_manual(
      name = "Model",
      values = plot_colors$model_colors,
      guide = guide_legend(
        title.position = "top",
        nrow = 3
      )
    ) +
    scale_linetype_manual(
      name = "Observed data",
      values = c(
        "Final evaluation data" = "solid",
        "Data as of nowcast date" = "solid"
      ),
      guide = guide_legend(
        title.position = "top",
        nrow = 3,
        override.aes = list(
          linewidth = 1
        )
      )
    ) +
    xlab("") +
    ylab("ED visits") +
    guides(
      color = "none"
    ) +
    theme(
      strip.placement = "outside",
      strip.background = element_rect(color = NA, fill = NA),
      legend.position = "top"
    ) +
    ggtitle(glue::glue("{pathogen_name}, {horizon_to_plot} week horizon, {age_group_to_plot}")) # nolint

  if (!is.null(fig_file_name)) {
    dir_create(fig_file_dir)
    ggsave(
      plot = p,
      filename = file.path(
        fig_file_dir,
        glue("{fig_file_name}.png")
      ),
      width = 16,
      height = 8
    )
  }

  return(p)
}
