#' Cases over time by age group and pathogen plot
#'
#' @param weekly_data Dataframe of cases by reference date, report date, week,
#'   and pathogen
#' @param season_to_plot Character string indicating what season to plot,
#'   default is NULL which will include all dates in the data
#' @param by_age_group Boolean indicating whether or not to plot cases by age
#'   group, default is TRUE.
#'
#' @returns ggplot
#' @autoglobal
#' @importFrom ggplot2 ggplot geom_line aes facet_wrap scale_color_manual xlab
#'   ylab scale_x_date element_blank guides ggsave
#'
get_cases_plot <- function(weekly_data,
                           season_to_plot = NULL,
                           by_age_group = TRUE) {
  # summarise by reference_date
  if (isTRUE(by_age_group)) {
    weekly_cases <- weekly_data |>
      group_by(pathogen_name, age_group, end_of_week_reference_date, season) |>
      summarise(count = sum(count))
  } else {
    weekly_cases <- weekly_data |>
      group_by(pathogen_name, end_of_week_reference_date, season) |>
      summarise(count = sum(count)) |>
      mutate(age_group = "00+")
  }


  if (is.null(season_to_plot)) {
    weekly_cases_filtered <- weekly_cases
  } else {
    weekly_cases_filtered <- filter(
      weekly_cases,
      season %in% season_to_plot
    )
  }

  plot_comps <- plot_components()
  p <- ggplot(weekly_cases_filtered) +
    geom_line(aes(
      x = end_of_week_reference_date,
      y = count,
      color = age_group
    )) +
    facet_wrap(~pathogen_name,
      scales = "free_y",
      ncol = 4
    ) +
    scale_color_manual(
      name = "Age_group",
      values = plot_comps$age_colors
    ) +
    xlab("") +
    ylab("Total incident ED visits") +
    scale_x_date(
      breaks = "2 weeks",
      date_labels = "%d %b %Y"
    ) +
    get_plot_theme() +
    guides(color = "none") +
    theme(strip.text = element_blank())
  return(p)
}

#' Delay over time by age group and pathogen plot
#'
#' @inheritParams get_cases_plot
#'
#' @returns ggplot
#' @autoglobal
#' @importFrom dplyr ungroup
#' @importFrom ggplot2 ggplot geom_line aes facet_wrap scale_color_manual xlab
#'   ylab scale_x_date element_blank guides ggsave
get_delay_over_time_plot <- function(weekly_data,
                                     season_to_plot = NULL) {
  delay_df_t <- weekly_data |>
    group_by(end_of_week_reference_date, pathogen_name, age_group, season) |>
    summarise(mean_delay = sum(count * delay) / sum(count))
  if (is.null(season_to_plot)) {
    delay_df_t_filtered <- delay_df_t
  } else {
    delay_df_t_filtered <- filter(
      delay_df_t,
      season %in% season_to_plot
    )
  }

  plot_comps <- plot_components()
  p <- ggplot(delay_df_t_filtered) +
    geom_line(aes(
      x = end_of_week_reference_date,
      y = mean_delay,
      color = age_group
    )) +
    facet_wrap(~pathogen_name, scales = "free_y", ncol = 4) +
    scale_color_manual(
      name = "Age_group",
      values = plot_comps$age_colors
    ) +
    xlab("") +
    ylab("Mean delay (weeks)") +
    scale_x_date(
      breaks = "2 weeks",
      date_labels = "%d %b %Y"
    ) +
    get_plot_theme() +
    guides(color = "none") +
    theme(axis.text.x = element_blank())

  return(p)
}

#' Violin plot of delay by pathogen and age group
#'
#' @inheritParams get_cases_plot
#'
#' @returns ggplot
#' @autoglobal
#' @importFrom ggplot2 geom_violin geom_hline geom_vline theme_bw xlim
#'   scale_fill_manual guides geom_jitter geom_point guide_legend
#'   ggsave
get_violin_plot_delay <- function(weekly_data,
                                  season_to_plot = NULL) {
  delay_df_t <- weekly_data |>
    group_by(end_of_week_reference_date, pathogen_name, age_group, season) |>
    summarise(mean_delay = sum(count * delay) / sum(count))

  mean_delay_by_pathogen_ag <- weekly_data |>
    group_by(pathogen_name, age_group, season) |>
    summarise(mean_delay = sum(count * delay) / sum(count))

  if (is.null(season_to_plot)) {
    delay_df_t_filtered <- delay_df_t
    mean_delay_by_pathogen_ag <- mean_delay_by_pathogen_ag
  } else {
    delay_df_t_filtered <- filter(
      delay_df_t,
      season %in% season_to_plot
    )
    mean_delay_by_pathogen_ag <- filter(
      mean_delay_by_pathogen_ag,
      season %in% season_to_plot
    )
  }
  plot_comps <- plot_components()
  p <- ggplot(delay_df_t_filtered) +
    geom_violin(aes(x = age_group, y = mean_delay, fill = age_group),
      alpha = 0.5
    ) +
    facet_wrap(~pathogen_name, scales = "free_y", ncol = 4) +
    geom_jitter(
      aes(
        x = age_group,
        y = mean_delay,
        color = age_group
      ),
      width = 0.1, # Control horizontal spread
      alpha = 1, # Make points semi-transparent
      size = 0.8 # Point size
    ) +
    geom_point(
      data = mean_delay_by_pathogen_ag,
      aes(x = age_group, y = mean_delay),
      size = 3,
      shape = 17,
      color = "black"
    ) +
    xlab("") +
    ylab("Mean delay (weeks)") +
    get_plot_theme(dates = FALSE) +
    scale_color_manual(
      values = plot_comps$age_colors
    ) +
    scale_fill_manual(
      name = "Age group",
      values = plot_comps$age_colors
    ) +
    guides(
      fill = guide_legend(
        title.position = "left",
        title.hjust = 0.5,
        nrow = 1
      )
    ) +
    guides(color = "none") +
    theme(strip.text = element_blank())
  return(p)
}

#' Get plot of cdf
#'
#' @inheritParams get_cases_plot
#'
#' @returns ggplot
#' @autoglobal
get_delay_cdf_plot <- function(weekly_data) {
  avg_delays <- weekly_data |>
    group_by(pathogen_name, age_group, delay) |>
    summarise(delay_count = sum(count), .groups = "drop") |>
    group_by(pathogen_name, age_group) |>
    mutate(
      total_cases = sum(delay_count),
      pmf = delay_count / total_cases,
      cdf = cumsum(pmf)
    ) |>
    ungroup()

  plot_comps <- plot_components()

  p <- ggplot(avg_delays) +
    geom_line(aes(x = delay, y = cdf, color = age_group)) +
    facet_wrap(~pathogen_name) +
    geom_hline(aes(yintercept = 0.95), linetype = "dashed") +
    geom_vline(aes(xintercept = 8)) +
    theme_bw() +
    xlim(c(0, 10)) +
    scale_color_manual(
      name = "Age_group",
      values = plot_comps$age_colors
    ) +
    get_plot_theme(dates = FALSE) +
    xlab("Delay (weeks)") +
    ylab("CDF of delay")
  return(p)
}

#' Make delay figure
#'
#' @param delay_over_time plot delay over time
#' @param case_counts plot cases over time
#' @param violin_plot_delay plot mean delay distribution
#' @param season_to_plot Character string indicating the season
#' @param fig_file_name Character string indicating name of fig
#' @param fig_file_dir Character string indicating the filepath
#'
#' @returns patchwork fig
#' @importFrom patchwork plot_annotation plot_layout
#' @importFrom fs dir_create
#' @autoglobal
make_delay_fig <- function(delay_over_time,
                           case_counts,
                           violin_plot_delay,
                           season_to_plot,
                           fig_file_name = NULL,
                           fig_file_dir = file.path("output", "figs")) {
  fig_layout <- "
  AAAA
  AAAA
  BBBB
  CCCC
  CCCC
  "

  fig <- delay_over_time +
    case_counts +
    violin_plot_delay +
    plot_layout(
      design = fig_layout,
      axes = "collect",
      guides = "collect"
    ) +
    plot_annotation(
      title = glue::glue("Delay characterization: {season_to_plot}"),
      theme = theme(
        legend.position = "top",
        legend.title = element_text(hjust = 0.5),
        plot.title = element_text(size = 20),
        legend.justification = "right",
        plot.tag = element_text(size = 20)
      )
    )
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
    width = 24,
    height = 12
  )
  ggsave(
    plot = fig,
    filename = file.path(
      fig_file_dir,
      glue("{fig_file_name}.png")
    ),
    width = 24,
    height = 12,
    dpi = 600
  )

  return(fig)
}
