#' Cases over time by age group and pathogen plot
#'
#' @param weekly_data Dataframe of cases by reference date, report date, week,
#'   and pathogen
#' @param season_to_plot Character string indicating what season to plot,
#'   default is NULL which will include all dates in the data
#'
#' @returns ggplot
#' @autoglobal
#' @importFrom ggplot2 ggplot geom_line aes facet_wrap
get_cases_plot <- function(weekly_data,
                           season_to_plot = NULL) {
  # summarise by reference_date
  weekly_cases <- weekly_data |>
    group_by(pathogen, age_group, end_of_week_reference_date, season) |>
    summarise(count = sum(count))

  if (is.null(season_to_plot)) {
    weekly_cases_filtered <- weekly_cases
  } else {
    weekly_cases_filtered <- filter(
      weekly_cases,
      season %in% season_to_plot
    )
  }

  p <- ggplot(weekly_cases_filtered) +
    geom_line(aes(
      x = end_of_week_reference_date,
      y = count,
      color = age_group
    )) +
    facet_wrap(~pathogen,
      scales = "free_y",
      nrow = 4
    )
  return(p)
}

#' Delay over time by age group and pathogen plot
#'
#' @inheritParams get_cases_plot
#'
#' @returns ggplot
#' @autoglobal
#' @importFrom dplyr ungroup
get_delay_over_time_plot <- function(weekly_data,
                                     season_to_plot = NULL) {
  delay_df_t <- weekly_data |>
    group_by(end_of_week_reference_date, pathogen, age_group, season) |>
    summarise(mean_delay = sum(count * delay) / sum(count))
  if (is.null(season_to_plot)) {
    delay_df_t_filtered <- delay_df_t
  } else {
    delay_df_t_filtered <- filter(
      delay_df_t,
      season %in% season_to_plot
    )
  }

  p <- ggplot(delay_df_t_filtered) +
    geom_line(aes(
      x = end_of_week_reference_date,
      y = mean_delay,
      color = age_group
    )) +
    facet_wrap(~pathogen, scales = "free_y", nrow = 4)
  return(p)
}

#' Violin plot of delay by pathogen and age group
#'
#' @inheritParams get_cases_plot
#'
#' @returns ggplot
#' @autoglobal
#' @importFrom ggplot2 geom_violin geom_hline geom_vline theme_bw xlim
get_violin_plot_delay <- function(weekly_data,
                                  season_to_plot = NULL) {
  delay_df_t <- weekly_data |>
    group_by(end_of_week_reference_date, pathogen, age_group, season) |>
    summarise(mean_delay = sum(count * delay) / sum(count))

  if (is.null(season_to_plot)) {
    delay_df_t_filtered <- delay_df_t
  } else {
    delay_df_t_filtered <- filter(
      delay_df_t,
      season %in% season_to_plot
    )
  }

  p <- ggplot(delay_df_t_filtered) +
    geom_violin(aes(x = age_group, y = mean_delay, fill = age_group)) +
    facet_wrap(~pathogen, scales = "free_y", nrow = 4)
}

get_delay_cdf_plot <- function(weekly_data) {
  avg_delays <- weekly_data |>
    group_by(pathogen, age_group, delay) |>
    summarise(delay_count = sum(count), .groups = "drop") |>
    group_by(pathogen, age_group) |>
    mutate(
      total_cases = sum(delay_count),
      pmf = delay_count / total_cases,
      cdf = cumsum(pmf)
    ) |>
    ungroup()
  p <- ggplot(avg_delays) +
    geom_line(aes(x = delay, y = cdf, color = age_group)) +
    facet_wrap(~pathogen) +
    geom_hline(aes(yintercept = 0.95), linetype = "dashed") +
    geom_vline(aes(xintercept = 8)) +
    theme_bw() +
    xlim(c(0, 10))
  return(p)
}
