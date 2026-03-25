#' Get MA delay data from local multipliers files
#'
#' @param fp_prefix Filepath prefix
#' @param pathogen Character string indicating pathogen
#' @param max_delay Integer indicating the maximum delay
#'
#' @returns Data.frame with median and 95% PI of PDF and CDF
#' @autoglobal
#' @importFrom dplyr filter mutate select
#' @importFrom readr read_csv
#' @importFrom glue glue
get_ma_delay_data <- function(fp_prefix,
                              pathogen,
                              max_delay) {
  fp <- glue::glue("{fp_prefix}_{pathogen}.csv")
  df_raw <- read_csv(fp)


  delay_df <- df_raw |>
    filter(WeeksAgo != 0) |> # Remove week 0 because this is the partial week
    mutate(
      median_pdf = diff(c(0, median)),
      lb_pdf = diff(c(0, `2.5%`)),
      ub_pdf = diff(c(0, `97.5%`)),
      median_cdf = median,
      lb_cdf = `2.5%`,
      ub_cdf = `97.5%`,
      delay = WeeksAgo - 1 # reindex their week 1 to week 0
    ) |>
    filter(delay <= max_delay) |>
    mutate(
      pathogen = pathogen,
      model = "ma"
    ) |>
    select(
      pathogen, delay, median_pdf, lb_pdf, ub_pdf,
      median_cdf,
      lb_cdf,
      ub_cdf
    )

  return(delay_df)
}

#' Get delay distribution from data using baselinenowcast
#'
#' @param data Dataframe of weekly data by age group and pathogen
#' @param nowcast_date Date of the nowcast
#' @param pathogen Character string indicating the pathogen
#' @param max_delay Integer indicating the maximum delay
#'
#' @returns Data.frame of delay distribution pdf and cdf for each nowcast date
#'   and pathogen
#' @autoglobal
#' @importFrom dplyr filter group_by pull
#' @importFrom baselinenowcast as_reporting_triangle
get_delay_df <- function(data,
                         nowcast_date,
                         pathogen,
                         max_delay) {
  this_data <- data |>
    filter(
      end_of_week_report_date <= !!nowcast_date,
      pathogen == !!pathogen
    ) |>
    group_by(
      end_of_week_reference_date,
      end_of_week_report_date, delay
    ) |>
    summarise(count = sum(count, na.rm = TRUE)) |>
    filter(delay <= max_delay) |>
    ungroup()

  this_season <- data |>
    filter(
      end_of_week_report_date == !!nowcast_date,
      end_of_week_reference_date == !!nowcast_date,
      delay <= max_delay
    ) |>
    pull(season) |>
    unique()

  rep_tri <- as_reporting_triangle(this_data,
    max_delay = max_delay,
    delays_unit = "weeks",
    reference_date = "end_of_week_reference_date",
    report_date = "end_of_week_report_date"
  )

  delay <- estimate_delay(rep_tri)

  delay_df <- data.frame(
    pathogen = pathogen,
    nowcast_date = nowcast_date,
    delay = 0:max_delay,
    delay_value = delay,
    model = "baselinenowcast",
    delay_cdf = cumsum(delay),
    season = this_season
  )
  return(delay_df)
}

#' Get a bar chart comparing the mean delays
#'
#' @param ma_delay Dataframe of delay distribution from MA data
#' @param delay_dfs_bnc Dataframe of delay distributions from data
#' @param plot_type Character string indicating whether to plot the pdf or
#'   the cdf
#'
#' @returns ggplot showing each delay distribution for both data and MA delay,
#'   faceted by pathogen
#' @autoglobal
#' @importFrom ggplot2 ggplot geom_line aes facet_wrap theme_bw xlab ylab
get_plot_delay_comparison <- function(ma_delay,
                                      delay_dfs_bnc,
                                      plot_type = "cdf") {
  if (plot_type == "cdf") {
    p <- ggplot() +
      geom_line(
        data = ma_delay,
        aes(x = delay, y = median_cdf), color = "black"
      ) +
      geom_line(
        data = delay_dfs_bnc,
        aes(
          x = delay, y = delay_cdf, group = nowcast_date,
          color = season
        ),
        alpha = 0.2,
        linewidth = 0.2
      ) +
      facet_wrap(~pathogen) +
      theme_bw() +
      xlab("Delay (weeks)") +
      ylab("Cumulative Distribution Function of Delay Distribution")
  } else {
    p <- ggplot() +
      geom_line(
        data = ma_delay,
        aes(x = delay, y = median_pdf), color = "black"
      ) +
      geom_line(
        data = delay_dfs_bnc,
        aes(
          x = delay, y = delay_value, group = nowcast_date,
          color = season
        ),
        alpha = 0.2,
        linewidth = 0.2
      ) +
      facet_wrap(~pathogen) +
      theme_bw() +
      xlab("Delay (weeks)") +
      ylab("Probability Distribution Function of Delay Distribution")
  }

  return(p)
}


#' Get a bar chart of the mean delay for each method and by season
#'
#' @param ma_delay Dataframe of MA delay distribution
#' @param delay_dfs_bnc Dataframe of delay distribution from latest data
#'
#' @returns ggplot of the mean delay by method and season
#' @autoglobal
#' @importFrom dplyr group_by summarise mutate
#' @importFrom ggplot geom_bar aes facet_wrap xlab ylab
get_bar_chart_mean_delay_comparison <- function(ma_delay,
                                                delay_dfs_bnc) {
  # Find average
  delay_df_avg <- delay_dfs_bnc |>
    group_by(pathogen, nowcast_date, season) |>
    summarise(mean_delay = sum(delay * delay_value)) |>
    group_by(pathogen, season) |>
    summarise(mean_delay = mean(mean_delay)) |>
    mutate(method = "baselinenowcast")

  ma_delay_avg <- ma_delay |>
    group_by(pathogen) |>
    summarise(mean_delay = sum(delay * median_pdf)) |>
    mutate(
      method = "ma",
      season = "2023"
    )

  delay_avg <- bind_rows(
    delay_df_avg,
    ma_delay_avg
  )

  p <- ggplot() +
    geom_bar(
      data = delay_avg,
      aes(x = method, y = mean_delay, fill = season), stat = "identity",
      position = "dodge"
    ) +
    facet_wrap(~pathogen) +
    theme_bw() +
    xlab("") +
    ylab("Average delay (weeks)")

  return(p)
}
