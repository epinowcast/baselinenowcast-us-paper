#' Fit the baselinenowcast method to the state level data (all age groups)
#'
#' @param all_data Data.frame of incident cases by reference date and report
#'   date by week for multiple age groups and pathogens
#' @param nowcast_date Date to produce the nowcast for.
#' @param pathogen_i Pathogen to nowcast.
#' @param eval_horizon Number of weeks to evaluation and save the nowcast.
#' @param max_delay Maximum delay in weeks.
#' @param quantiles_for_scoring Vector of quantiles to score.
#' @param scale_factor Scale factor on maximum delay of the amount of data to
#'   be used to train the baselinenowcast model.
#' @param prop_delay Proportion of all training volume to use for delay
#'   estimation
#' @param draws Number of draws to save
#'
#' @returns Quantiled dataframe of nowcasts with initial and final case counts
#'   alongside it.
fit_bnc_state <- function(all_data,
                          nowcast_date,
                          pathogen_i,
                          eval_horizon,
                          max_delay,
                          quantiles_for_scoring,
                          scale_factor = 3,
                          prop_delay = 0.5,
                          draws = 1000) {
  this_data <- all_data |>
    filter(
      end_of_week_report_date <= nowcast_date,
      pathogen == pathogen_i
    ) |>
    group_by(
      end_of_week_reference_date,
      end_of_week_report_date, delay
    ) |>
    summarise(count = sum(count, na.rm = TRUE)) |>
    filter(delay <= max_delay) |>
    ungroup()

  initial_data_summed <- this_data |>
    filter(end_of_week_reference_date >=
      max(end_of_week_reference_date) - weeks(eval_horizon)) |>
    group_by(end_of_week_reference_date) |>
    summarise(initial_count = sum(count))

  final_data_summed <- all_data |>
    filter(
      pathogen == pathogen_i,
      delay <= max_delay,
      end_of_week_reference_date <= nowcast_date
    ) |>
    group_by(end_of_week_reference_date) |>
    summarise(final_count = sum(count, na.rm = TRUE)) |>
    ungroup() |>
    filter(end_of_week_reference_date >=
      max(end_of_week_reference_date) - weeks(eval_horizon))
  pathogen_name <- all_data |>
    filter(pathogen == pathogen_i) |>
    distinct(pathogen_name) |>
    pull(pathogen_name)


  # convert to a reporting triangle
  rep_tri <- as_reporting_triangle(this_data,
    max_delay = max_delay,
    delays_unit = "weeks",
    reference_date = "end_of_week_reference_date",
    report_date = "end_of_week_report_date"
  )

  # generate a nowcast using the default settings
  nowcast_df <- baselinenowcast(rep_tri,
    scale_factor = scale_factor,
    prop_delay = prop_delay,
    draws = draws
  ) |>
    filter(reference_date >= max(reference_date) - weeks(eval_horizon)) |>
    trajectories_to_quantiles(
      quantiles = quantiles_for_scoring,
      timepoint_cols = "reference_date",
      value_col = "pred_count",
    ) |>
    mutate(
      pathogen = pathogen_i,
      pathogen_name = pathogen_name,
      nowcast_date = nowcast_date,
      age_group = "00+",
      scale_factor = scale_factor,
      prop_delay = prop_delay,
      model_type = "base"
    ) |>
    left_join(initial_data_summed,
      by = c("reference_date" = "end_of_week_reference_date")
    ) |>
    left_join(final_data_summed,
      by = c("reference_date" = "end_of_week_reference_date")
    )
  return(nowcast_df)
}
