#' Suppress output and messages for code.
#' @param code Code to run quietly.
#' @return The result of running the code.
#' @export
#' @examples
#' result <- quiet(message("This message should be suppressed"))
#' print(result)
quiet <- function(code) {
  sink(nullfile())
  on.exit(sink())
  return(suppressMessages(code))
}

#' Aggregate individual trajectory
#' timeseries or forecasts to quantile
#' timeseries or forecasts
#'
#' Given a tidy data frame of
#' trajectories, aggregate it to
#' a quantile timeseries for the
#' given quantile values.
#'
#' From https://github.com/CDCgov/forecasttools/blob/main/R/trajectories_to_quantiles.R #nolint
#'
#' @param trajectories Tidy data frame or tibble
#' of trajectories
#' @param quantiles Quantiles to output for each
#' timepoint (default the FluSight/COVIDHub 2024-25 quantiles:
#' `c(0.01, 0.025, 1:19/20, 0.975, 0.99)`
#' @param timepoint_cols Name(s) of the column(s) in `trajectories`
#' that identifies unique timepoints. Default `"timepoint"`.
#' @param value_col name of the column in `trajectories`
#' with the trajectory values (for which we wish to
#' compute quantiles), e.g. `hosp`, `weekly_hosp`, `cases`,
#' etc. Default `value`.
#' @param quantile_value_name What to name
#' the column containing quantile values in
#' the output table. Default `"quantile_value"`
#' @param quantile_level_name What to name
#' the column containing quantile levels in
#' the output table. Default `"quantile_level"`
#' @param id_cols additional id columns in
#' `trajectories` to group by before aggregating,
#' e.g. a `location` column if `trajectories` contains
#' trajectories over the same time period for multiple
#' locations, such as different US States and Territories.
#' If NULL, ignored. Default NULL.
#' @export
#' @autoglobal
#' @importFrom dplyr anti_join reframe summarise
trajectories_to_quantiles <- function(
  trajectories,
  quantiles = c(
    0.01,
    0.025,
    1:19 / 20,
    0.975,
    0.99
  ),
  timepoint_cols = "timepoint",
  value_col = "value",
  quantile_value_name = "quantile_value",
  quantile_level_name = "quantile_level",
  id_cols = NULL
) {
  grouped_df <- trajectories |>
    rename(value_col = !!value_col) |>
    group_by(
      across(tidyselect::all_of(c(timepoint_cols, id_cols)))
    )

  missing_groups <- grouped_df |>
    summarise(
      "any_missing" = anyNA(.data$value_col), # nolint
      .groups = "drop"
    ) |>
    filter(.data$any_missing) |>
    select(-"any_missing")

  quant_df <- grouped_df |>
    anti_join(missing_groups, by = colnames(missing_groups)) |>
    reframe(
      !!quantile_value_name := stats::quantile(
        .data$value_col,
        probs = !!quantiles
      ),
      !!quantile_level_name := !!quantiles
    )
  return(quant_df)
}

#' Filter to in-season epiweeks
#'
#' Restricts a data.frame (e.g. scores, nowcasts or data) to dates falling
#'   within an epiweek window that wraps the year boundary (e.g. epiweeks 40
#'   through 19), for the specified pathogens only. Rows for all other
#'   pathogens are left unchanged.
#'
#' @param df Data.frame containing a date column and a `pathogen` column.
#' @param start_epiweek Integer indicating the first epiweek of the season.
#'   Default is 40.
#' @param end_epiweek Integer indicating the last epiweek of the season.
#'   Default is 19.
#' @param pathogens_to_filter Character vector of pathogens to restrict.
#'   Default is `c("flu", "rsv")`.
#' @param date_col Character string indicating the name of the date column
#'   to filter on. Default is `"reference_date"`.
#'
#' @returns Data.frame filtered to in-season dates.
#' @autoglobal
#' @importFrom lubridate epiweek
filter_to_season_epiweeks <- function(df,
                                      start_epiweek = 40,
                                      end_epiweek = 19,
                                      pathogens_to_filter = c("flu", "rsv"),
                                      date_col = "reference_date") {
  epiweek_ref <- epiweek(df[[date_col]])
  in_season <- epiweek_ref >= start_epiweek | epiweek_ref <= end_epiweek
  keep <- in_season | !df$pathogen %in% pathogens_to_filter
  return(df[keep, ])
}

#' Filter to the forecasts available for every model
#'
#' Restricts a data.frame of nowcasts or scores to the forecasts (defined by
#'   `forecast_unit`) that are present for every model in it, so that models
#'   are compared on the same set of forecasts.
#'
#' @param df Data.frame of nowcasts or scores with a `model` column.
#' @param forecast_unit Character vector of the columns, other than `model`,
#'   that define a single forecast.
#'
#' @returns Data.frame filtered to the forecasts common to all models.
#' @autoglobal
#' @importFrom dplyr distinct across all_of count filter select semi_join
#'   n_distinct
filter_to_common_forecasts <- function(df, forecast_unit) {
  n_models <- n_distinct(df$model)
  common <- df |>
    distinct(model, across(all_of(forecast_unit))) |>
    count(across(all_of(forecast_unit))) |>
    filter(n == n_models) |>
    select(-n)
  return(semi_join(df, common, by = forecast_unit))
}

#' Filter nowcasts to the evaluation horizons
#'
#' Adds a `horizon` column (weeks between the reference date and the nowcast
#'   date) and keeps only the horizons that are evaluated, so that scores and
#'   coverage are computed on the same nowcasts.
#'
#' @param nowcasts Data.frame of nowcasts with `nowcast_date` and
#'   `reference_date` columns.
#' @param eval_horizon Integer indicating the number of weeks before the
#'   nowcast date to evaluate.
#'
#' @returns `nowcasts` with a `horizon` column, filtered to
#'   `horizon <= eval_horizon`.
#' @autoglobal
#' @importFrom dplyr mutate filter
filter_to_eval_horizons <- function(nowcasts, eval_horizon) {
  nowcasts <- nowcasts |>
    mutate(
      horizon = as.integer(floor((nowcast_date - reference_date) / 7))
    ) |>
    filter(horizon <= eval_horizon)
  return(nowcasts)
}
