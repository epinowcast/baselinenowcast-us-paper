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
#' @importFrom dplyr anti_join reframe
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
    summarize(
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
