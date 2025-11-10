#' Get weekly data from raw data
#'
#' @param raw_data Data.frame of cases by reference and report date and
#'   pathogen
#'
#' @returns Case counts by epiweek
#' @autoglobal
#' @importFrom dplyr mutate rename filter group_by summarise
#' @importFrom tidyr expand_grid
#' @importFrom lubridate epiyear epiweek wday
get_weekly_data <- function(raw_data) {
  date_spine <- expand_grid(
    reference_date = seq(
      from = min(raw_data$reference_date),
      to = max(raw_data$reference_date), by = "day"
    ),
    report_date = seq(
      from = min(raw_data$report_date),
      to = max(raw_data$report_date), by = "day"
    ),
  ) |>
    mutate(
      epiweek_ref = epiweek(reference_date),
      epiyear_ref = epiyear(reference_date),
      epiweek_rep = epiweek(report_date),
      epiyear_rep = epiyear(report_date),
      wday_reference = wday(reference_date, label = TRUE),
      wday_report = wday(report_date, label = TRUE)
    ) |>
    filter(wday_reference == "Sat", wday_report == "Sat") |>
    rename(
      end_of_week_reference_date = reference_date,
      end_of_week_report_date = report_date
    )

  weekly_data <- raw_data |>
    mutate(
      epiweek_ref = epiweek(reference_date),
      epiyear_ref = epiyear(reference_date),
      epiweek_rep = epiweek(report_date),
      epiyear_rep = epiyear(report_date)
    ) |>
    group_by(
      epiyear_ref, epiweek_ref,
      epiyear_rep, epiweek_rep,
      pathogen, age_group
    ) |>
    summarise(
      count = sum(count),
      .groups = "drop"
    ) |>
    left_join(date_spine) |>
    mutate(
      delay = as.integer(difftime(end_of_week_report_date,
        end_of_week_reference_date,
        units = "weeks"
      )),
      delay_unit = "weeks"
    )
  return(weekly_data)
}

#' Clean data and add attributes like season and other indicators
#'
#' @param weekly_data Data.frame of cases by epiweek
#' @param max_delay Integer indicating max delay in weeks.
#' @param nowcast_date_range Range of dates to be nowcasted.
#' @param prev_season_date_range Range of dates for previous season.
#' @param delay_est_date_range Range of dates used for historical delay
#'   estimation
#'
#' @returns Cleaned dataframe filtered by max delay with season indicated.
#' @autoglobal
clean_data <- function(weekly_data,
                       max_delay,
                       nowcast_date_range,
                       prev_season_date_range,
                       delay_est_date_range) {
  # Filter to max_delay
  df_filtered <- weekly_data |>
    filter(delay <= max_delay) |>
    # indicate "seasons" and "time periods"
    mutate(
      season =
        case_when(
          end_of_week_reference_date < max(nowcast_date_range) &
            end_of_week_reference_date > min(nowcast_date_range) ~
            "2024-2025",
          end_of_week_reference_date < max(prev_season_date_range) &
            end_of_week_reference_date > min(prev_season_date_range) ~
            "2023-2024",
          TRUE ~ "other"
        ),
      time_period = ifelse(end_of_week_reference_date <
        max(delay_est_date_range) &
        end_of_week_reference_date >
          min(delay_est_date_range),
      "MA training period",
      "other"
      )
    ) |>
    filter(age_group != "Unknown")
  return(df_filtered)
}
