#' Get weekly data from raw data
#'
#' @param raw_data Data.frame of cases by reference and report date and
#'   pathogen
#'
#' @returns Case counts by epiweek
#' @autoglobal
#' @importFrom dplyr mutate rename filter group_by summarise left_join
#'   case_when
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
    )
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
      delay_unit = "weeks",
      pathogen_name = case_when(
        pathogen == "bar" ~ "Broad Acute Respiratory",
        pathogen == "flu" ~ "Influenza",
        pathogen == "covid" ~ "COVID-19",
        pathogen == "rsv" ~ "RSV"
      )
    )
  return(weekly_data)
}

#' Get properly formatted daily data with delays
#'
#' @param raw_data Daily data from file.
#'
#' @returns formatted daily data
get_daily_data <- function(raw_data) {
  data_df <- raw_data |>
    mutate(
      delay = as.integer(difftime(report_date,
        reference_date,
        units = "days"
      )),
      delay_unit = "days",
      pathogen_name = case_when(
        pathogen == "bar" ~ "Broad Acute Respiratory",
        pathogen == "flu" ~ "Influenza",
        pathogen == "covid" ~ "COVID-19",
        pathogen == "rsv" ~ "RSV"
      )
    ) |>
    filter(age_group != "Unknown")
  return(data_df)
}

#' Get the respiratory season a date falls in
#'
#' Seasons run from epiweek 27 of one year to epiweek 26 of the next.
#'
#' @param date Vector of dates
#'
#' @returns Character vector of seasons, e.g. "2024-2025"
#' @importFrom lubridate epiweek epiyear
get_season <- function(date) {
  start_year <- ifelse(epiweek(date) >= 27, epiyear(date), epiyear(date) - 1)
  return(paste(start_year, start_year + 1, sep = "-"))
}

#' Clean data and add attributes like season and other indicators
#'
#' @param weekly_data Data.frame of cases by epiweek
#' @param max_delay Integer indicating max delay in weeks.
#' @param seasons Character vector of seasons to label, e.g. "2024-2025".
#'   Weeks outside these seasons are labelled "other".
#' @param delay_est_date_range Range of dates used for historical delay
#'   estimation
#'
#' @returns Cleaned dataframe filtered by max delay with season indicated.
#' @autoglobal
clean_data <- function(weekly_data,
                       max_delay,
                       seasons,
                       delay_est_date_range) {
  # Filter to max_delay
  df_filtered <- weekly_data |>
    filter(delay <= max_delay) |>
    # indicate "seasons" and "time periods"
    mutate(
      season = get_season(end_of_week_reference_date),
      season = ifelse(season %in% seasons, season, "other"),
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
