#' Calculate two-week percent change
#'
#' @param current_value Numeric value at current time point
#' @param previous_value Numeric value at previous time point (2 weeks prior)
#' @return Numeric percent change
#' @autoglobal
calculate_percent_change <- function(current_value, previous_value) {
  if (is.na(previous_value) || previous_value == 0) {
    return(NA_real_)
  }
  ((current_value - previous_value) / previous_value) * 100
}

#' Classify trend based on percent change threshold
#'
#' @param percent_change Numeric percent change value
#' @param threshold Numeric threshold for classifying stable vs changing,
#'   default is 5 (for ±5%)
#' @return Character trend category: "increasing", "stable", or "decreasing"
#' @autoglobal
classify_trend <- function(percent_change, threshold = 5) {
  if (is.na(percent_change)) {
    return(NA_character_)
  }
  if (percent_change > threshold) {
    return("increasing")
  } else if (percent_change < -threshold) {
    return("decreasing")
  } else {
    return("stable")
  }
}

#' Calculate trends for observed data
#'
#' @param data Data frame containing observed counts with columns:
#'   reference_date, final_count, and grouping variables
#' @param group_vars Character vector of grouping variables (e.g.,
#'   c("pathogen", "location"))
#' @param threshold Numeric threshold for trend classification, default is 5
#' @importFrom dplyr arrange group_by mutate lag ungroup
#' @importFrom lubridate weeks
#' @return Data frame with added columns: prev_count, pct_change, trend_observed
#' @autoglobal
calculate_data_trends <- function(data,
                                  group_vars = c("pathogen"),
                                  threshold = 5) {
  data |>
    arrange(reference_date) |>
    group_by(across(all_of(group_vars))) |>
    mutate(
      prev_count = lag(final_count, n = 2, order_by = reference_date),
      pct_change = calculate_percent_change(final_count, prev_count),
      trend_observed = classify_trend(pct_change, threshold)
    ) |>
    ungroup()
}

#' Calculate trends for nowcast predictions
#'
#' @param nowcasts Data frame containing nowcast quantiles with columns:
#'   reference_date, nowcast_date, quantile_level, quantile_value, model,
#'   and grouping variables
#' @param group_vars Character vector of grouping variables (e.g.,
#'   c("pathogen", "model", "nowcast_date"))
#' @param threshold Numeric threshold for trend classification, default is 5
#' @importFrom dplyr filter arrange group_by mutate lag ungroup
#' @importFrom lubridate weeks
#' @return Data frame with median nowcasts and trend classifications
#' @autoglobal
calculate_nowcast_trends <- function(nowcasts,
                                     group_vars = c("pathogen", "model", "nowcast_date"), # nolint
                                     threshold = 5) {
  # Extract median predictions
  median_nowcasts <- nowcasts |>
    filter(quantile_level == 0.5) |>
    arrange(reference_date)

  # Calculate trends
  median_nowcasts |>
    group_by(across(all_of(group_vars))) |>
    mutate(
      prev_quantile_value = lag(quantile_value, n = 2, order_by = reference_date), # nolint
      pct_change_predicted = calculate_percent_change(
        quantile_value,
        prev_quantile_value
      ),
      trend_predicted = classify_trend(pct_change_predicted, threshold)
    ) |>
    ungroup()
}

#' Compare predicted trends to observed trends
#'
#' @param nowcast_trends Data frame with nowcast trends (from
#'   calculate_nowcast_trends)
#' @param data_trends Data frame with observed data trends (from
#'   calculate_data_trends)
#' @param group_vars Character vector of additional grouping variables beyond
#'   reference_date
#' @importFrom dplyr inner_join select mutate
#' @return Data frame with both predicted and observed trends for comparison
#' @autoglobal
join_trends <- function(nowcast_trends,
                        data_trends,
                        group_vars = c("pathogen")) {
  # Select relevant columns from data trends
  data_cols <- c(
    "reference_date", group_vars, "trend_observed",
    "final_count", "pct_change"
  )
  data_for_join <- data_trends |>
    select(all_of(data_cols))

  # Join with nowcast trends
  nowcast_trends |>
    inner_join(
      data_for_join,
      by = c("reference_date", group_vars)
    ) |>
    mutate(
      trend_correct = !is.na(trend_predicted) &
        !is.na(trend_observed) &
        trend_predicted == trend_observed
    )
}

#' Calculate trend accuracy metrics by model and pathogen
#'
#' @param trend_comparison Data frame from join_trends() containing both
#'   predicted and observed trends
#' @param group_vars Character vector of grouping variables for summarization
#' @importFrom dplyr group_by summarise n filter
#' @return Data frame with accuracy metrics by group
#' @autoglobal
calculate_trend_accuracy <- function(trend_comparison,
                                     group_vars = c("pathogen", "model")) {
  trend_comparison |>
    filter(!is.na(trend_predicted) & !is.na(trend_observed)) |>
    group_by(across(all_of(group_vars))) |>
    summarise(
      n_predictions = n(),
      n_correct = sum(trend_correct, na.rm = TRUE),
      accuracy = n_correct / n_predictions * 100,
      .groups = "drop"
    )
}

#' Calculate trend accuracy stratified by observed trend category
#'
#' @param trend_comparison Data frame from join_trends() containing both
#'   predicted and observed trends
#' @param group_vars Character vector of grouping variables for summarization
#' @importFrom dplyr group_by summarise n filter
#' @return Data frame with accuracy metrics by group and trend category
#' @autoglobal
calculate_trend_accuracy_by_category <- function(
  trend_comparison,
  group_vars = c("pathogen", "model")
) {
  trend_comparison |>
    filter(!is.na(trend_predicted) & !is.na(trend_observed)) |>
    group_by(across(all_of(c(group_vars, "trend_observed")))) |>
    summarise(
      n_predictions = n(),
      n_correct = sum(trend_correct, na.rm = TRUE),
      accuracy = n_correct / n_predictions * 100,
      .groups = "drop"
    )
}

#' Create confusion matrix for trend predictions
#'
#' @param trend_comparison Data frame from join_trends() containing both
#'   predicted and observed trends
#' @param group_vars Character vector of grouping variables
#' @importFrom dplyr group_by summarise n filter
#' @importFrom tidyr complete
#' @return Data frame with counts for each predicted vs observed combination
#' @autoglobal
create_trend_confusion_matrix <- function(
  trend_comparison,
  group_vars = c("pathogen", "model")
) {
  trend_comparison |>
    filter(!is.na(trend_predicted) & !is.na(trend_observed)) |>
    group_by(across(all_of(c(
      group_vars, "trend_predicted", "trend_observed"
    )))) |>
    summarise(
      count = n(),
      .groups = "drop"
    ) |>
    complete(
      !!!syms(group_vars),
      trend_predicted = c("increasing", "stable", "decreasing"),
      trend_observed = c("increasing", "stable", "decreasing"),
      fill = list(count = 0)
    )
}
