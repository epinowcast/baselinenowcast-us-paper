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
  accuracy_df <- trend_comparison |>
    filter(!is.na(trend_nowcast), !is.na(trend_obs)) |>
    group_by(across(all_of(group_vars))) |>
    summarise(
      n_predictions = n(),
      n_correct = sum(trend_correct, na.rm = TRUE),
      accuracy = n_correct / n_predictions * 100,
      .groups = "drop"
    )
  return(accuracy_df)
}


#' Create confusion matrix for trend predictions
#'
#' @param trend_comparison Data frame from join_trends() containing both
#'   predicted and observed trends
#' @param group_vars Character vector of grouping variables
#' @importFrom dplyr group_by summarise n filter syms all_of across
#' @importFrom tidyr complete
#' @return Data frame with counts for each predicted vs observed combination
#' @autoglobal
create_trend_confusion_matrix <- function(
  trend_comparison,
  group_vars = c("pathogen", "model")
) {
  mat <- trend_comparison |>
    filter(
      !is.na(trend_nowcast),
      !is.na(trend_obs)
    ) |>
    group_by(across(all_of(c(
      group_vars, "trend_nowcast", "trend_obs"
    )))) |>
    summarise(
      count = n(),
      .groups = "drop"
    ) |>
    complete(
      !!!syms(group_vars),
      trend_nowcast = c("increasing", "stable", "decreasing"),
      trend_obs = c("increasing", "stable", "decreasing"),
      fill = list(count = 0)
    )
  return(mat)
}
