trend_targets <- list(
  # State-level trend assessment ---------------------------------------------

  ## Calculate trends for nowcast predictions and observations (state-level)
  tar_target(
    name = state_nowcast_trends,
    command = state_nowcasts |>
      group_by(pathogen, pathogen_name, nowcast_date, model) |>
      filter(quantile_level == 0.5) |>
      mutate(
        prev_value = lag(quantile_value, n = 2, order_by = reference_date),
        prev_count = lag(final_count, n = 2, order_by = reference_date)
      ) |>
      filter(
        !is.na(prev_value), !is.na(prev_count),
        reference_date == max(reference_date)
      ) |>
      group_by(nowcast_date, pathogen, model) |>
      mutate(
        pct_change_nowcast = (quantile_value - prev_value) / prev_value * 100,
        pct_change_obs = (final_count - prev_count) / prev_count * 100,
        trend_nowcast = classify_trend(pct_change_nowcast, trend_threshold),
        trend_obs = classify_trend(pct_change_obs, trend_threshold),
        trend_correct = !is.na(trend_nowcast) &
          !is.na(trend_obs) &
          trend_nowcast == trend_obs
      )
  ),

  ## Calculate overall accuracy (state-level)
  tar_target(
    name = state_trend_accuracy,
    command = calculate_trend_accuracy(
      trend_comparison = state_nowcast_trends,
      group_vars = c("pathogen", "pathogen_name", "model")
    )
  ),
  tar_target(
    name = state_trend_accuracy_by_trend,
    command = calculate_trend_accuracy(
      trend_comparison = state_nowcast_trends,
      group_vars = c("pathogen", "pathogen_name", "model", "trend_obs")
    )
  ),

  ## Create confusion matrix (state-level)
  tar_target(
    name = state_trend_confusion_matrix,
    command = create_trend_confusion_matrix(
      trend_comparison = state_nowcast_trends,
      group_vars = c("pathogen", "pathogen_name", "model")
    )
  ),

  ## Calculate accuracy over time (state-level)
  tar_target(
    name = state_trend_accuracy_over_time,
    command = calculate_trend_accuracy(
      trend_comparison = state_nowcast_trends,
      group_vars = c("pathogen", "pathogen_name", "model", "nowcast_date")
    )
  ),

  # Age-group trend assessment ------------------------------------------------

  ## Calculate trends for observed age-group data

  ## Calculate trends for nowcast predictions and data by  (age-group)
  tar_target(
    name = age_group_nowcast_trends,
    command = age_group_nowcasts |>
      group_by(pathogen, pathogen_name, nowcast_date, model, age_group) |>
      filter(quantile_level == 0.5) |>
      mutate(
        prev_value = lag(quantile_value, n = 2, order_by = reference_date),
        prev_count = lag(final_count, n = 2, order_by = reference_date)
      ) |>
      filter(
        !is.na(prev_value), !is.na(prev_count),
        reference_date == max(reference_date)
      ) |>
      group_by(nowcast_date, pathogen, model, age_group) |>
      mutate(
        pct_change_nowcast = (quantile_value - prev_value) / prev_value * 100,
        pct_change_obs = (final_count - prev_count) / prev_count * 100,
        trend_nowcast = classify_trend(pct_change_nowcast, trend_threshold),
        trend_obs = classify_trend(pct_change_obs, trend_threshold),
        trend_correct = !is.na(trend_nowcast) &
          !is.na(trend_obs) &
          trend_nowcast == trend_obs
      )
  ),


  ## Calculate overall accuracy (age-group)
  tar_target(
    name = age_group_trend_accuracy,
    command = calculate_trend_accuracy(
      trend_comparison = age_group_nowcast_trends,
      group_vars = c("pathogen", "pathogen_name", "age_group", "model")
    )
  ),

  ## Calculate accuracy by trend category (age-group)
  tar_target(
    name = age_group_trend_accuracy_by_trend,
    command = calculate_trend_accuracy(
      trend_comparison = age_group_nowcast_trends,
      group_vars = c(
        "pathogen", "pathogen_name", "age_group", "model",
        "trend_obs"
      )
    )
  ),

  ## Create confusion matrix (age-group)
  tar_target(
    name = age_group_trend_confusion_matrix,
    command = create_trend_confusion_matrix(
      trend_comparison = age_group_nowcast_trends,
      group_vars = c("pathogen", "pathogen_name", "age_group", "model")
    )
  ),

  ## Calculate accuracy over time (age-group)
  tar_target(
    name = age_group_trend_accuracy_over_time,
    command = calculate_trend_accuracy(
      trend_comparison = age_group_nowcast_trends,
      group_vars = c(
        "pathogen", "pathogen_name", "age_group",
        "model", "nowcast_date"
      )
    )
  )
)
