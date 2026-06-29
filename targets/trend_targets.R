trend_targets <- list(
  # State-level trend assessment ----------------------------------------------

  ## Calculate trends for observed data
  tar_target(
    name = state_data_trends,
    command = clean_weekly_data |>
      filter(delay <= max_delay) |>
      group_by(pathogen, pathogen_name, end_of_week_reference_date) |>
      summarise(final_count = sum(count), .groups = "drop") |>
      rename(reference_date = end_of_week_reference_date) |>
      calculate_data_trends(
        group_vars = c("pathogen", "pathogen_name"),
        threshold = 5
      )
  ),

  ## Calculate trends for nowcast predictions (state-level)
  tar_target(
    name = state_nowcast_trends,
    command = state_nowcasts |>
      calculate_nowcast_trends(
        group_vars = c("pathogen", "pathogen_name", "model", "nowcast_date"),
        threshold = 5
      )
  ),

  ## Join predicted and observed trends (state-level)
  tar_target(
    name = state_trend_comparison,
    command = join_trends(
      nowcast_trends = state_nowcast_trends,
      data_trends = state_data_trends,
      group_vars = c("pathogen", "pathogen_name")
    )
  ),

  ## Calculate overall accuracy (state-level)
  tar_target(
    name = state_trend_accuracy,
    command = calculate_trend_accuracy(
      trend_comparison = state_trend_comparison,
      group_vars = c("pathogen", "pathogen_name", "model")
    )
  ),

  ## Calculate accuracy by trend category (state-level)
  tar_target(
    name = state_trend_accuracy_by_category,
    command = calculate_trend_accuracy_by_category(
      trend_comparison = state_trend_comparison,
      group_vars = c("pathogen", "pathogen_name", "model")
    )
  ),

  ## Create confusion matrix (state-level)
  tar_target(
    name = state_trend_confusion_matrix,
    command = create_trend_confusion_matrix(
      trend_comparison = state_trend_comparison,
      group_vars = c("pathogen", "pathogen_name", "model")
    )
  ),

  ## Calculate accuracy over time (state-level)
  tar_target(
    name = state_trend_accuracy_over_time,
    command = calculate_trend_accuracy(
      trend_comparison = state_trend_comparison,
      group_vars = c("pathogen", "pathogen_name", "model", "nowcast_date")
    )
  ),

  # Age-group trend assessment ------------------------------------------------

  ## Calculate trends for observed age-group data
  tar_target(
    name = age_group_data_trends,
    command = clean_weekly_data |>
      filter(
        delay <= max_delay,
        age_group != "00+"
      ) |>
      group_by(pathogen, pathogen_name, age_group, end_of_week_reference_date) |> # nolint
      summarise(final_count = sum(count), .groups = "drop") |>
      rename(reference_date = end_of_week_reference_date) |>
      calculate_data_trends(
        group_vars = c("pathogen", "pathogen_name", "age_group"),
        threshold = 5
      )
  ),

  ## Calculate trends for nowcast predictions (age-group)
  tar_target(
    name = age_group_nowcast_trends,
    command = age_group_nowcasts_ma_method_comp |>
      calculate_nowcast_trends(
        group_vars = c(
          "pathogen", "pathogen_name", "age_group",
          "model", "nowcast_date"
        ),
        threshold = 5
      )
  ),

  ## Join predicted and observed trends (age-group)
  tar_target(
    name = age_group_trend_comparison,
    command = join_trends(
      nowcast_trends = age_group_nowcast_trends,
      data_trends = age_group_data_trends,
      group_vars = c("pathogen", "pathogen_name", "age_group")
    )
  ),

  ## Calculate overall accuracy (age-group)
  tar_target(
    name = age_group_trend_accuracy,
    command = calculate_trend_accuracy(
      trend_comparison = age_group_trend_comparison,
      group_vars = c("pathogen", "pathogen_name", "age_group", "model")
    )
  ),

  ## Calculate accuracy by trend category (age-group)
  tar_target(
    name = age_group_trend_accuracy_by_category,
    command = calculate_trend_accuracy_by_category(
      trend_comparison = age_group_trend_comparison,
      group_vars = c("pathogen", "pathogen_name", "age_group", "model")
    )
  ),

  ## Create confusion matrix (age-group)
  tar_target(
    name = age_group_trend_confusion_matrix,
    command = create_trend_confusion_matrix(
      trend_comparison = age_group_trend_comparison,
      group_vars = c("pathogen", "pathogen_name", "age_group", "model")
    )
  ),

  ## Calculate accuracy over time (age-group)
  tar_target(
    name = age_group_trend_accuracy_over_time,
    command = calculate_trend_accuracy(
      trend_comparison = age_group_trend_comparison,
      group_vars = c(
        "pathogen", "pathogen_name", "age_group",
        "model", "nowcast_date"
      )
    )
  )
)
