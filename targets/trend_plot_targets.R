trend_plot_targets <- list(
  # State-level trend plots ---------------------------------------------------

  ## Overall accuracy bar chart
  tar_target(
    name = plot_state_trend_accuracy,
    command = plot_trend_accuracy(
      accuracy_data = state_trend_accuracy,
      title = "State-level Trend Prediction Accuracy",
      fig_file_name = "state_trend_accuracy"
    )
  ),

  ## Accuracy by trend category
  tar_target(
    name = plot_state_trend_accuracy_by_category,
    command = plot_trend_accuracy_by_category(
      accuracy_by_category = state_trend_accuracy_by_category,
      title = "State-level Accuracy by Observed Trend",
      fig_file_name = "state_trend_accuracy_by_category"
    )
  ),

  ## Confusion matrices for each pathogen
  tar_target(
    name = plot_state_trend_confusion,
    command = plot_trend_confusion_matrix(
      confusion_matrix = state_trend_confusion_matrix,
      title = "State-level Trend Confusion Matrix",
      fig_file_name = "state_trend_confusion_matrix"
    )
  ),

  ## Accuracy over time
  tar_target(
    name = plot_state_trend_accuracy_over_time,
    command = plot_trend_accuracy_over_time(
      accuracy_over_time = state_trend_accuracy_over_time,
      title = "State-level Trend Accuracy Over Time",
      fig_file_name = "state_trend_accuracy_over_time"
    )
  ),

  # Age-group trend plots -----------------------------------------------------

  ## Overall accuracy bar chart
  tar_target(
    name = plot_ag_trend_accuracy,
    command = plot_trend_accuracy(
      accuracy_data = age_group_trend_accuracy |>
        # Summarize across age groups for cleaner visualization
        group_by(pathogen, pathogen_name, model) |>
        summarise(
          n_predictions = sum(n_predictions),
          n_correct = sum(n_correct),
          accuracy = n_correct / n_predictions * 100,
          .groups = "drop"
        ),
      title = "Age-group Trend Prediction Accuracy (Aggregated)",
      fig_file_name = "ag_trend_accuracy"
    )
  ),

  ## Accuracy by trend category (aggregated across age groups)
  tar_target(
    name = plot_ag_trend_accuracy_by_category,
    command = plot_trend_accuracy_by_category(
      accuracy_by_category = age_group_trend_accuracy_by_category |>
        group_by(pathogen, pathogen_name, model, trend_observed) |>
        summarise(
          n_predictions = sum(n_predictions),
          n_correct = sum(n_correct),
          accuracy = n_correct / n_predictions * 100,
          .groups = "drop"
        ),
      title = "Age-group Accuracy by Trend (Aggregated)",
      fig_file_name = "ag_trend_accuracy_by_category"
    )
  ),

  ## Confusion matrix
  tar_target(
    name = plot_ag_trend_confusion,
    command = plot_trend_confusion_matrix(
      confusion_matrix = age_group_trend_confusion_matrix |>
        # Aggregate across age groups for readability
        group_by(
          pathogen, pathogen_name, model,
          trend_predicted, trend_observed
        ) |>
        summarise(count = sum(count), .groups = "drop"),
      title = "Age-group Trend Confusion Matrix (Aggregated)",
      fig_file_name = "ag_trend_confusion_matrix"
    )
  ),

  ## Accuracy over time (aggregated)
  tar_target(
    name = plot_ag_trend_accuracy_over_time,
    command = plot_trend_accuracy_over_time(
      accuracy_over_time = age_group_trend_accuracy_over_time |>
        group_by(pathogen, pathogen_name, model, nowcast_date) |>
        summarise(
          n_predictions = sum(n_predictions),
          n_correct = sum(n_correct),
          accuracy = n_correct / n_predictions * 100,
          .groups = "drop"
        ),
      title = "Age-group Trend Accuracy Over Time (Aggregated)",
      fig_file_name = "ag_trend_accuracy_over_time"
    )
  ),

  # Individual pathogen plots for detailed analysis ---------------------------

  ## State-level by pathogen
  tar_target(
    name = plot_state_trend_bar,
    command = plot_trend_confusion_matrix(
      confusion_matrix = state_trend_confusion_matrix,
      pathogen_filter = "bar",
      title = "BAR Trend Confusion Matrix (State-level)",
      fig_file_name = "state_trend_confusion_bar"
    )
  ),
  tar_target(
    name = plot_state_trend_covid,
    command = plot_trend_confusion_matrix(
      confusion_matrix = state_trend_confusion_matrix,
      pathogen_filter = "covid",
      title = "COVID-19 Trend Confusion Matrix (State-level)",
      fig_file_name = "state_trend_confusion_covid"
    )
  ),
  tar_target(
    name = plot_state_trend_flu,
    command = plot_trend_confusion_matrix(
      confusion_matrix = state_trend_confusion_matrix,
      pathogen_filter = "flu",
      title = "Influenza Trend Confusion Matrix (State-level)",
      fig_file_name = "state_trend_confusion_flu"
    )
  ),
  tar_target(
    name = plot_state_trend_rsv,
    command = plot_trend_confusion_matrix(
      confusion_matrix = state_trend_confusion_matrix,
      pathogen_filter = "rsv",
      title = "RSV Trend Confusion Matrix (State-level)",
      fig_file_name = "state_trend_confusion_rsv"
    )
  )
)
