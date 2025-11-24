state_nowcast_eval_plot_targets <- list(
  tar_target(
    name = plot_nowcasts_vs_data,
    command = get_plot_nowcasts_vs_data(
      nowcasts = state_nowcasts,
      all_data = clean_weekly_data,
      max_delay = max_delay,
      pathogen_i = "bar",
      nowcast_dates_to_plot = c(
        "2024-08-03",
        "2024-10-12",
        "2024-12-28",
        "2025-03-01",
        "2025-05-17"
      )
    )
  ),
  tar_target(
    name = bar_chart_scores,
    command = get_overall_scores(scores_su)
  )
)
