ag_nowcast_eval_plot_targets <- list(
  tar_target(
    name = plot_age_group_nowcasts_vs_data_bar,
    command = get_plot_ag_nowcasts_vs_data(
      nowcasts = age_group_nowcasts,
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
    name = plot_age_group_nowcasts_vs_data_covid,
    command = get_plot_ag_nowcasts_vs_data(
      nowcasts = age_group_nowcasts,
      all_data = clean_weekly_data,
      max_delay = max_delay,
      pathogen_i = "covid",
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
    name = plot_age_group_nowcasts_vs_data_flu,
    command = get_plot_ag_nowcasts_vs_data(
      nowcasts = age_group_nowcasts,
      all_data = clean_weekly_data,
      max_delay = max_delay,
      pathogen_i = "flu",
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
    name = plot_age_group_nowcasts_vs_data_rsv,
    command = get_plot_ag_nowcasts_vs_data(
      nowcasts = age_group_nowcasts,
      all_data = clean_weekly_data,
      max_delay = max_delay,
      pathogen_i = "rsv",
      nowcast_dates_to_plot = c(
        "2024-11-02",
        "2024-12-07",
        "2024-12-28",
        "2025-01-18"
      )
    )
  ),
  tar_target(
    name = bar_chart_model_comp_across_ag,
    command = get_bar_chart_scores(scores_ag_su)
  )
)
