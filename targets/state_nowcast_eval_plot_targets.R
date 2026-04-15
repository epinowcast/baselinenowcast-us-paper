state_nowcast_eval_plot_targets <- list(
  tar_target(
    name = plot_state_nowcasts_vs_data_bar,
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
    name = plot_state_nowcasts_vs_data_covid,
    command = get_plot_nowcasts_vs_data(
      nowcasts = state_nowcasts,
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
    name = plot_state_nowcasts_vs_data_flu,
    command = get_plot_nowcasts_vs_data(
      nowcasts = state_nowcasts,
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
    name = plot_state_nowcasts_vs_data_rsv,
    command = get_plot_nowcasts_vs_data(
      nowcasts = state_nowcasts,
      all_data = clean_weekly_data,
      max_delay = max_delay,
      pathogen_i = "rsv",
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
    command = get_bar_chart_scores(scores_su)
  ),
  tar_target(
    name = bar_chart_scores_bar,
    command = get_bar_chart_scores(scores_su |>
      filter(pathogen == "bar"))
  ),
  tar_target(
    name = bar_chart_scores_covid,
    command = get_bar_chart_scores(
      scores_su |>
        filter(pathogen == "covid"),
      remove_legend = TRUE
    )
  ),
  tar_target(
    name = bar_chart_scores_flu,
    command = get_bar_chart_scores(
      scores_su |>
        filter(pathogen == "flu"),
      remove_legend = TRUE
    )
  ),
  tar_target(
    name = bar_chart_scores_rsv,
    command = get_bar_chart_scores(
      scores_su |>
        filter(pathogen == "rsv"),
      remove_legend = TRUE
    )
  ),
  tar_target(
    name = fig_state_nowcast_comp,
    command = make_state_nowcast_comp_fig(
      plot_state_nowcasts_vs_data_bar,
      bar_chart_scores_bar,
      bar_chart_scores_covid,
      bar_chart_scores_flu,
      bar_chart_scores_rsv,
      fig_file_name = "fig3_state_nowcast_comp"
    )
  ),
  #  Supplementary figures -----------------------------------------------
  # WIS scores over time by model and pathogen + data
  tar_target(
    name = state_wis_over_time,
    command = get_state_wis_over_time_plot(
      all_data = clean_weekly_data,
      scores = scores_su,
      nowcasts = state_nowcasts,
      max_delay = max_delay,
      fig_file_name = "state_wis_t"
    )
  ),

  # Coverage for each of the models and pathogens
  tar_target(
    name = bar_chart_coverage,
    command = get_bar_chart_coverage(
      coverage = coverage_state,
      title = "95% interval coverage all age groups combined",
      fig_file_name = "state_coverage"
    )
  )
)
