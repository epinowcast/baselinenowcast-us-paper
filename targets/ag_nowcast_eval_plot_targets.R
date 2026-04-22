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
  ),
  # Age group fig components -----------------------------------
  tar_target(
    name = plot_00_04_nowcasts_vs_data_rsv,
    command = get_plot_ag_nowcasts_vs_data(
      nowcasts = age_group_nowcasts |> filter(age_group == "00-04"),
      all_data = clean_weekly_data |> filter(age_group == "00-04"),
      max_delay = max_delay,
      pathogen_i = "rsv",
      nowcast_dates_to_plot = c(
        "2024-11-09",
        "2024-11-30",
        "2024-12-21",
        "2025-01-11",
        "2025-02-01"
      )
    )
  ),
  tar_target(
    name = plot_18_44_nowcasts_vs_data_rsv,
    command = get_plot_ag_nowcasts_vs_data(
      nowcasts = age_group_nowcasts |> filter(age_group == "18-44"),
      all_data = clean_weekly_data |> filter(age_group == "18-44"),
      max_delay = max_delay,
      pathogen_i = "rsv",
      nowcast_dates_to_plot = c(
        "2024-11-09",
        "2024-11-30",
        "2024-12-21",
        "2025-01-11",
        "2025-02-01"
      )
    )
  ),
  tar_target(
    name = plot_65_plus_nowcasts_vs_data_rsv,
    command = get_plot_ag_nowcasts_vs_data(
      nowcasts = age_group_nowcasts |> filter(age_group == "65+"),
      all_data = clean_weekly_data |> filter(age_group == "65+"),
      max_delay = max_delay,
      pathogen_i = "rsv",
      nowcast_dates_to_plot = c(
        "2024-11-09",
        "2024-11-30",
        "2024-12-21",
        "2025-01-11",
        "2025-02-01"
      )
    )
  ),
  tar_target(
    name = plot_00_04_nowcasts_vs_data_covid,
    command = get_plot_ag_nowcasts_vs_data(
      nowcasts = age_group_nowcasts |> filter(age_group == "00-04"),
      all_data = clean_weekly_data |> filter(age_group == "00-04"),
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
    name = plot_65_plus_nowcasts_vs_data_covid,
    command = get_plot_ag_nowcasts_vs_data(
      nowcasts = age_group_nowcasts |> filter(age_group == "65+"),
      all_data = clean_weekly_data |> filter(age_group == "65+"),
      max_delay = max_delay,
      pathogen_i = "covid",
      nowcast_dates_to_plot = c(
        "2024-07-20",
        "2024-09-21",
        "2024-12-21",
        "2025-02-22",
        "2025-04-26"
      )
    )
  ),
  tar_target(
    name = bar_chart_model_comp_across_ag_bar,
    command = get_bar_chart_scores(scores_ag_su |>
      filter(pathogen == "bar"))
  ),
  tar_target(
    name = bar_chart_model_comp_across_ag_covid,
    command = get_bar_chart_scores(scores_ag_su |>
      filter(pathogen == "covid"))
  ),
  tar_target(
    name = bar_chart_model_comp_across_ag_flu,
    command = get_bar_chart_scores(scores_ag_su |>
      filter(pathogen == "flu"))
  ),
  tar_target(
    name = bar_chart_model_comp_across_ag_rsv,
    command = get_bar_chart_scores(scores_ag_su |>
      filter(pathogen == "rsv"))
  ),
  tar_target(
    name = fig_ag_nowcast_comp,
    command = make_ag_nowcast_comp_fig(
      plot_00_04_nowcasts_vs_data_rsv,
      plot_65_plus_nowcasts_vs_data_rsv,
      plot_00_04_nowcasts_vs_data_covid,
      plot_65_plus_nowcasts_vs_data_covid,
      bar_chart_model_comp_across_ag_bar,
      bar_chart_model_comp_across_ag_covid,
      bar_chart_model_comp_across_ag_flu,
      bar_chart_model_comp_across_ag_rsv,
      fig_file_name = "fig4_ag_nowcast_comp"
    )
  )
  # Supplemental figures---------------------------------------------
)
