state_nowcast_eval_plot_targets <- list(
  # Main text fig
  tar_target(
    name = plot_state_nowcasts_vs_data_bar,
    command = get_plot_nowcasts_vs_data(
      nowcasts = state_nowcasts,
      all_data = clean_weekly_data,
      max_delay = max_delay,
      pathogen_i = "bar",
      fig_name_suffix = "two_methods",
      nowcast_dates_to_plot = example_nowcast_dates
    )
  ),
  tar_target(
    name = plot_state_nowcasts_vs_data_covid,
    command = get_plot_nowcasts_vs_data(
      nowcasts = state_nowcasts,
      all_data = clean_weekly_data,
      max_delay = max_delay,
      pathogen_i = "covid",
      fig_name_suffix = "two_methods",
      nowcast_dates_to_plot = example_nowcast_dates
    )
  ),
  tar_target(
    name = plot_state_nowcasts_vs_data_flu,
    command = get_plot_nowcasts_vs_data(
      nowcasts = state_nowcasts,
      all_data = clean_weekly_data,
      max_delay = max_delay,
      pathogen_i = "flu",
      fig_name_suffix = "two_methods",
      nowcast_dates_to_plot = example_nowcast_dates
    )
  ),
  tar_target(
    name = plot_state_nowcasts_vs_data_rsv,
    command = get_plot_nowcasts_vs_data(
      nowcasts = state_nowcasts,
      all_data = clean_weekly_data,
      max_delay = max_delay,
      pathogen_i = "rsv",
      fig_name_suffix = "two_methods",
      nowcast_dates_to_plot = example_nowcast_dates_rsv
    )
  ),
  tar_target(
    name = bar_chart_scores,
    command = get_bar_chart_scores(scores_su)
  ),
  tar_target(
    name = bar_chart_scores_natural,
    command = get_bar_chart_scores(scores_su_natural)
  ),
  tar_target(
    name = bar_chart_scores_all,
    command = get_bar_chart_scores(scores_su_all)
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
      plot_state_nowcasts_vs_data_covid,
      bar_chart_scores_bar,
      bar_chart_scores_covid,
      bar_chart_scores_flu,
      bar_chart_scores_rsv,
      fig_file_name = "fig3_state_nowcast"
    )
  ),
  # Supp fig restricted to epiweeks 40-19 (flu and RSV only) -------------------
  tar_target(
    name = bar_chart_scores_bar_in_season,
    command = get_bar_chart_scores(
      scores_su |>
        filter(pathogen == "bar") |>
        filter_to_season_epiweeks()
    )
  ),
  tar_target(
    name = bar_chart_scores_covid_in_season,
    command = get_bar_chart_scores(
      scores_su |>
        filter(pathogen == "covid") |>
        filter_to_season_epiweeks(),
      remove_legend = TRUE
    )
  ),
  tar_target(
    name = bar_chart_scores_flu_in_season,
    command = get_bar_chart_scores(
      scores_su |>
        filter(pathogen == "flu") |>
        filter_to_season_epiweeks(),
      remove_legend = TRUE
    )
  ),
  tar_target(
    name = bar_chart_scores_rsv_in_season,
    command = get_bar_chart_scores(
      scores_su |>
        filter(pathogen == "rsv") |>
        filter_to_season_epiweeks(),
      remove_legend = TRUE
    )
  ),
  tar_target(
    name = plot_state_nowcasts_vs_data_covid_in_season,
    command = get_plot_nowcasts_vs_data(
      nowcasts = state_nowcasts,
      all_data = clean_weekly_data,
      max_delay = max_delay,
      pathogen_i = "covid",
      fig_name_suffix = "two_methods_in_season",
      nowcast_dates_to_plot = example_nowcast_dates_in_season,
      reference_date_range = in_season_date_range
    )
  ),
  tar_target(
    name = fig_state_nowcast_comp_in_season,
    command = make_state_nowcast_comp_fig(
      plot_state_nowcasts_vs_data_covid_in_season,
      bar_chart_scores_bar_in_season,
      bar_chart_scores_covid_in_season,
      bar_chart_scores_flu_in_season,
      bar_chart_scores_rsv_in_season,
      fig_file_name = "fig3_state_nowcast_in_season"
    )
  ),

  # Alternate main -------------------------------------------
  tar_target(
    name = plot_state_nowcasts_vs_data_bar2,
    command = get_plot_nowcasts_vs_data(
      nowcasts = state_nowcasts2,
      all_data = clean_weekly_data,
      max_delay = max_delay,
      pathogen_i = "bar",
      fig_name_suffix = "two_methods",
      nowcast_dates_to_plot = example_nowcast_dates
    )
  ),
  tar_target(
    name = bar_chart_scores_bar2,
    command = get_bar_chart_scores(scores_su2 |>
      filter(pathogen == "bar"))
  ),
  tar_target(
    name = bar_chart_scores_covid2,
    command = get_bar_chart_scores(
      scores_su2 |>
        filter(pathogen == "covid"),
      remove_legend = TRUE
    )
  ),
  tar_target(
    name = bar_chart_scores_flu2,
    command = get_bar_chart_scores(
      scores_su2 |>
        filter(pathogen == "flu"),
      remove_legend = TRUE
    )
  ),
  tar_target(
    name = bar_chart_scores_rsv2,
    command = get_bar_chart_scores(
      scores_su2 |>
        filter(pathogen == "rsv"),
      remove_legend = TRUE
    )
  ),
  tar_target(
    name = fig_state_nowcast_comp2,
    command = make_state_nowcast_comp_fig(
      plot_state_nowcasts_vs_data_bar2,
      bar_chart_scores_bar2,
      bar_chart_scores_covid2,
      bar_chart_scores_flu2,
      bar_chart_scores_rsv2,
      fig_file_name = "fig3_state_nowcast_comp2"
    )
  ),
  # Supp fig comparing weekly daily variations --------------------------------
  tar_target(
    name = plot_state_nowcasts_vs_data_bar_alt,
    command = get_plot_nowcasts_vs_data(
      nowcasts = state_nowcasts_alt,
      all_data = clean_weekly_data,
      max_delay = max_delay,
      pathogen_i = "bar",
      fig_name_suffix = "two_methods",
      nowcast_dates_to_plot = example_nowcast_dates
    )
  ),
  tar_target(
    name = plot_state_nowcasts_vs_data_covid_alt,
    command = get_plot_nowcasts_vs_data(
      nowcasts = state_nowcasts_alt,
      all_data = clean_weekly_data,
      max_delay = max_delay,
      pathogen_i = "covid",
      fig_name_suffix = "two_methods",
      nowcast_dates_to_plot = example_nowcast_dates
    )
  ),
  tar_target(
    name = plot_state_nowcasts_vs_data_flu_alt,
    command = get_plot_nowcasts_vs_data(
      nowcasts = state_nowcasts_alt,
      all_data = clean_weekly_data,
      max_delay = max_delay,
      pathogen_i = "flu",
      fig_name_suffix = "two_methods",
      nowcast_dates_to_plot = example_nowcast_dates
    )
  ),
  tar_target(
    name = plot_state_nowcasts_vs_data_rsv_alt,
    command = get_plot_nowcasts_vs_data(
      nowcasts = state_nowcasts_alt,
      all_data = clean_weekly_data,
      max_delay = max_delay,
      pathogen_i = "rsv",
      fig_name_suffix = "two_methods",
      nowcast_dates_to_plot = example_nowcast_dates
    )
  ),
  tar_target(
    name = bar_chart_scores_bar_alt,
    command = get_bar_chart_scores(scores_su_alt |>
      filter(pathogen == "bar"))
  ),
  tar_target(
    name = bar_chart_scores_covid_alt,
    command = get_bar_chart_scores(
      scores_su_alt |>
        filter(pathogen == "covid"),
      remove_legend = TRUE
    )
  ),
  tar_target(
    name = bar_chart_scores_flu_alt,
    command = get_bar_chart_scores(
      scores_su_alt |>
        filter(pathogen == "flu"),
      remove_legend = TRUE
    )
  ),
  tar_target(
    name = bar_chart_scores_rsv_alt,
    command = get_bar_chart_scores(
      scores_su_alt |>
        filter(pathogen == "rsv"),
      remove_legend = TRUE
    )
  ),
  tar_target(
    name = fig_state_nowcast_comp_alt,
    command = make_state_nowcast_comp_fig(
      plot_state_nowcasts_vs_data_bar_alt,
      bar_chart_scores_bar_alt,
      bar_chart_scores_covid_alt,
      bar_chart_scores_flu_alt,
      bar_chart_scores_rsv_alt,
      fig_file_name = "fig3_state_nowcast_comp_alt"
    )
  ),

  # Supp fig DPH method comparisons --------------------------------
  tar_target(
    name = plot_state_nowcasts_vs_data_bar_comp,
    command = get_plot_nowcasts_vs_data(
      nowcasts = state_nowcasts_ma_method_comp,
      all_data = clean_weekly_data,
      max_delay = max_delay,
      pathogen_i = "bar",
      fig_name_suffix = "method_comp",
      nowcast_dates_to_plot = example_nowcast_dates
    )
  ),
  tar_target(
    name = plot_state_nowcasts_vs_data_covid_comp,
    command = get_plot_nowcasts_vs_data(
      nowcasts = state_nowcasts_ma_method_comp,
      all_data = clean_weekly_data,
      max_delay = max_delay,
      pathogen_i = "covid",
      fig_name_suffix = "method_comp",
      nowcast_dates_to_plot = example_nowcast_dates
    )
  ),
  tar_target(
    name = plot_state_nowcasts_vs_data_flu_comp,
    command = get_plot_nowcasts_vs_data(
      nowcasts = state_nowcasts_ma_method_comp,
      all_data = clean_weekly_data,
      max_delay = max_delay,
      pathogen_i = "flu",
      fig_name_suffix = "method_comp",
      nowcast_dates_to_plot = example_nowcast_dates
    )
  ),
  tar_target(
    name = plot_state_nowcasts_vs_data_rsv_comp,
    command = get_plot_nowcasts_vs_data(
      nowcasts = state_nowcasts_ma_method_comp,
      all_data = clean_weekly_data,
      max_delay = max_delay,
      pathogen_i = "rsv",
      fig_name_suffix = "method_comp",
      nowcast_dates_to_plot = example_nowcast_dates
    )
  ),
  tar_target(
    name = bar_chart_scores_bar_comp,
    command = get_bar_chart_scores(scores_su_method_comp |>
      filter(pathogen == "bar"))
  ),
  tar_target(
    name = bar_chart_scores_covid_comp,
    command = get_bar_chart_scores(
      scores_su_method_comp |>
        filter(pathogen == "covid"),
      remove_legend = TRUE
    )
  ),
  tar_target(
    name = bar_chart_scores_flu_comp,
    command = get_bar_chart_scores(
      scores_su_method_comp |>
        filter(pathogen == "flu"),
      remove_legend = TRUE
    )
  ),
  tar_target(
    name = bar_chart_scores_rsv_comp,
    command = get_bar_chart_scores(
      scores_su_method_comp |>
        filter(pathogen == "rsv"),
      remove_legend = TRUE
    )
  ),
  tar_target(
    name = fig_state_nowcast_comp_methods,
    command = make_state_nowcast_comp_fig(
      plot_state_nowcasts_vs_data_covid_comp,
      bar_chart_scores_bar_comp,
      bar_chart_scores_covid_comp,
      bar_chart_scores_flu_comp,
      bar_chart_scores_rsv_comp,
      fig_file_name = "fig3_state_nowcast_comp"
    )
  ),


  # Supp fig real-time vs retrospective DPH nowcasts ----------------------
  tar_target(
    name = plot_state_nowcasts_vs_data_covid_dph_rt_retro,
    command = get_plot_nowcasts_vs_data(
      nowcasts = state_nowcasts_dph_rt_retro_comp,
      all_data = clean_weekly_data,
      max_delay = max_delay,
      pathogen_i = "covid",
      fig_name_suffix = "dph_rt_retro",
      nowcast_dates_to_plot = example_nowcast_dates
    )
  ),
  tar_target(
    name = bar_chart_scores_bar_dph_rt_retro,
    command = get_bar_chart_scores(
      scores_su_dph_rt_retro_comp |>
        filter(pathogen == "bar")
    )
  ),
  tar_target(
    name = bar_chart_scores_covid_dph_rt_retro,
    command = get_bar_chart_scores(
      scores_su_dph_rt_retro_comp |>
        filter(pathogen == "covid"),
      remove_legend = TRUE
    )
  ),
  tar_target(
    name = bar_chart_scores_flu_dph_rt_retro,
    command = get_bar_chart_scores(
      scores_su_dph_rt_retro_comp |>
        filter(pathogen == "flu"),
      remove_legend = TRUE
    )
  ),
  tar_target(
    name = bar_chart_scores_rsv_dph_rt_retro,
    command = get_bar_chart_scores(
      scores_su_dph_rt_retro_comp |>
        filter(pathogen == "rsv"),
      remove_legend = TRUE
    )
  ),
  tar_target(
    name = fig_state_nowcast_dph_rt_retro,
    command = make_state_nowcast_comp_fig(
      plot_state_nowcasts_vs_data_covid_dph_rt_retro,
      bar_chart_scores_bar_dph_rt_retro,
      bar_chart_scores_covid_dph_rt_retro,
      bar_chart_scores_flu_dph_rt_retro,
      bar_chart_scores_rsv_dph_rt_retro,
      fig_file_name = "fig3_state_nowcast_dph_rt_retro"
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
  tar_target(
    name = state_wis_over_time_in_season,
    command = get_state_wis_over_time_plot(
      all_data = clean_weekly_data |>
        filter_to_season_epiweeks(date_col = "end_of_week_reference_date"),
      scores = scores_su |>
        filter_to_season_epiweeks(),
      nowcasts = state_nowcasts,
      max_delay = max_delay,
      fig_file_name = "state_wis_t_in_season"
    )
  ),

  # Coverage for each of the models and pathogens
  tar_target(
    name = bar_chart_coverage,
    command = get_bar_chart_coverage(
      coverage = coverage_state,
      title = "95% interval coverage state-level nowcasts",
      fig_file_name = "state_coverage"
    )
  ),
  tar_target(
    name = bar_chart_coverage_alt,
    command = get_bar_chart_coverage(
      coverage = coverage_state_alt,
      title = "95% interval coverage state-level nowcasts",
      fig_file_name = "state_coverage_alt"
    )
  ),
  # Horizon 0 nowcasts for all pathogens
  tar_target(
    name = nowcasts_by_horizon_0_bar_00plus,
    command = get_plot_nowcasts_over_time(
      state_nowcasts_all |>
        filter(model %in%
          c(
            "DPH method (real-time)",
            "baselinenowcast"
          )) |>
        mutate(age_group = "00+"),
      horizon_to_plot = 0,
      age_group_to_plot = "00+",
      pathogen_to_plot = "bar",
      fig_file_name = "bar_horizon_0_00+"
    )
  ),
  tar_target(
    name = nowcasts_by_horizon_0_covid_00plus,
    command = get_plot_nowcasts_over_time(
      state_nowcasts_all |>
        filter(model %in%
          c(
            "DPH method (real-time)",
            "baselinenowcast"
          )) |>
        mutate(age_group = "00+"),
      horizon_to_plot = 0,
      age_group_to_plot = "00+",
      pathogen_to_plot = "covid",
      fig_file_name = "covid_horizon_0_00+"
    )
  ),
  tar_target(
    name = nowcasts_by_horizon_0_flu_00plus,
    command = get_plot_nowcasts_over_time(
      state_nowcasts_all |>
        filter(model %in%
          c(
            "DPH method (real-time)",
            "baselinenowcast"
          )) |>
        mutate(age_group = "00+"),
      horizon_to_plot = 0,
      age_group_to_plot = "00+",
      pathogen_to_plot = "flu",
      fig_file_name = "flu_horizon_0_00+"
    )
  ),
  tar_target(
    name = nowcasts_by_horizon_0_rsv_00plus,
    command = get_plot_nowcasts_over_time(
      state_nowcasts_all |>
        filter(model %in%
          c(
            "DPH method (real-time)",
            "baselinenowcast"
          )) |>
        mutate(age_group = "00+"),
      horizon_to_plot = 0,
      age_group_to_plot = "00+",
      pathogen_to_plot = "rsv",
      fig_file_name = "rsv_horizon_0_00+"
    )
  ),
  tar_target(
    name = nowcasts_by_horizon_1wk_rsv_00plus,
    command = get_plot_nowcasts_over_time(
      state_nowcasts_all |>
        filter(model %in%
          c(
            "DPH method (real-time)",
            "baselinenowcast"
          )) |>
        mutate(age_group = "00+"),
      horizon_to_plot = -1,
      age_group_to_plot = "00+",
      pathogen_to_plot = "rsv",
      fig_file_name = "rsv_horizon_1wk_00+"
    )
  ),

  ## Comparison of the two DPH methods-------------------------------------
  tar_target(
    name = plot_state_nowcasts_vs_data_bar_DPH,
    command = get_plot_nowcasts_vs_data(
      nowcasts = state_nowcasts_ma_method_comp,
      all_data = clean_weekly_data,
      max_delay = max_delay,
      pathogen_i = "bar",
      nowcast_dates_to_plot = example_nowcast_dates
    )
  ),
  tar_target(
    name = plot_state_nowcasts_vs_data_covid_DPH,
    command = get_plot_nowcasts_vs_data(
      nowcasts = state_nowcasts_ma_method_comp,
      all_data = clean_weekly_data,
      max_delay = max_delay,
      pathogen_i = "covid",
      nowcast_dates_to_plot = example_nowcast_dates
    )
  ),
  tar_target(
    name = plot_state_nowcasts_vs_data_flu_DPH,
    command = get_plot_nowcasts_vs_data(
      nowcasts = state_nowcasts_ma_method_comp,
      all_data = clean_weekly_data,
      max_delay = max_delay,
      pathogen_i = "flu",
      nowcast_dates_to_plot = example_nowcast_dates
    )
  ),
  tar_target(
    name = plot_state_nowcasts_vs_data_rsv_DPH,
    command = get_plot_nowcasts_vs_data(
      nowcasts = state_nowcasts_ma_method_comp,
      all_data = clean_weekly_data,
      max_delay = max_delay,
      pathogen_i = "rsv",
      nowcast_dates_to_plot = example_nowcast_dates
    )
  ),
  tar_target(
    name = nowcasts_by_horizon_0_bar_00plus_DPH,
    command = get_plot_nowcasts_over_time(
      state_nowcasts_ma_method_comp |> mutate(age_group = "00+"),
      horizon_to_plot = 0,
      age_group_to_plot = "00+",
      pathogen_to_plot = "bar",
      fig_file_name = "bar_horizon_0_00+_3_methods"
    )
  ),
  tar_target(
    name = nowcasts_by_horizon_0_rsv_00plus_DPH,
    command = get_plot_nowcasts_over_time(
      state_nowcasts_ma_method_comp |> mutate(age_group = "00+"),
      horizon_to_plot = 0,
      age_group_to_plot = "00+",
      pathogen_to_plot = "rsv",
      fig_file_name = "rsv_horizon_0_00+_3_methods"
    )
  ),
  tar_target(
    name = nowcasts_by_horizon_0_covid_00plus_DPH,
    command = get_plot_nowcasts_over_time(
      state_nowcasts_ma_method_comp |> mutate(age_group = "00+"),
      horizon_to_plot = 0,
      age_group_to_plot = "00+",
      pathogen_to_plot = "covid",
      fig_file_name = "covid_horizon_0_00+_3_methods"
    )
  ),
  tar_target(
    name = nowcasts_by_horizon_0_flu_00plus_DPH,
    command = get_plot_nowcasts_over_time(
      state_nowcasts_ma_method_comp |> mutate(age_group = "00+"),
      horizon_to_plot = 0,
      age_group_to_plot = "00+",
      pathogen_to_plot = "flu",
      fig_file_name = "flu_horizon_0_00+_3_methods"
    )
  )
)
