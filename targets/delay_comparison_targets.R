delay_comparison_targets <- list(
  # Get the single static delay distribution used in the MADPH nowcasts
  tar_target(
    name = full_nowcast_date_range,
    command = bind_rows(
      prev_season_date_range,
      nowcast_date_range
    ) |>
      distinct(nowcast_date)
  ),
  tar_group_by(
    name = all_scenarios,
    command = crossing(
      pathogens,
      full_nowcast_date_range
    ) |>
      mutate(scenario_name = paste(nowcast_date, pathogen, sep = "_")),
    by = scenario_name
  ),
  tar_target(
    name = ma_delay,
    command = get_ma_delay_data(
      fp_prefix = ma_delay_fp_prefix,
      pathogen = pathogens_grouped$pathogen,
      max_delay = max_delay
    ),
    pattern = pathogens_grouped
  ),
  # Get the delay distribution used in each baselinenowcast nowcast (across
  # all age groups)
  tar_target(
    name = delay_dfs_bnc,
    command = get_delay_df(clean_weekly_data,
      nowcast_date = all_scenarios$nowcast_date,
      pathogen = all_scenarios$pathogen,
      max_delay = max_delay
    ),
    pattern = all_scenarios
  ),
  tar_target(
    name = plot_delay_comparison_pdf,
    command = get_plot_delay_comparison(
      ma_delay = ma_delay,
      delay_dfs_bnc = delay_dfs_bnc,
      plot_type = "pdf"
    )
  ),
  tar_target(
    name = plot_delay_comparison_cdf,
    command = get_plot_delay_comparison(
      ma_delay = ma_delay,
      delay_dfs_bnc = delay_dfs_bnc,
      plot_type = "cdf"
    )
  ),
  tar_target(
    name = plot_bar_chart_mean_delay,
    command = get_bar_chart_mean_delay_comp(
      ma_delay = ma_delay,
      delay_dfs_bnc = delay_dfs_bnc
    )
  )
)
