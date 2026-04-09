training_volume_optimisation_targets <- list(
  # Generate the optimal for 2023-2024 season
  tar_group_by(
    name = tv_scenarios,
    command = crossing(
      scale_factor_tv_range,
      prop_delay_tv_range,
      pathogens,
      prev_season_date_range
    ) |>
      mutate(tv_name = glue::glue("{pathogen}_{nowcast_date}_{scale_factor}_vol_{prop_delay}_prop_delay")), # nolint
    by = tv_name
  ),
  # Get the state nowcasts as quantiles using baselinenowcast with 9 different
  # training volumes
  tar_target(
    name = state_nowcasts_bnc_tv,
    command = fit_bnc_state(
      all_data = clean_weekly_data,
      nowcast_date = tv_scenarios$nowcast_date,
      pathogen_i = tv_scenarios$pathogen,
      quantiles_for_scoring = quantiles_for_scoring,
      max_delay = max_delay,
      eval_horizon = eval_horizon,
      scale_factor = tv_scenarios$scale_factor,
      prop_delay = tv_scenarios$prop_delay
    ),
    pattern = map(tv_scenarios)
  ),
  tar_target(
    name = scores_tv_su,
    command = state_nowcasts_bnc_tv |>
      as_forecast_quantile(
        predicted = "quantile_value",
        observed = "final_count",
        forecast_unit = c(
          "pathogen",
          "pathogen_name",
          "reference_date",
          "nowcast_date",
          "prop_delay",
          "scale_factor"
        )
      ) |>
      score()
  ),
  tar_target(
    name = bar_chart_tv,
    command = get_bar_chart_tv_scores(scores_tv_su)
  ),
  tar_target(
    name = heatmap_tv,
    command = get_plot_tv_scores(scores_tv_su,
      title = "2023-2024"
    )
  ),
  tar_target(
    name = table_summary,
    command = get_table_min_wis(scores_tv_su)
  ),
  tar_target(
    name = map_tv,
    command = table_summary |>
      select(scale_factor, prop_delay, pathogen)
  ),

  # Generate the optimal using 2024-2025 data---------------------------------
  tar_group_by(
    name = tv_scenarios_latest,
    command = crossing(
      scale_factor_tv_range,
      prop_delay_tv_range,
      pathogens,
      nowcast_date_range
    ) |>
      mutate(tv_name = glue::glue("{pathogen}_{nowcast_date}_{scale_factor}_vol_{prop_delay}_prop_delay")), # nolint
    by = tv_name
  ),
  # Get the state nowcasts as quantiles using baselinenowcast with 9 different
  # training volumes
  tar_target(
    name = state_nowcasts_bnc_tv_latest,
    command = fit_bnc_state(
      all_data = clean_weekly_data,
      nowcast_date = tv_scenarios_latest$nowcast_date,
      pathogen_i = tv_scenarios_latest$pathogen,
      quantiles_for_scoring = quantiles_for_scoring,
      max_delay = max_delay,
      eval_horizon = eval_horizon,
      scale_factor = tv_scenarios_latest$scale_factor,
      prop_delay = tv_scenarios_latest$prop_delay
    ),
    pattern = map(tv_scenarios_latest)
  ),
  tar_target(
    name = scores_tv_su_latest,
    command = state_nowcasts_bnc_tv_latest |>
      as_forecast_quantile(
        predicted = "quantile_value",
        observed = "final_count",
        forecast_unit = c(
          "pathogen",
          "pathogen_name",
          "reference_date",
          "nowcast_date",
          "prop_delay",
          "scale_factor"
        )
      ) |>
      score()
  ),
  tar_target(
    name = bar_chart_tv_latest,
    command = get_bar_chart_tv_scores(scores_tv_su_latest)
  ),
  tar_target(
    name = heatmap_tv_latest,
    command = get_plot_tv_scores(scores_tv_su_latest,
      title = "2024-2025"
    )
  ),
  tar_target(
    name = table_summary_latest,
    command = get_table_min_wis(scores_tv_su_latest)
  )
)
