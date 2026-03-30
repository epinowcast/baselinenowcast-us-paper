training_volume_optimisation_targets <- list(
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
          "model",
          "prop_delay",
          "scale_factor"
        )
      ) |>
      score()
  )
)
