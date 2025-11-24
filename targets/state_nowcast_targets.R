state_nowcast_targets <- list(
  tar_group_by(
    name = state_scenarios,
    command = crossing(nowcast_date_range, pathogens) |>
      mutate(scenario_name = paste(nowcast_date, pathogen, sep = "_")),
    by = scenario_name
  ),
  # Get the state nowcasts as quantiles from both methods----------------------
  ## baselinenowcast default method
  tar_target(
    name = state_nowcasts_bnc,
    command = fit_bnc_state(
      all_data = clean_weekly_data,
      nowcast_date = state_scenarios$nowcast_date,
      pathogen_i = state_scenarios$pathogen,
      quantiles_for_scoring = quantiles_for_scoring,
      max_delay = max_delay,
      eval_horizon = eval_horizon,
    ),
    pattern = map(state_scenarios)
  ),
  ## PLACEHOLDER MADPH nowcasts (just use longer training vol so they differ)
  tar_target(
    name = state_nowcasts_madph,
    command = fit_bnc_state(
      all_data = clean_weekly_data,
      nowcast_date = state_scenarios$nowcast_date,
      pathogen_i = state_scenarios$pathogen,
      quantiles_for_scoring = quantiles_for_scoring,
      max_delay = max_delay,
      eval_horizon = eval_horizon,
      scale_factor = 5,
    ),
    pattern = map(state_scenarios)
  ),
  tar_target(
    name = state_nowcasts_madph_named,
    command = state_nowcasts_madph |>
      mutate(model = "MADPH")
  ),
  tar_target(
    name = state_nowcasts_bnc_named,
    command = state_nowcasts_bnc |>
      mutate(model = "baselinenowcast")
  ),
  tar_target(
    name = state_nowcasts,
    command = bind_rows(
      state_nowcasts_bnc_named,
      state_nowcasts_madph_named
    ) |>
      select(
        reference_date, quantile_value, quantile_level,
        pathogen, nowcast_date, model, final_count, initial_count,
        pathogen_name
      )
  )
)
