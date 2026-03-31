state_nowcast_targets <- list(
  # Get the state nowcasts as quantiles from both methods----------------------
  ## baselinenowcast default method
  tar_target(
    name = state_nowcasts_bnc_full,
    command = fit_bnc_state(
      all_data = clean_weekly_data,
      nowcast_date = state_scenarios$nowcast_date,
      pathogen_i = state_scenarios$pathogen,
      quantiles_for_scoring = quantiles_for_scoring,
      max_delay = max_delay,
      eval_horizon = eval_horizon,
      prop_delay = state_scenarios$prop_delay,
      scale_factor = state_scenarios$scale_factor
    ),
    pattern = map(state_scenarios)
  ),
  tar_target(
    name = state_nowcasts_bnc,
    command = state_nowcasts_bnc_full |> distinct()
  ),
  # Load in MA state-level nowcasts
  tar_target(
    name = raw_state_nowcasts_madph,
    command = get_madph_nowcasts(
      fp = ma_state_nowcasts_fp
    )
  ),
  tar_target(
    name = state_nowcasts_madph,
    command = clean_madph_nowcasts(
      ma_nowcasts = raw_state_nowcasts_madph
    ),
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
