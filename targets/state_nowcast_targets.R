state_nowcast_targets <- list(
  # Get the state nowcasts as quantiles from both methods----------------------
  ## baselinenowcast default method
  tar_target(
    name = state_nowcasts_bnc_full,
    command = fit_bnc_state_from_daily(
      all_data = clean_daily_data,
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
  ## Load in MA state-level nowcasts----------------------------------------
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
      mutate(model = "MADPH (2023 data)")
  ),
  tar_target(
    name = state_nowcasts_bnc_named,
    command = state_nowcasts_bnc |>
      mutate(model = "baselinenowcast")
  ),
  ## Compute MADPH method nowcasts -----------------------------------------
  tar_target(
    name = derived_multipliers_state,
    command = get_mult_from_daily_data_orig(
      # Use only data from 2023
      all_data = raw_data |>
        filter(
          reference_date < "2023-12-30",
          reference_date >= "2023-01-01"
        ),
      max_delay = max_delay,
      source = "MADPH our implementation orig (2023 data)",
      this_age_group = "00+"
    )
  ),
  tar_target(
    name = derived_multipliers_state_revised,
    command = get_multipliers(
      # Use only data from 2023
      all_data = clean_weekly_data |>
        filter(
          end_of_week_reference_date < "2023-12-30",
          end_of_week_reference_date >= "2023-01-01"
        ),
      source = "MADPH our implementation revised (2023 data)",
      this_age_group = "00+"
    )
  ),
  tar_target(
    name = derived_multipliers_state_revised_updated,
    command = get_multipliers(
      # Use only data from 2023
      all_data = clean_weekly_data |>
        filter(
          end_of_week_reference_date < "2025-12-30",
          end_of_week_reference_date >= "2025-01-01"
        ),
      source = "MADPH our implementation revised (2025 data)",
      this_age_group = "00+"
    )
  ),
  tar_target(
    name = state_nowcasts_madph_imp,
    command = implement_madph_method(
      multipliers = derived_multipliers_state,
      all_data = clean_weekly_data,
      age_group = "00+",
      nowcast_date = state_scenarios$nowcast_date,
      pathogen_i = state_scenarios$pathogen,
      max_delay = max_delay,
      eval_horizon = eval_horizon,
      model_name = "MADPH our implementation orig (2023 data)"
    ),
    pattern = map(state_scenarios)
  ),
  tar_target(
    name = state_nowcasts_madph_imp_revised,
    command = implement_madph_method(
      multipliers = derived_multipliers_state_revised,
      all_data = clean_weekly_data,
      age_group = "00+",
      nowcast_date = state_scenarios$nowcast_date,
      pathogen_i = state_scenarios$pathogen,
      max_delay = max_delay,
      eval_horizon = eval_horizon,
      model_name = "MADPH our implementation revised (2023 data)"
    ),
    pattern = map(state_scenarios)
  ),
  tar_target(
    name = state_nowcasts_madph_imp_revised_updated,
    command = implement_madph_method(
      multipliers = derived_multipliers_state_revised_updated,
      all_data = clean_weekly_data,
      age_group = "00+",
      nowcast_date = state_scenarios$nowcast_date,
      pathogen_i = state_scenarios$pathogen,
      max_delay = max_delay,
      eval_horizon = eval_horizon,
      model_name = "MADPH our implementation revised (2025 data)"
    ),
    pattern = map(state_scenarios)
  ),
  ## Combine baselinenowcast and MADPH method--------------------------------
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
  ),
  tar_target(
    name = state_nowcasts_ma_method_comp,
    command = bind_rows(
      state_nowcasts_madph_named,
      state_nowcasts_madph_imp,
      state_nowcasts_madph_imp_revised,
      state_nowcasts_madph_imp_revised_updated,
      state_nowcasts_bnc_named
    ) |>
      select(
        reference_date, quantile_value, quantile_level,
        pathogen, nowcast_date, model, final_count, initial_count,
        pathogen_name
      )
  )
)
