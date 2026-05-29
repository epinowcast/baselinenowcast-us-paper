age_group_nowcast_targets <- list(
  # Get the age group nowcasts as quantiles from both bnc methods--------------
  # "base" is the default (no strata sharing),
  # "strata sharing" uses the delay and uncertainty estimates across age groups
  # for each nowcast
  tar_target(
    name = age_group_nowcasts_bnc,
    command = fit_bnc_age_groups(
      all_data = clean_weekly_data,
      nowcast_date = scenarios$nowcast_date,
      pathogen_i = scenarios$pathogen,
      model = scenarios$model,
      quantiles_for_scoring = quantiles_for_scoring,
      max_delay = max_delay,
      eval_horizon = eval_horizon,
    ),
    pattern = map(scenarios)
  ),
  # Load in MA age group nowcasts
  tar_target(
    name = raw_ag_nowcasts_madph,
    command = get_madph_nowcasts(
      fp = ma_ag_nowcasts_fp
    )
  ),
  tar_target(
    name = age_group_nowcasts_madph,
    command = clean_madph_nowcasts_ag(
      ma_nowcasts = raw_ag_nowcasts_madph
    ),
  ),
  tar_target(
    name = age_group_nowcasts_madph_named,
    command = age_group_nowcasts_madph |>
      mutate(model = "MADPH (2023 data)")
  ),
  tar_target(
    name = age_group_nowcasts_bnc_named,
    command = age_group_nowcasts_bnc
  ),
  # Compute MADPH nowcasts----------------
  tar_target(
    name = derived_multipliers_ag,
    command = get_multipliers_from_daily_data_orig(
      # Use only data from 2023
      all_data = raw_data |>
        filter(
          reference_date < "2023-12-30",
          reference_date >= "2023-01-01"
        ),
      max_delay = max_delay,
      source = "MADPH our implementation orig (2023 data)",
      age_group = age_groups$age_group
    ),
    pattern = age_groups
  ),
  tar_target(
    name = derived_multipliers_revised_ag,
    command = get_multipliers(
      # Use only data from 2023
      all_data = clean_weekly_data |>
        filter(
          end_of_week_reference_date < "2023-12-30",
          end_of_week_reference_date >= "2023-01-01"
        ),
      source = "MADPH our implementation revised (2023 data)",
      age_group = age_groups$age_group
    ),
    pattern = age_groups
  ),
  tar_target(
    name = derived_multipliers_revised_updated_ag,
    command = get_multipliers(
      # Use only data from 2023
      all_data = clean_weekly_data |>
        filter(
          end_of_week_reference_date < "2025-12-30",
          end_of_week_reference_date >= "2025-01-01"
        ),
      source = "MADPH our implementation revised (2025 data)",
      age_group = age_groups$age_group
    ),
    pattern = age_groups
  ),
  tar_target(
    name = nowcasts_madph_imp_ag,
    command = implement_madph_method(
      multipliers = derived_multipliers_ag,
      all_data = clean_weekly_data,
      age_group = "all",
      nowcast_date = state_scenarios$nowcast_date,
      pathogen_i = state_scenarios$pathogen,
      max_delay = max_delay,
      eval_horizon = eval_horizon,
      model_name = "MADPH our implementation orig (2023 data)"
    ),
    pattern = map(state_scenarios)
  ),
  tar_target(
    name = nowcasts_madph_imp_revised_ag,
    command = implement_madph_method(
      multipliers = derived_multipliers_revised_ag,
      all_data = clean_weekly_data,
      age_group = "all",
      nowcast_date = state_scenarios$nowcast_date,
      pathogen_i = state_scenarios$pathogen,
      max_delay = max_delay,
      eval_horizon = eval_horizon,
      model_name = "MADPH our implementation revised (2023 data)"
    ),
    pattern = map(state_scenarios)
  ),
  tar_target(
    name = nowcasts_madph_imp_revised_updated_ag,
    command = implement_madph_method(
      multipliers = derived_multipliers_revised_updated_ag,
      all_data = clean_weekly_data,
      age_group = "all",
      nowcast_date = state_scenarios$nowcast_date,
      pathogen_i = state_scenarios$pathogen,
      max_delay = max_delay,
      eval_horizon = eval_horizon,
      model_name = "MADPH our implementation revised (2025 data)"
    ),
    pattern = map(state_scenarios)
  ),
  tar_target(
    name = age_group_nowcasts,
    command = bind_rows(
      age_group_nowcasts_bnc_named,
      age_group_nowcasts_madph_named
    ) |>
      select(
        reference_date, age_group, quantile_value, quantile_level,
        pathogen, nowcast_date, model, final_count, initial_count,
        pathogen_name
      )
  ),
  tar_target(
    name = age_group_nowcasts_ma_method_comp,
    command = bind_rows(
      age_group_nowcasts_bnc_named,
      age_group_nowcasts_madph_named,
      nowcasts_madph_imp_ag,
      nowcasts_madph_imp_revised_ag,
      nowcasts_madph_imp_revised_updated_ag
    ) |>
      select(
        reference_date, age_group, quantile_value, quantile_level,
        pathogen, nowcast_date, model, final_count, initial_count,
        pathogen_name
      )
  )
)
