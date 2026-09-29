age_group_nowcast_targets <- list(
  # Get the age group nowcasts as quantiles from both bnc methods--------------
  # "base" is the default (no strata sharing),
  # "strata sharing" uses the delay and uncertainty estimates across age groups
  # for each nowcast
  tar_target(
    name = age_group_nowcasts_bnc,
    command = fit_bnc_age_groups_from_daily(
      all_data = clean_daily_data,
      nowcast_date = scenarios$nowcast_date,
      pathogen_i = scenarios$pathogen,
      model = scenarios$model,
      quantiles_for_scoring = quantiles_for_scoring,
      max_delay = max_delay,
      scale_factor = scenarios$scale_factor,
      prop_delay = scenarios$prop_delay,
      eval_horizon = eval_horizon,
    ),
    pattern = map(scenarios)
  ),
  tar_target(
    name = age_group_nowcasts_bnc_dw_raw,
    command = fit_bnc_age_groups_wkly_dly(
      all_data = clean_daily_data,
      nowcast_date = scenarios$nowcast_date,
      pathogen_i = scenarios$pathogen,
      model = scenarios$model,
      quantiles_for_scoring = quantiles_for_scoring,
      max_delay = max_delay,
      scale_factor = scenarios$scale_factor,
      prop_delay = scenarios$prop_delay,
      eval_horizon = eval_horizon,
    ),
    pattern = map(scenarios)
  ),
  tar_target(
    name = age_group_nowcasts_bnc_weekly_raw,
    command = fit_bnc_age_groups(
      all_data = clean_weekly_data,
      nowcast_date = scenarios$nowcast_date,
      pathogen_i = scenarios$pathogen,
      model = scenarios$model,
      quantiles_for_scoring = quantiles_for_scoring,
      max_delay = max_delay,
      scale_factor = scenarios$scale_factor,
      prop_delay = scenarios$prop_delay,
      eval_horizon = eval_horizon,
    ),
    pattern = map(scenarios)
  ),
  tar_target(
    name = age_group_nowcasts_bnc_weekly,
    command = age_group_nowcasts_bnc_weekly_raw |>
      mutate(model = ifelse(model == "baselinenowcast base", "baselinenowcast weekly",
        "baselinenowcast strata sharing weekly"
      ))
  ),
  tar_target(
    name = age_group_nowcasts_bnc_daily,
    command = age_group_nowcasts_bnc |>
      mutate(model = ifelse(model == "baselinenowcast base", "baselinenowcast",
        "baselinenowcast strata sharing"
      ))
  ),
  tar_target(
    name = age_group_nowcasts_bnc_dw,
    command = age_group_nowcasts_bnc_dw_raw |>
      mutate(model = ifelse(model == "baselinenowcast base", "baselinenowcast weekly reference daily reports",
        "baselinenowcast strata sharing weekly reference daily reports"
      ))
  ),

  # Load in MA age group nowcasts
  tar_target(
    name = raw_ag_nowcasts_dph,
    command = get_dph_nowcasts(
      fp = ma_ag_nowcasts_fp
    )
  ),
  tar_target(
    name = age_group_nowcasts_dph,
    command = clean_dph_nowcasts_ag(
      ma_nowcasts = raw_ag_nowcasts_dph,
      eval_horizon = eval_horizon
    ) |>
      add_ground_truth_counts(age_group_nowcasts_bnc),
  ),
  tar_target(
    name = age_group_nowcasts_dph_named,
    command = age_group_nowcasts_dph |>
      mutate(model = "DPH method")
  ),
  # Compute DPH nowcasts----------------
  tar_target(
    name = derived_multipliers_ag,
    command = get_mult_from_daily_data_orig(
      # Use only data from 2023
      all_data = clean_daily_data |>
        filter(
          reference_date < "2023-12-30",
          reference_date >= "2023-01-01"
        ),
      max_delay = max_delay,
      source = "DPH our implementation orig",
      this_age_group = age_groups$age_group
    ),
    pattern = age_groups
  ),
  tar_target(
    name = derived_multipliers_revised_ag,
    command = get_mult_from_weekly_data_rev(
      # Use only data from 2023
      all_data = clean_weekly_data |>
        filter(
          end_of_week_reference_date < "2023-12-30",
          end_of_week_reference_date >= "2023-01-01"
        ),
      source = "DPH revised",
      this_age_group = age_groups$age_group
    ),
    pattern = age_groups
  ),
  tar_target(
    name = nowcasts_dph_imp_ag,
    command = impl_dph_method_from_daily(
      multipliers = derived_multipliers_ag,
      all_data = clean_daily_data,
      age_group = "all",
      nowcast_date = state_scenarios$nowcast_date,
      pathogen_i = state_scenarios$pathogen,
      max_delay = max_delay,
      eval_horizon = eval_horizon,
      model_name = "DPH our implementation orig"
    ),
    pattern = map(state_scenarios)
  ),
  tar_target(
    name = nowcasts_dph_imp_revised_ag,
    command = impl_dph_method_from_weekly(
      multipliers = derived_multipliers_revised_ag,
      all_data = clean_weekly_data,
      age_group = "all",
      nowcast_date = state_scenarios$nowcast_date,
      pathogen_i = state_scenarios$pathogen,
      max_delay = max_delay,
      eval_horizon = eval_horizon,
      model_name = "DPH revised"
    ),
    pattern = map(state_scenarios)
  ),
  tar_target(
    name = age_group_nowcasts,
    command = bind_rows(
      age_group_nowcasts_bnc_daily,
      age_group_nowcasts_dph_named
    ) |>
      select(
        reference_date, age_group, quantile_value, quantile_level,
        pathogen, nowcast_date, model, final_count, initial_count,
        pathogen_name
      )
  ),
  tar_target(
    name = prelim_data_as_model_ag,
    command = age_group_nowcasts_bnc_daily |>
      mutate(
        model = "preliminary data",
        quantile_value = initial_count
      )
  ),
  tar_target(
    name = age_group_nowcasts2,
    command = bind_rows(
      age_group_nowcasts_bnc_daily,
      nowcasts_dph_imp_revised_ag,
    ) |>
      select(
        reference_date, age_group, quantile_value, quantile_level,
        pathogen, nowcast_date, model, final_count, initial_count,
        pathogen_name
      )
  ),
  tar_target(
    name = age_group_nowcasts_all,
    command = bind_rows(
      age_group_nowcasts_bnc_dw,
      age_group_nowcasts_bnc_daily,
      age_group_nowcasts_bnc_weekly,
      age_group_nowcasts_dph_named,
      nowcasts_dph_imp_revised_ag,
      prelim_data_as_model_ag
    ) |>
      select(
        reference_date, age_group, quantile_value, quantile_level,
        pathogen, nowcast_date, model, final_count, initial_count,
        pathogen_name
      )
  ),
  tar_target(
    name = age_group_nowcasts_alt,
    command = bind_rows(
      age_group_nowcasts_bnc_daily,
      age_group_nowcasts_bnc_weekly,
      age_group_nowcasts_bnc_dw
    ) |>
      select(
        reference_date, age_group, quantile_value, quantile_level,
        pathogen, nowcast_date, model, final_count, initial_count,
        pathogen_name
      ) |>
      mutate(model = case_when(
        model == "baselinenowcast" ~ "baselinenowcast daily",
        model == "baselinenowcast strata sharing" ~ "baselinenowcast strata sharing daily",
        TRUE ~ model
      ))
  ),
  tar_target(
    name = age_group_nowcasts_ma_method_comp,
    command = bind_rows(
      age_group_nowcasts_dph_named,
      age_group_nowcasts_bnc_daily,
      nowcasts_dph_imp_revised_ag
    ) |>
      select(
        reference_date, age_group, quantile_value, quantile_level,
        pathogen, nowcast_date, model, final_count, initial_count,
        pathogen_name
      )
  )
)
