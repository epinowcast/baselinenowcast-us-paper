age_group_nowcast_targets <- list(
  # Get the age group nowcasts as quantiles from both bnc methods--------------
  # "base" is the default (no strata sharing),
  # "strata sharing" uses the delay and uncertainty estimates across age groups
  # for each nowcast
  tar_target(
    name = age_group_nowcasts,
    command = fit_bnc_age_groups(
      all_data = clean_weekly_data,
      nowcast_date = scenarios$nowcast_date,
      pathogen_i = scenarios$pathogen,
      model_type = scenarios$model,
      quantiles_for_scoring = quantiles_for_scoring,
      max_delay = max_delay,
      eval_horizon = eval_horizon,
    ),
    pattern = map(scenarios)
  )
)
