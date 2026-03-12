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
      mutate(model = "MADPH")
  ),
  tar_target(
    name = age_group_nowcasts_bnc_named,
    command = age_group_nowcasts_bnc
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
  )
)
