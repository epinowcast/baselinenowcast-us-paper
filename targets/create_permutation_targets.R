create_permutation_targets <- list(
  # State level nowcasts, only uses baselinenowcast without strata sharing
  tar_group_by(
    name = state_scenarios_raw,
    command = crossing(
      pathogens,
      nowcast_date_range
    ) |>
      mutate(scenario_name = paste(nowcast_date, pathogen, sep = "_")),
    by = scenario_name
  ),
  tar_target(
    name = state_scenarios,
    command = state_scenarios_raw |>
      left_join(map_tv, by = "pathogen"),
    pattern = map(state_scenarios_raw)
  ),
  # Age group nowcasts, using both age group independent and strata sharing
  # across age groups
  tar_group_by(
    name = scenarios_raw,
    command = crossing(
      pathogens, nowcast_date_range, models
    ) |>
      mutate(scenario_name = paste(pathogen, nowcast_date, model,
        sep = "_"
      )),
    scenario_name
  ),
  # Join the optimal training volume to the scenarios
  tar_target(
    name = scenarios,
    command = scenarios_raw |>
      left_join(map_tv, by = "pathogen"),
    pattern = map(scenarios_raw)
  )
)
