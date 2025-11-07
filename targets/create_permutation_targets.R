create_permutation_targets <- list(
  tar_group_by(
    name = scenarios,
    command = crossing(
      pathogens, nowcast_date_range, models
    ) |>
      mutate(scenario_name = paste(pathogen, nowcast_date, model,
        sep = "_"
      )),
    scenario_name
  )
)
