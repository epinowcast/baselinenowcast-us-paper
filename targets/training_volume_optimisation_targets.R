training_volume_optimisation_targets <- list(
  tar_group_by(
    name = tv_scenarios,
    command = crossing(
      scale_factor_tv_range,
      prop_delay_tv_range
    ) |>
      mutate(tv_name = glue::glue("{scale_factor}_vol_{prop_delay}_prop_delay")),
    by = tv_name
  )
)
