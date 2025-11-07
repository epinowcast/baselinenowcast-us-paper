config_targets <- list(
  tar_target(
    name = pathogens,
    command = tibble(
      pathogen = c("bar", "flu", "covid", "rsv")
    )
  ),
  tar_target(
    name = temporal_granularity,
    command = "weeks"
  ),
  tar_target(
    name = nowcast_date_range,
    command = tibble(
      nowcast_date = seq(
        from = ymd("2024-06-29"),
        to = ymd("2025-06-28"),
        by = temporal_granularity
      )
    )
  ),
  tar_target(
    name = max_delay,
    command = 8
  ),
  tar_target(
    name = eval_horizon,
    command = 8
  ),
  # This is for MA implementantation
  tar_target(
    name = delay_est_training_date_range,
    command = seq(
      from = ymd("2023-01-07"),
      to = ymd("2024-01-06"),
      by = temporal_granularity
    )
  ),
  tar_target(
    name = models,
    command = tibble(
      model = c(
        "baselinenowcast base",
        "baselinenowcast strata sharing"
      )
    )
  ),
  tar_target(
    name = pathogen_data_fp,
    command = file.path("input", "data")
  )
)
