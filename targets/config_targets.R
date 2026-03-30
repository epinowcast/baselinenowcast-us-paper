config_targets <- list(
  tar_target(
    name = pathogens,
    command = tibble(
      pathogen = c("bar", "flu", "covid", "rsv")
    )
  ),
  tar_target(
    name = ma_state_nowcasts_fp,
    command = file.path(
      "output", "ma_nowcasts",
      "bnc_state_nowcasts_dph.csv"
    )
  ),
  tar_target(
    name = ma_ag_nowcasts_fp,
    command = file.path(
      "output", "ma_nowcasts",
      "bnc_age_group_nowcasts_dph.csv"
    )
  ),
  tar_target(
    name = ma_delay_fp_prefix,
    command = file.path(
      "output", "ma_delays",
      "multipliers"
    )
  ),
  tar_target(
    name = temporal_granularity,
    command = "weeks"
  ),
  tar_target(
    name = scale_factor_tv_range,
    command = tibble(
      scale_factor = c(0.5, 1, 1.5)
    )
  ),
  tar_target(
    name = prop_delay_tv_range,
    command = tibble(
      prop_delay = c(0.33, 0.5, 0.67)
    )
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
    name = prev_season_date_range,
    command = tibble(
      nowcast_date = seq(
        from = ymd("2023-07-01"),
        to = ymd("2024-06-30"),
        by = temporal_granularity
      )
    )
  ),
  tar_target(
    name = max_delay,
    command = 10
  ),
  tar_target(
    name = eval_horizon,
    command = 10
  ),
  tar_target(
    name = quantiles_for_scoring,
    command = c(0.975, 0.5, 0.025)
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
