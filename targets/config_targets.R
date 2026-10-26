config_targets <- list(
  tar_target(
    name = pathogens,
    command = tibble(
      pathogen = c("bar", "flu", "covid", "rsv")
    )
  ),
  # State-level DPH nowcasts produced in real-time
  tar_target(
    name = ma_state_nowcasts_realtime_fp,
    command = file.path(
      "output", "ma_nowcasts_latest_season",
      "dph_state_nowcasts_realtime.csv"
    ),
    format = "file"
  ),
  # State-level DPH nowcasts produced retrospectively
  tar_target(
    name = ma_state_nowcasts_retro_fp,
    command = file.path(
      "output", "ma_nowcasts_latest_season",
      "dph_state_nowcasts_retro.csv"
    ),
    format = "file"
  ),
  # Age group DPH nowcasts (only produced retrospectively)
  tar_target(
    name = ma_ag_nowcasts_fp,
    command = file.path(
      "output", "ma_nowcasts_latest_season",
      "dph_age_group_nowcasts.csv"
    ),
    format = "file"
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
      scale_factor = c(0.5 * 3, 3, 1.5 * 3)
    )
  ),
  tar_target(
    name = prop_delay_tv_range,
    command = tibble(
      prop_delay = c(0.33, 0.5, 0.67)
    )
  ),
  # Wednesdays matching the real-time DPH nowcast dates (Saturdays + 4 days)
  tar_target(
    name = nowcast_date_range,
    command = tibble(
      nowcast_date = seq(
        from = ymd("2025-08-13"),
        to = ymd("2026-07-08"),
        by = temporal_granularity
      )
    )
  ),
  tar_target(
    name = seasons,
    command = c("2023-2024", "2024-2025", "2025-2026")
  ),
  tar_target(
    name = current_season_date_range,
    command = tibble(
      nowcast_date = seq(
        from = ymd("2025-07-09"),
        to = ymd("2026-07-08"),
        by = temporal_granularity
      )
    )
  ),
  tar_target(
    name = first_season_date_range,
    command = tibble(
      nowcast_date = seq(
        from = ymd("2023-07-12"),
        to = ymd("2024-07-03"),
        by = temporal_granularity
      )
    )
  ),
  tar_target(
    name = prev_season_date_range,
    command = tibble(
      nowcast_date = seq(
        from = ymd("2024-07-10"),
        to = ymd("2025-07-02"),
        by = temporal_granularity
      )
    )
  ),
  # Training volume optimisation uses only data available before the
  # evaluation season (reports before August 2025), so only nowcast dates
  # with max_delay weeks of follow-up before the cutoff can be scored
  tar_target(
    name = tv_data_cutoff,
    command = ymd("2025-08-01")
  ),
  tar_target(
    name = tv_date_range,
    command = prev_season_date_range |>
      filter(nowcast_date + weeks(max_delay) < tv_data_cutoff)
  ),
  # Example nowcast dates shown in the nowcasts vs data figures ---------------
  # Evenly spaced across the evaluation season
  tar_target(
    name = example_nowcast_dates,
    command = c(
      "2025-08-13", "2025-10-15", "2025-12-31", "2026-03-04", "2026-05-20"
    )
  ),
  # The evenly spaced dates whose most recent reference week (ending the
  # Saturday before the nowcast date) is in season (epiweeks 40 to 19)
  tar_target(
    name = example_nowcast_dates_in_season,
    command = example_nowcast_dates[
      epiweek(ymd(example_nowcast_dates) - days(4)) >= 40 |
        epiweek(ymd(example_nowcast_dates) - days(4)) <= 19
    ]
  ),
  # First and last reference weeks (ending Saturdays) of the evaluation season
  # in epiweeks 40 to 19
  tar_target(
    name = in_season_date_range,
    command = {
      week_ends <- current_season_date_range$nowcast_date - days(4)
      range(week_ends[epiweek(week_ends) >= 40 | epiweek(week_ends) <= 19])
    }
  ),
  # Centred on the RSV peak
  tar_target(
    name = example_nowcast_dates_rsv,
    command = c(
      "2025-10-15", "2025-11-05", "2025-11-19", "2026-01-28", "2026-02-04",
      "2026-02-11", "2026-02-18", "2026-02-25", "2026-03-04", "2026-03-11"
    )
  ),
  tar_target(
    name = example_nowcast_dates_rsv_ag,
    command = c("2025-12-31", "2026-02-04", "2026-02-25", "2026-03-18")
  ),
  tar_target(
    name = example_nowcast_dates_rsv_ag_detail,
    command = c(
      "2026-01-07", "2026-01-28", "2026-02-18", "2026-03-11", "2026-04-01"
    )
  ),
  # Centred on the COVID-19 peak
  tar_target(
    name = example_nowcast_dates_covid_ag,
    command = c(
      "2025-11-19", "2025-12-10", "2025-12-31", "2026-01-21", "2026-02-11"
    )
  ),
  tar_target(
    name = example_nowcast_dates_covid_spread,
    command = c(
      "2025-08-20", "2025-09-24", "2025-12-24", "2026-02-25", "2026-04-29"
    )
  ),
  # Minimum number of initial visits for a reference date to be used to
  # estimate the DPH multipliers, and for a reference week to be nowcasted
  # with them (to be confirmed with DPH)
  tar_target(
    name = dph_min_initial_count,
    command = 1
  ),
  tar_target(
    name = max_delay,
    command = 10
  ),
  tar_target(
    name = eval_horizon,
    command = 3
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
  ),
  tar_target(
    name = combined_data_file,
    command = file.path("input", "data", "indexed_daily_data.csv"),
    format = "file"
  ),
  tar_target(
    name = trend_threshold,
    command = 5
  )
)
