load_and_clean_data_targets <- list(
  tar_group_by(
    name = pathogens_grouped,
    command = pathogens,
    pathogen
  ),
  tar_target(
    name = raw_data,
    command = read_pathogen_data(
      pathogens_grouped,
      pathogen_data_fp
    ),
    pattern = map(pathogens_grouped)
  ),
  tar_target(
    name = weekly_data,
    command = get_weekly_data(raw_data)
  ),
  tar_target(
    name = clean_weekly_data,
    command = clean_data(
      weekly_data = weekly_data,
      max_delay = max_delay,
      nowcast_date_range = nowcast_date_range$nowcast_date,
      prev_season_date_range = prev_season_date_range$nowcast_date,
      delay_est_date_range = delay_est_training_date_range
    )
  )
)
