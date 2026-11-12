epi_threshold_targets <- list(
  tar_target(
    name = pct_dat,
    command = read_csv(ma_percent_data_fp)
  ),
  # BAR masking threshold (% of ED visits), set by DPH using the moving
  # epidemic method on final data
  tar_target(
    name = masking_threshold,
    command = 0.117
  ),
  tar_target(
    name = masking_status,
    command = add_masking_status(
      pct_data_clean = clean_pct_data(pct_dat, pathogen_i = "bar"),
      threshold = masking_threshold,
      n_weeks_off = 2
    )
  ),
  tar_target(
    name = plot_masking,
    command = get_plot_masking(
      masking_data = masking_status,
      threshold = masking_threshold,
      fig_file_name = "bar_masking_nowcast_vs_prelim"
    )
  )
)
