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
  # Add the real-time denominator used by DPH and the baselinenowcast
  # nowcasts expressed as a percent of ED visits with that denominator
  tar_target(
    name = pct_dat_realtime,
    command = add_realtime_denominator(
      pct_data = pct_dat,
      state_nowcasts = state_nowcasts_all
    )
  ),
  tar_target(
    name = pct_dat_realtime_file,
    command = {
      fp <- file.path(
        "output", "ma_masking",
        "dph_state_nowcasts_realtime_withmem_bnc.csv"
      )
      readr::write_csv(pct_dat_realtime, fp)
      fp
    },
    format = "file"
  ),
  # Consistency of the real-time denominator with the preliminary percent
  tar_target(
    name = realtime_denominator_check,
    command = check_realtime_denominator(pct_dat_realtime, pathogen_i = "bar")
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
    name = masking_status_all,
    command = add_masking_status(
      pct_data_clean = clean_pct_data(pct_dat_realtime, pathogen_i = "bar"),
      threshold = masking_threshold,
      n_weeks_off = 2
    )
  ),
  tar_target(
    name = masking_changes,
    command = get_masking_changes(masking_status_all)
  ),
  tar_target(
    name = plot_masking_nowcasts,
    command = get_plot_masking_nowcasts(
      masking_data = masking_status_all,
      threshold = masking_threshold,
      fig_file_name = "bar_pct_nowcasts_masking"
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
