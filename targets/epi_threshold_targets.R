epi_threshold_targets <- list(
  tar_target(
    name = pct_dat,
    command = read_csv(ma_percent_data_fp)
  ),
  tar_target(
    name = plot_epi_threshold,
    command = get_plot_epi_threshold(
      pct_data = pct_dat,
      threshold = 0.117
    )
  )
)
