delay_plot_targets <- list(
  # Plot the delay cdf without using the max_delay cut off
  tar_target(
    name = delay_cdf,
    command = get_delay_cdf_plot(weekly_data)
  ),
  # Everything else: use the data filtered to max delay and
  # with seasons indicated
  tar_target(
    name = case_count_plot,
    command = get_cases_plot(clean_weekly_data,
      season_to_plot = "2024-2025"
    )
  ),
  tar_target(
    name = delay_over_time,
    command = get_delay_over_time_plot(clean_weekly_data,
      season_to_plot = "2024-2025"
    )
  ),
  tar_target(
    name = violin_plot_delay,
    command = get_violin_plot_delay(clean_weekly_data,
      season_to_plot = "2024-2025"
    )
  )
)
