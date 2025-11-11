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
      season_to_plot = "2024-2025",
      by_age_group = FALSE
    )
  ),
  tar_target(
    name = case_count_by_ag_plot,
    command = get_cases_plot(clean_weekly_data,
      season_to_plot = "2024-2025",
      by_age_group = TRUE
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
  ),
  tar_target(
    name = delay_fig,
    command = make_delay_fig(delay_over_time,
      case_count_plot,
      violin_plot_delay,
      season_to_plot = "2024-2025",
      fig_file_name = "delay_all_pathogens_24_25"
    )
  ),
  # Make the same for 2023-2024
  tar_target(
    name = case_count_plot23,
    command = get_cases_plot(clean_weekly_data,
      season_to_plot = "2023-2024",
      by_age_group = FALSE
    )
  ),
  tar_target(
    name = case_count_by_ag_plot23,
    command = get_cases_plot(clean_weekly_data,
      season_to_plot = "2023-2024",
      by_age_group = TRUE
    )
  ),
  tar_target(
    name = delay_over_time23,
    command = get_delay_over_time_plot(clean_weekly_data,
      season_to_plot = "2023-2024"
    )
  ),
  tar_target(
    name = violin_plot_delay23,
    command = get_violin_plot_delay(clean_weekly_data,
      season_to_plot = "2023-2024"
    )
  ),
  tar_target(
    name = delay_fig23,
    command = make_delay_fig(delay_over_time23,
      case_count_plot23,
      violin_plot_delay23,
      season_to_plot = "2023-2024",
      fig_file_name = "delay_all_pathogens_23_24"
    )
  )
)
