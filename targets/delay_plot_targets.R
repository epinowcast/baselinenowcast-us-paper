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
      season_to_plot = "2025-2026",
      by_age_group = FALSE
    )
  ),
  tar_target(
    name = case_count_by_season_plot,
    command = get_cases_by_season_plot(clean_weekly_data,
      season_to_plot = seasons
    )
  ),
  tar_target(
    name = case_count_by_ag_plot,
    command = get_cases_plot(clean_weekly_data,
      season_to_plot = "2025-2026",
      by_age_group = TRUE
    )
  ),
  tar_target(
    name = delay_over_time,
    command = get_delay_over_time_plot(clean_weekly_data,
      season_to_plot = "2025-2026"
    )
  ),
  tar_target(
    name = delay_over_time_free_y,
    command = get_delay_over_time_plot(clean_weekly_data,
      season_to_plot = "2025-2026",
      ylims = FALSE
    )
  ),
  tar_target(
    name = prop_visits_t,
    command = get_plot_prop_visits_t(clean_weekly_data,
      season_to_plot = "2025-2026"
    )
  ),
  tar_target(
    name = delay_over_time_mult_seasons,
    command = get_delay_t_by_season(clean_weekly_data,
      season_to_plot = seasons,
      ylims = FALSE
    )
  ),
  tar_target(
    name = violin_plot_delay,
    command = get_violin_plot_delay(clean_weekly_data,
      season_to_plot = "2025-2026"
    )
  ),
  tar_target(
    name = violin_plot_delay_free_y,
    command = get_violin_plot_delay(clean_weekly_data,
      season_to_plot = "2025-2026",
      ylims = FALSE
    )
  ),
  tar_target(
    name = violin_plot_prop_visits,
    command = get_violin_plot_prop_visits(clean_weekly_data,
      season_to_plot = "2025-2026"
    )
  ),


  # Delay fig main text ---------------------------------------------------
  tar_target(
    name = delay_fig,
    command = make_delay_fig(delay_over_time,
      case_count_plot,
      violin_plot_delay,
      season_to_plot = "2025-2026",
      fig_file_name = "delay_all_pathogens_25_26"
    )
  ),
  # Alternate delay fig: prop of cases reported ----------------------------
  tar_target(
    name = prop_visits_rep_fig,
    command = make_delay_fig(prop_visits_t,
      case_count_plot,
      violin_plot_prop_visits,
      season_to_plot = "2025-2026",
      fig_file_name = "prop_visits_all_pathogens_25_26"
    )
  ),
  tar_target(
    name = delay_fig_free_y,
    command = make_delay_fig(delay_over_time_free_y,
      case_count_plot,
      violin_plot_delay_free_y,
      season_to_plot = "2025-2026",
      fig_file_name = "delay_all_pathogens_25_26_free_y"
    )
  ),
  tar_target(
    name = compare_seasons_plot,
    command = make_comp_seasons_fig(delay_over_time_mult_seasons,
      case_count_by_season_plot,
      fig_file_name = "compare_seasons"
    )
  ),
  # Between-season variability plot (standalone)
  tar_target(
    name = delay_by_season_plot,
    command = get_plot_delay_by_season(clean_weekly_data,
      fig_file_name = "delay_between_season_variability"
    )
  ),
  # Within-season variability plot (normalized)
  tar_target(
    name = normalized_delay_within_season_plot,
    command = get_plot_norm_delay_in_season(clean_weekly_data)
  ),
  # Combined two-panel comparison plot
  tar_target(
    name = within_between_season_comparison_plot,
    command = make_within_between_season_fig(clean_weekly_data,
      fig_file_name = "delay_within_between_season_comparison"
    )
  )
)
