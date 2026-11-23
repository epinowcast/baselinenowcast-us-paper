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
  ),
  # Masking figure with baselinenowcast added (alternative to Fig. S18)
  tar_target(
    name = plot_masking_with_bnc,
    command = get_plot_masking(
      masking_data = masking_status_all,
      threshold = masking_threshold,
      include_bnc = TRUE,
      fig_file_name = "bar_masking_nowcast_vs_prelim_with_bnc"
    )
  ),
  # Version of Fig 3 with the BAR percent nowcasts and masking weeks as A
  tar_target(
    name = fig_state_nowcast_masking_pct,
    command = make_state_nowcast_comp_fig(
      plot_masking_nowcasts,
      bar_chart_scores_bar,
      bar_chart_scores_covid,
      bar_chart_scores_flu,
      bar_chart_scores_rsv,
      fig_file_name = "fig3nowcasts_maskingpct"
    )
  ),
  # Fig 3 mock-up: BAR same-week count nowcasts with a masking timeline strip
  tar_target(
    name = plot_masking_timeline,
    command = get_plot_masking_timeline(
      masking_status_all,
      date_limits = range(masking_status_all$reference_date) + c(-7, 7)
    )
  ),
  tar_target(
    name = fig_state_nowcast_masking_timeline,
    command = make_state_nowcast_comp_fig(
      nowcasts_by_horizon_0_bar_00plus +
        ggplot2::scale_x_date(
          date_breaks = "1 month", date_labels = "%b %Y",
          limits = range(masking_status_all$reference_date) + c(-7, 7)
        ) +
        ggplot2::labs(title = "Broad Acute Respiratory, same-week nowcasts"),
      bar_chart_scores_bar,
      bar_chart_scores_covid,
      bar_chart_scores_flu,
      bar_chart_scores_rsv,
      masking_timeline = plot_masking_timeline,
      fig_file_name = "fig3nowcasts_maskingtimeline"
    )
  )
)
