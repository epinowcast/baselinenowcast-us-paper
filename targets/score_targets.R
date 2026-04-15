score_targets <- list(
  tar_target(
    name = scores_su_raw,
    command = state_nowcasts |>
      as_forecast_quantile(
        predicted = "quantile_value",
        observed = "final_count",
        forecast_unit = c(
          "pathogen",
          "pathogen_name",
          "reference_date",
          "nowcast_date",
          "model"
        )
      ) |>
      transform_forecasts(fun = log_shift, offset = 1) |>
      score()
  ),
  tar_target(
    name = coverage_state,
    command = state_nowcasts |>
      as_forecast_quantile(
        predicted = "quantile_value",
        observed = "final_count",
        forecast_unit = c(
          "pathogen",
          "pathogen_name",
          "reference_date",
          "nowcast_date",
          "model"
        )
      ) |> scoringutils::get_coverage(
        by = c("pathogen", "pathogen_name", "model")
      )
  ),
  tar_target(
    name = scores_su,
    command = scores_su_raw |>
      filter(scale == "log")
  ),
  tar_target(
    name = scores_su_natural,
    command = scores_su_raw |>
      filter(scale == "natural")
  ),
  tar_target(
    name = scores_ag_su_raw,
    command = age_group_nowcasts |>
      as_forecast_quantile(
        predicted = "quantile_value",
        observed = "final_count",
        forecast_unit = c(
          "pathogen",
          "age_group",
          "pathogen_name",
          "reference_date",
          "nowcast_date",
          "model"
        )
      ) |>
      transform_forecasts(fun = log_shift, offset = 1) |>
      score()
  ),
  tar_target(
    name = coverage_ag,
    command = age_group_nowcasts |>
      as_forecast_quantile(
        predicted = "quantile_value",
        observed = "final_count",
        forecast_unit = c(
          "pathogen",
          "age_group",
          "pathogen_name",
          "reference_date",
          "nowcast_date",
          "model"
        )
      ) |> scoringutils::get_coverage(
        by = c("pathogen", "pathogen_name", "model")
      )
  ),
  tar_target(
    name = scores_ag_su,
    command = scores_ag_su_raw |>
      filter(scale == "log")
  ),
  tar_target(
    name = scores_ag_su_natural,
    command = scores_ag_su_raw |>
      filter(scale == "natural")
  )
)
