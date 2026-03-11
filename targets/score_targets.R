score_targets <- list(
  tar_target(
    name = scores_su,
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
      score()
  ),
  tar_target(
    name = scores_ag_su,
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
      score()
  )
)
