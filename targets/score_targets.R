score_targets <- list(
  tar_target(
    name = scores_su,
    command = state_nowcasts |>
      as_forecast_quantile(
        predicted = "quantile_value",
        observed = "final_count"
      ) |>
      score()
  )
)
