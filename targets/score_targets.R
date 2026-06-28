score_targets <- list(
  tar_target(
    name = scores_su_raw,
    command = state_nowcasts_ma_method_comp |>
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
    name = coverage_state_raw,
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
    name = scores_ag_su_raw,
    command = age_group_nowcasts_ma_method_comp |>
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
    name = coverage_ag_raw,
    command = age_group_nowcasts_ma_method_comp |>
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

  ## Filter to the combinations you want---------------------------------
  ### Main figure scores------------------------------------------------
  tar_target(
    name = scores_su,
    command = scores_su_raw |>
      filter(
        scale == "log",
        model %in% c(
          "baselinenowcast",
          "baselinenowcast weekly",
          "baselinenowcast 7-day sum",
          "MADPH method"
        )
      )
  ),
  tar_target(
    name = scores_su_alt,
    command = scores_su_raw |>
      filter(
        scale == "log",
        model %in% c(
          "baselinenowcast",
          "baselinenowcast weekly",
          "baselinenowcast 7-day sum",
          "MADPH original"
        )
      )
  ),
  tar_target(
    name = scores_su_natural,
    command = scores_su_raw |>
      filter(
        scale == "natural",
        model %in% c(
          "baselinenowcast",
          "baselinenowcast weekly",
          "baselinenowcast 7-day sum",
          "MADPH method"
        )
      )
  ),
  tar_target(
    name = scores_su_all,
    command = scores_su_raw |>
      filter(scale == "log")
  ),
  tar_target(
    name = scores_su_natural_all,
    command = scores_su_raw |>
      filter(scale == "natural")
  ),
  tar_target(
    name = scores_ag_su,
    command = scores_ag_su_raw |>
      filter(
        scale == "log",
        model %in% c(
          "baselinenowcast base",
          "baselinenowcast strata sharing",
          "baselinenowcast base weekly",
          "baselinenowcast strata sharing weekly",
          "baselinenowcast 7-day sum",
          "baselinenowcast strata sharing 7-day sum",
          "MADPH method"
        )
      )
  ),
  tar_target(
    name = scores_ag_su_alt,
    command = scores_ag_su_raw |>
      filter(
        scale == "log",
        model %in% c(
          "baselinenowcast base",
          "baselinenowcast strata sharing",
          "baselinenowcast base weekly",
          "baselinenowcast strata sharing weekly",
          "baselinenowcast 7-day sum",
          "baselinenowcast strata sharing 7-day sum",
          "MADPH original"
        )
      )
  ),
  tar_target(
    name = scores_ag_su_natural,
    command = scores_ag_su_raw |>
      filter(
        scale == "natural",
        model %in% c(
          "baselinenowcast base",
          "baselinenowcast strata sharing",
          "baselinenowcast base weekly",
          "baselinenowcast strata sharing weekly",
          "baselinenowcast 7-day sum",
          "baselinenwocast strata sharing 7-day sum",
          "MADPH method"
        )
      )
  ),
  tar_target(
    name = scores_ag_su_all,
    command = scores_ag_su_raw |>
      filter(scale == "log")
  ),
  tar_target(
    name = scores_ag_su_natural_all,
    command = scores_ag_su_raw |>
      filter(scale == "natural")
  ),
  tar_target(
    name = coverage_ag,
    command = coverage_ag_raw |>
      filter(model %in% c(
        "baselinenowcast base",
        "baselinenowcast strata sharing",
        "baselinenowcast base weekly",
        "baselinenowcast strata sharing weekly",
        "baselinenowcast 7-day sum",
        "baselinenwocast strata sharing 7-day sum",
        "MADPH method"
      ))
  ),
  tar_target(
    name = coverage_state,
    command = coverage_state_raw |>
      filter(model %in% c(
        "baselinenowcast",
        "MADPH method",
        "baselinenowcast weekly",
        "baselinenowcast 7-day sum"
      ))
  )
)
