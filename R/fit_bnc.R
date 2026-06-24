#' Fit the baselinenowcast method to the state level data (all age groups)
#'
#' @param all_data Data.frame of incident cases by reference date and report
#'   date by week for multiple age groups and pathogens
#' @param nowcast_date Date to produce the nowcast for.
#' @param pathogen_i Pathogen to nowcast.
#' @param eval_horizon Number of weeks to evaluation and save the nowcast.
#' @param max_delay Maximum delay in weeks.
#' @param quantiles_for_scoring Vector of quantiles to score.
#' @param scale_factor Scale factor on maximum delay of the amount of data to
#'   be used to train the baselinenowcast model.
#' @param prop_delay Proportion of all training volume to use for delay
#'   estimation
#' @param draws Number of draws to save
#' @importFrom baselinenowcast as_reporting_triangle baselinenowcast
#' @importFrom lubridate weeks
#' @importFrom dplyr distinct pull
#'
#' @returns Quantiled dataframe of nowcasts with initial and final case counts
#'   alongside it.
fit_bnc_state <- function(all_data,
                          nowcast_date,
                          pathogen_i,
                          eval_horizon,
                          max_delay,
                          quantiles_for_scoring,
                          scale_factor = 3,
                          prop_delay = 0.5,
                          draws = 1000) {
  this_data <- all_data |>
    filter(
      end_of_week_report_date <= nowcast_date,
      pathogen == pathogen_i
    ) |>
    group_by(
      end_of_week_reference_date,
      end_of_week_report_date, delay
    ) |>
    summarise(count = sum(count, na.rm = TRUE)) |>
    filter(delay <= max_delay) |>
    ungroup()

  initial_data_summed <- this_data |>
    filter(end_of_week_reference_date >=
      max(end_of_week_reference_date) - weeks(eval_horizon)) |>
    group_by(end_of_week_reference_date) |>
    summarise(initial_count = sum(count))

  final_data_summed <- all_data |>
    filter(
      pathogen == pathogen_i,
      delay <= max_delay, # Might want to change this so that it is still
      # a rolling evaluation but its longer
      end_of_week_reference_date <= nowcast_date
    ) |>
    group_by(end_of_week_reference_date) |>
    summarise(final_count = sum(count, na.rm = TRUE)) |>
    ungroup() |>
    filter(end_of_week_reference_date >=
      max(end_of_week_reference_date) - weeks(eval_horizon))
  pathogen_name <- all_data |>
    filter(pathogen == pathogen_i) |>
    distinct(pathogen_name) |>
    pull(pathogen_name)


  # convert to a reporting triangle
  rep_tri <- as_reporting_triangle(this_data,
    max_delay = max_delay,
    delays_unit = "weeks",
    reference_date = "end_of_week_reference_date",
    report_date = "end_of_week_report_date"
  )

  # generate a nowcast using the default settings
  nowcast_df <- baselinenowcast(rep_tri,
    scale_factor = scale_factor,
    prop_delay = prop_delay,
    draws = draws
  ) |>
    filter(reference_date >= max(reference_date) - weeks(eval_horizon)) |>
    trajectories_to_quantiles(
      quantiles = quantiles_for_scoring,
      timepoint_cols = "reference_date",
      value_col = "pred_count"
    ) |>
    mutate(
      pathogen = pathogen_i,
      pathogen_name = pathogen_name,
      nowcast_date = nowcast_date,
      age_group = "00+",
      scale_factor = scale_factor,
      prop_delay = prop_delay,
      model_type = "base"
    ) |>
    left_join(initial_data_summed,
      by = c("reference_date" = "end_of_week_reference_date") # nolint
    ) |>
    left_join(final_data_summed,
      by = c("reference_date" = "end_of_week_reference_date") # nolint
    )
  return(nowcast_df)
}

#' Fit the baselinenowcast method to the state level data (all age groups)
#'   from daily data
#'
#' @param all_data Data.frame of incident cases by reference date and report
#'   date by day for multiple age groups and pathogens
#' @param nowcast_date Date to produce the nowcast for.
#' @param pathogen_i Pathogen to nowcast.
#' @param eval_horizon Number of weeks to evaluation and save the nowcast.
#' @param max_delay Maximum delay in weeks.
#' @param quantiles_for_scoring Vector of quantiles to score.
#' @param scale_factor Scale factor on maximum delay of the amount of data to
#'   be used to train the baselinenowcast model.
#' @param prop_delay Proportion of all training volume to use for delay
#'   estimation
#' @param draws Number of draws to save
#' @importFrom baselinenowcast as_reporting_triangle baselinenowcast
#' @importFrom lubridate weeks
#' @importFrom dplyr distinct pull
#'
#' @returns Quantiled dataframe of nowcasts with initial and final case counts
#'   alongside it.
fit_bnc_state_from_daily <- function(all_data,
                                     nowcast_date,
                                     pathogen_i,
                                     eval_horizon,
                                     max_delay,
                                     quantiles_for_scoring,
                                     scale_factor = 3,
                                     prop_delay = 0.5,
                                     draws = 1000) {
  # Convert delay from weekly to daily for noowcasting
  max_delay_daily <- 7 * max_delay
  this_data <- all_data |>
    filter(
      report_date <= nowcast_date,
      pathogen == pathogen_i
    ) |>
    group_by(
      reference_date,
      report_date, delay
    ) |>
    summarise(count = sum(count, na.rm = TRUE)) |>
    filter(delay <= max_delay_daily) |>
    ungroup()

  initial_data_summed <- this_data |>
    mutate(
      reference_date = ceiling_date(reference_date,
        unit = "week",
        week_start = 6,
        change_on_boundary = FALSE
      )
    ) |>
    group_by(reference_date) |>
    summarise(initial_count = sum(count))

  final_data_summed <- all_data |>
    filter(
      pathogen == pathogen_i,
      delay <= max_delay_daily, # Might want to change this so that it is still
      # a rolling evaluation but its longer
      reference_date <= nowcast_date
    ) |>
    mutate(
      reference_date = ceiling_date(reference_date,
        unit = "week",
        week_start = 6,
        change_on_boundary = FALSE
      )
    ) |>
    group_by(reference_date) |>
    summarise(final_count = sum(count, na.rm = TRUE)) |>
    ungroup()
  pathogen_name <- all_data |>
    filter(pathogen == pathogen_i) |>
    distinct(pathogen_name) |>
    pull(pathogen_name)


  # convert to a reporting triangle
  rep_tri <- as_reporting_triangle(this_data,
    max_delay = max_delay_daily,
    delays_unit = "days",
    reference_date = "reference_date",
    report_date = "report_date"
  )

  # generate a nowcast using the default settings
  nowcast_df <- baselinenowcast(rep_tri,
    scale_factor = scale_factor,
    prop_delay = prop_delay,
    draws = draws
  ) |>
    mutate(
      # Convert to weekly
      end_of_week_reference_date = ceiling_date(reference_date,
        unit = "week",
        week_start = 6,
        change_on_boundary = FALSE
      )
    ) |>
    group_by(end_of_week_reference_date, draw) |>
    summarise(
      pred_count = sum(pred_count)
    ) |>
    filter(end_of_week_reference_date < nowcast_date) |>
    rename(reference_date = end_of_week_reference_date) |>
    trajectories_to_quantiles(
      quantiles = quantiles_for_scoring,
      timepoint_cols = "reference_date",
      value_col = "pred_count"
    ) |>
    mutate(
      pathogen = pathogen_i,
      pathogen_name = pathogen_name,
      nowcast_date = nowcast_date,
      age_group = "00+",
      scale_factor = scale_factor,
      prop_delay = prop_delay,
      model_type = "base"
    ) |>
    left_join(initial_data_summed,
      by = "reference_date"
    ) |>
    left_join(final_data_summed,
      by = "reference_date"
    ) |>
    filter(reference_date >= max(reference_date) - weeks(eval_horizon))

  return(nowcast_df)
}


#' Fit the baselinenowcast method to the state level data (all age groups)
#'   from daily data, usign the 7day rolling sum(right aligned) of incidence
#'
#' @param all_data Data.frame of incident cases by reference date and report
#'   date by day for multiple age groups and pathogens
#' @param nowcast_date Date to produce the nowcast for.
#' @param pathogen_i Pathogen to nowcast.
#' @param eval_horizon Number of weeks to evaluation and save the nowcast.
#' @param max_delay Maximum delay in weeks.
#' @param quantiles_for_scoring Vector of quantiles to score.
#' @param scale_factor Scale factor on maximum delay of the amount of data to
#'   be used to train the baselinenowcast model.
#' @param prop_delay Proportion of all training volume to use for delay
#'   estimation
#' @param draws Number of draws to save
#' @importFrom baselinenowcast as_reporting_triangle baselinenowcast
#' @importFrom lubridate weeks
#' @importFrom dplyr distinct pull
#'
#' @returns Quantiled dataframe of nowcasts with initial and final case counts
#'   alongside it.
fit_bnc_state_7d_sum <- function(all_data,
                                 nowcast_date,
                                 pathogen_i,
                                 eval_horizon,
                                 max_delay,
                                 quantiles_for_scoring,
                                 scale_factor = 3,
                                 prop_delay = 0.5,
                                 draws = 1000) {
  # Convert delay from weekly to daily for noowcasting
  max_delay_daily <- 7 * max_delay
  this_data <- all_data |>
    filter(
      report_date <= nowcast_date,
      pathogen == pathogen_i
    ) |>
    group_by(
      reference_date,
      report_date, delay
    ) |>
    summarise(count = sum(count, na.rm = TRUE)) |>
    filter(delay <= max_delay_daily) |>
    ungroup()

  initial_data_summed <- this_data |>
    mutate(
      reference_date = ceiling_date(reference_date,
        unit = "week",
        week_start = 6,
        change_on_boundary = FALSE
      )
    ) |>
    group_by(reference_date) |>
    summarise(initial_count = sum(count, na.rm = TRUE)) |>
    ungroup()

  final_data_summed <- all_data |>
    filter(
      pathogen == pathogen_i,
      delay <= max_delay_daily, # Might want to change this so that it is still
      # a rolling evaluation but its longer
      reference_date <= nowcast_date
    ) |>
    mutate(
      reference_date = ceiling_date(reference_date,
        unit = "week",
        week_start = 6,
        change_on_boundary = FALSE
      )
    ) |>
    group_by(reference_date) |>
    summarise(final_count = sum(count, na.rm = TRUE)) |>
    ungroup()
  pathogen_name <- all_data |>
    filter(pathogen == pathogen_i) |>
    distinct(pathogen_name) |>
    pull(pathogen_name)


  # convert to a reporting triangle
  rep_tri <- as_reporting_triangle(this_data,
    delays_unit = "days",
    reference_date = "reference_date",
    report_date = "report_date"
  ) |> truncate_to_delay(max_delay = max_delay_daily)

  # generate a nowcast using the default settings
  nowcast_df <- baselinenowcast(rep_tri,
    scale_factor = scale_factor,
    prop_delay = prop_delay,
    draws = draws,
    ref_time_aggregator = function(x) zoo::rollsum(x, k = 7, align = "right")
  )
  nowcasts <- nowcast_df |>
    mutate(
      # Convert to weekly by filtering to end of week reference date
      end_of_week_reference_date = ceiling_date(reference_date,
        unit = "week",
        week_start = 6,
        change_on_boundary = FALSE
      )
    ) |>
    filter(reference_date == end_of_week_reference_date) |>
    filter(end_of_week_reference_date < nowcast_date) |>
    select(-end_of_week_reference_date) |>
    trajectories_to_quantiles(
      quantiles = quantiles_for_scoring,
      timepoint_cols = "reference_date",
      value_col = "pred_count"
    ) |>
    mutate(
      pathogen = pathogen_i,
      pathogen_name = pathogen_name,
      nowcast_date = nowcast_date,
      age_group = "00+",
      scale_factor = scale_factor,
      prop_delay = prop_delay,
      model_type = "base"
    ) |>
    left_join(initial_data_summed,
      by = "reference_date"
    ) |>
    left_join(final_data_summed,
      by = "reference_date"
    ) |>
    filter(reference_date >= max(reference_date) - weeks(eval_horizon))

  return(nowcasts)
}

#' Fit the baselinenowcast method to age-groups
#'
#' @param all_data Data.frame of incident cases by reference date and report
#'   date by week for multiple age groups and pathogens
#' @param nowcast_date Date to produce the nowcast for.
#' @param pathogen_i Pathogen to nowcast.
#' @param eval_horizon Number of weeks to evaluation and save the nowcast.
#' @param max_delay Maximum delay in weeks.
#' @param quantiles_for_scoring Vector of quantiles to score.
#' @param scale_factor Scale factor on maximum delay of the amount of data to
#'   be used to train the baselinenowcast model.
#' @param prop_delay Proportion of all training volume to use for delay
#'   estimation
#' @param draws Number of draws to save
#' @importFrom baselinenowcast as_reporting_triangle baselinenowcast
#'   get_delays_from_dates
#' @importFrom lubridate weeks
#' @importFrom dplyr distinct pull
#'
#' @returns Quantiled dataframe of nowcasts with initial and final case counts
#'   alongside it.
fit_bnc_age_groups <- function(all_data,
                               nowcast_date,
                               pathogen_i,
                               model,
                               eval_horizon,
                               max_delay,
                               quantiles_for_scoring,
                               scale_factor = 3,
                               prop_delay = 0.5,
                               draws = 1000) {
  this_data <- all_data |>
    filter(
      end_of_week_report_date <= nowcast_date,
      pathogen == pathogen_i
    ) |>
    group_by(
      end_of_week_reference_date,
      end_of_week_report_date,
      age_group,
      delay
    ) |>
    summarise(count = sum(count, na.rm = TRUE)) |>
    filter(delay <= max_delay) |>
    ungroup() |>
    rename(
      reference_date = end_of_week_reference_date,
      report_date = end_of_week_report_date
    )

  initial_data_summed <- this_data |>
    filter(reference_date >=
      max(reference_date) - weeks(eval_horizon)) |>
    group_by(
      reference_date,
      age_group
    ) |>
    summarise(initial_count = sum(count, na.rm = TRUE))

  final_data_summed <- all_data |>
    rename(reference_date = end_of_week_reference_date) |>
    filter(
      pathogen == pathogen_i,
      delay <= max_delay, # Might want to change this so that it is still
      # a rolling evaluation but its longer
      reference_date <= nowcast_date
    ) |>
    group_by(
      reference_date,
      age_group
    ) |>
    summarise(final_count = sum(count, na.rm = TRUE)) |>
    ungroup() |>
    filter(reference_date >=
      max(reference_date) - weeks(eval_horizon))
  pathogen_name <- all_data |>
    filter(pathogen == pathogen_i) |>
    distinct(pathogen_name) |>
    pull(pathogen_name)

  # Get unique values
  reference_dates <- unique(this_data$reference_date)
  age_groups <- unique(this_data$age_group)

  # Create all combinations
  all_combos <- expand.grid(
    reference_date = reference_dates,
    age_group = age_groups,
    delay = 0:max_delay,
    stringsAsFactors = FALSE
  )

  # Merge with actual data
  all_combos <- merge(
    all_combos,
    this_data,
    by = c("reference_date", "delay", "age_group"),
    all.x = TRUE
  )

  # Fill in missing counts with 0
  all_combos$count[is.na(all_combos$count)] <- 0

  # For missing rows, calculate the maximum observable delay
  # based on the latest report date in the dataset
  max_report_date <- max(this_data$report_date)

  all_combos$report_date <- all_combos$reference_date +
    all_combos$delay * 7 # assuming weekly data

  # You can then filter to only include combinations where
  # report_date <= max_report_date to avoid impossible future combinations
  all_combos <- all_combos[all_combos$report_date <= max_report_date, ]

  # Generate a nowcast using the baselinenowcast.data.frame method (will need
  # to change to as_rep_tri_df |> baselinenowcast when we update the package)
  if (model == "baselinenowcast base") {
    nowcast_df <- baselinenowcast(all_combos,
      strata_cols = "age_group",
      delays_unit = "weeks",
      scale_factor = scale_factor,
      prop_delay = prop_delay,
      draws = draws
    )
  } else if (model == "baselinenowcast strata sharing") {
    nowcast_df <- baselinenowcast(all_combos,
      strata_cols = "age_group",
      max_delay = max_delay,
      delays_unit = "weeks",
      strata_sharing = c("delay", "uncertainty"),
      scale_factor = scale_factor,
      prop_delay = prop_delay,
      draws = draws
    )
  }
  nowcasts_clean <- nowcast_df |>
    filter(reference_date >= max(reference_date) - weeks(eval_horizon)) |>
    trajectories_to_quantiles(
      quantiles = quantiles_for_scoring,
      timepoint_cols = "reference_date",
      value_col = "pred_count",
      id_cols = "age_group"
    ) |>
    mutate(
      pathogen = pathogen_i,
      model = model,
      pathogen_name = pathogen_name,
      nowcast_date = nowcast_date,
      scale_factor = scale_factor,
      prop_delay = prop_delay
    ) |>
    left_join(initial_data_summed,
      by = c("reference_date", "age_group")
    ) |>
    left_join(final_data_summed,
      by = c("reference_date", "age_group")
    )
  return(nowcasts_clean)
}


#' Fit the baselinenowcast method to age-groups
#'
#' @param all_data Data.frame of incident cases by reference date and report
#'   date by day for multiple age groups and pathogens
#' @param nowcast_date Date to produce the nowcast for.
#' @param pathogen_i Pathogen to nowcast.
#' @param model Character string indicating the model
#' @param eval_horizon Number of weeks to evaluation and save the nowcast.
#' @param max_delay Maximum delay in weeks.
#' @param quantiles_for_scoring Vector of quantiles to score.
#' @param scale_factor Scale factor on maximum delay of the amount of data to
#'   be used to train the baselinenowcast model.
#' @param prop_delay Proportion of all training volume to use for delay
#'   estimation
#' @param draws Number of draws to save
#' @param max_trim_attempts Number of times to trim data to prevent all 0s
#' @importFrom baselinenowcast as_reporting_triangle baselinenowcast
#'   get_delays_from_dates
#' @importFrom lubridate weeks
#' @importFrom dplyr distinct pull
#' @autoglobal
#' @returns Quantiled dataframe of nowcasts with initial and final case counts
#'   alongside it.
fit_bnc_age_groups_from_daily <- function(all_data,
                                          nowcast_date,
                                          pathogen_i,
                                          model,
                                          eval_horizon,
                                          max_delay,
                                          quantiles_for_scoring,
                                          scale_factor = 3,
                                          prop_delay = 0.5,
                                          draws = 1000,
                                          max_trim_attempts = 6) {
  # Convert delay from weekly to daily for noowcasting
  max_delay_daily <- 7 * max_delay
  this_data <- all_data |>
    filter(
      report_date <= nowcast_date,
      pathogen == pathogen_i
    ) |>
    group_by(
      reference_date,
      report_date,
      age_group,
      delay
    ) |>
    summarise(count = sum(count, na.rm = TRUE)) |>
    filter(delay <= max_delay_daily) |>
    ungroup()

  initial_data_summed <- this_data |>
    mutate(
      end_of_week_reference_date = ceiling_date(reference_date,
        unit = "week",
        week_start = 6,
        change_on_boundary = FALSE
      )
    ) |>
    group_by(
      end_of_week_reference_date,
      age_group
    ) |>
    summarise(initial_count = sum(count, na.rm = TRUE))

  final_data_summed <- all_data |>
    mutate(
      end_of_week_reference_date = ceiling_date(reference_date,
        unit = "week",
        week_start = 6,
        change_on_boundary = FALSE
      )
    ) |>
    filter(
      pathogen == pathogen_i,
      delay <= max_delay_daily, # Might want to change this so that it is still
      # a rolling evaluation but its longer
      reference_date <= nowcast_date
    ) |>
    group_by(
      end_of_week_reference_date,
      age_group
    ) |>
    summarise(final_count = sum(count, na.rm = TRUE)) |>
    ungroup()

  pathogen_name <- all_data |>
    filter(pathogen == pathogen_i) |>
    distinct(pathogen_name) |>
    pull(pathogen_name)

  # Get unique values
  reference_dates <- unique(this_data$reference_date)
  age_groups <- unique(this_data$age_group)

  # Create all combinations
  all_combos <- expand.grid(
    reference_date = reference_dates,
    age_group = age_groups,
    delay = 0:max_delay_daily,
    stringsAsFactors = FALSE
  )

  # Merge with actual data
  all_combos <- left_join(all_combos,
    this_data |> select(!report_date), # nolint
    by = c("reference_date", "delay", "age_group")
  )

  # Fill in missing counts with 0
  all_combos$count[is.na(all_combos$count)] <- 0

  # For missing rows, calculate the maximum observable delay
  # based on the latest report date in the dataset
  max_report_date <- max(this_data$report_date)

  all_combos$report_date <- all_combos$reference_date +
    all_combos$delay

  # You can then filter to only include combinations where
  # report_date <= max_report_date to avoid impossible future combinations
  all_combos <- all_combos[all_combos$report_date <= max_report_date, ]


  if (model == "baselinenowcast base") {
    nowcast_df <- baselinenowcast(all_combos,
      strata_cols = "age_group",
      delays_unit = "days",
      max_delay = max_delay_daily,
      scale_factor = scale_factor,
      prop_delay = prop_delay,
      draws = draws
    )
  } else if (model == "baselinenowcast strata sharing") {
    nowcast_df <- baselinenowcast(all_combos,
      strata_cols = "age_group",
      max_delay = max_delay_daily,
      delays_unit = "days",
      strata_sharing = c("delay", "uncertainty"),
      scale_factor = scale_factor,
      prop_delay = prop_delay,
      draws = draws
    )
  }

  nowcasts_clean <- nowcast_df |>
    mutate(
      # Convert to weekly
      end_of_week_reference_date = ceiling_date(reference_date,
        unit = "week",
        week_start = 6,
        change_on_boundary = FALSE
      )
    ) |>
    group_by(end_of_week_reference_date, draw, age_group) |>
    summarise(
      pred_count = sum(pred_count)
    ) |>
    ungroup() |>
    # Exclude the nowcasts made in the partial week
    filter(end_of_week_reference_date < nowcast_date) |>
    rename(reference_date = end_of_week_reference_date) |>
    filter(reference_date >= max(reference_date) - weeks(eval_horizon)) |>
    trajectories_to_quantiles(
      quantiles = quantiles_for_scoring,
      timepoint_cols = "reference_date",
      value_col = "pred_count",
      id_cols = "age_group"
    ) |>
    # nolint start
    left_join(initial_data_summed,
      by = c(
        "reference_date" = "end_of_week_reference_date",
        "age_group"
      )
    ) |>
    left_join(final_data_summed,
      by = c(
        "reference_date" = "end_of_week_reference_date",
        "age_group"
      )
    ) |>
    # nolint end
    mutate(
      pathogen = pathogen_i,
      model = model,
      pathogen_name = pathogen_name,
      nowcast_date = nowcast_date,
      scale_factor = scale_factor,
      prop_delay = prop_delay
    )
  return(nowcasts_clean)
}

#' Fit the baselinenowcast method to age-groups using the 7-day rolling sum
#'
#' @param all_data Data.frame of incident cases by reference date and report
#'   date by day for multiple age groups and pathogens
#' @param nowcast_date Date to produce the nowcast for.
#' @param pathogen_i Pathogen to nowcast.
#' @param model Character string indicating the model
#' @param eval_horizon Number of weeks to evaluation and save the nowcast.
#' @param max_delay Maximum delay in weeks.
#' @param quantiles_for_scoring Vector of quantiles to score.
#' @param scale_factor Scale factor on maximum delay of the amount of data to
#'   be used to train the baselinenowcast model.
#' @param prop_delay Proportion of all training volume to use for delay
#'   estimation
#' @param draws Number of draws to save
#' @param max_trim_attempts Number of times to trim data to prevent all 0s
#' @importFrom baselinenowcast as_reporting_triangle baselinenowcast
#'   get_delays_from_dates
#' @importFrom lubridate weeks
#' @importFrom dplyr distinct pull
#' @autoglobal
#' @returns Quantiled dataframe of nowcasts with initial and final case counts
#'   alongside it.
fit_bnc_age_groups_7d_sum <- function(all_data,
                                      nowcast_date,
                                      pathogen_i,
                                      model,
                                      eval_horizon,
                                      max_delay,
                                      quantiles_for_scoring,
                                      scale_factor = 3,
                                      prop_delay = 0.5,
                                      draws = 1000,
                                      max_trim_attempts = 6) {
  # Convert delay from weekly to daily for noowcasting
  max_delay_daily <- 7 * max_delay
  this_data <- all_data |>
    filter(
      report_date <= nowcast_date,
      pathogen == pathogen_i
    ) |>
    group_by(
      reference_date,
      report_date,
      age_group,
      delay
    ) |>
    summarise(count = sum(count, na.rm = TRUE)) |>
    filter(delay <= max_delay_daily) |>
    ungroup()

  initial_data_summed <- this_data |>
    mutate(
      end_of_week_reference_date = ceiling_date(reference_date,
        unit = "week",
        week_start = 6,
        change_on_boundary = FALSE
      )
    ) |>
    group_by(
      end_of_week_reference_date,
      age_group
    ) |>
    summarise(initial_count = sum(count, na.rm = TRUE)) |>
    ungroup()

  final_data_summed <- all_data |>
    mutate(
      end_of_week_reference_date = ceiling_date(reference_date,
        unit = "week",
        week_start = 6,
        change_on_boundary = FALSE
      )
    ) |>
    filter(
      pathogen == pathogen_i,
      delay <= max_delay_daily, # Might want to change this so that it is still
      # a rolling evaluation but its longer
      reference_date <= nowcast_date
    ) |>
    group_by(
      end_of_week_reference_date,
      age_group
    ) |>
    summarise(final_count = sum(count, na.rm = TRUE)) |>
    ungroup()

  pathogen_name <- all_data |>
    filter(pathogen == pathogen_i) |>
    distinct(pathogen_name) |>
    pull(pathogen_name)

  # Get unique values
  reference_dates <- unique(this_data$reference_date)
  age_groups <- unique(this_data$age_group)

  # Create all combinations
  all_combos <- expand.grid(
    reference_date = reference_dates,
    age_group = age_groups,
    delay = 0:max_delay_daily,
    stringsAsFactors = FALSE
  )

  # Merge with actual data
  all_combos <- left_join(all_combos,
    this_data |> select(!report_date), # nolint
    by = c("reference_date", "delay", "age_group")
  )

  # Fill in missing counts with 0
  all_combos$count[is.na(all_combos$count)] <- 0

  # For missing rows, calculate the maximum observable delay
  # based on the latest report date in the dataset
  max_report_date <- max(this_data$report_date)

  all_combos$report_date <- all_combos$reference_date +
    all_combos$delay

  # You can then filter to only include combinations where
  # report_date <= max_report_date to avoid impossible future combinations
  all_combos <- all_combos[all_combos$report_date <= max_report_date, ]


  if (model == "baselinenowcast base") {
    nowcast_df <- baselinenowcast(all_combos,
      strata_cols = "age_group",
      delays_unit = "days",
      max_delay = max_delay_daily,
      scale_factor = scale_factor,
      prop_delay = prop_delay,
      draws = draws,
      ref_time_aggregator = function(x) zoo::rollsum(x, k = 7, align = "right")
    )

    nowcast_df1 <- baselinenowcast(all_combos,
      strata_cols = "age_group",
      delays_unit = "days",
      max_delay = max_delay_daily,
      scale_factor = scale_factor,
      prop_delay = prop_delay,
      draws = draws
    )
  } else if (model == "baselinenowcast strata sharing") {
    nowcast_df <- baselinenowcast(all_combos,
      strata_cols = "age_group",
      max_delay = max_delay_daily,
      delays_unit = "days",
      strata_sharing = c("delay", "uncertainty"),
      scale_factor = scale_factor,
      prop_delay = prop_delay,
      draws = draws,
      ref_time_aggregator = function(x) zoo::rollsum(x, k = 7, align = "right")
    )
  }

  nowcasts <- nowcast_df |>
    mutate(
      # Convert to weekly by filtering to only Saturdays
      end_of_week_reference_date = ceiling_date(reference_date,
        unit = "week",
        week_start = 6,
        change_on_boundary = FALSE
      )
    ) |>
    filter(reference_date == end_of_week_reference_date) |>
    filter(end_of_week_reference_date < nowcast_date) |>
    select(-end_of_week_reference_date) |>
    ungroup() |>
    filter(reference_date >= max(reference_date) - weeks(eval_horizon)) |>
    trajectories_to_quantiles(
      quantiles = quantiles_for_scoring,
      timepoint_cols = "reference_date",
      value_col = "pred_count",
      id_cols = "age_group"
    ) |>
    # nolint start
    left_join(initial_data_summed,
      by = c(
        "reference_date" = "end_of_week_reference_date",
        "age_group"
      )
    ) |>
    left_join(final_data_summed,
      by = c(
        "reference_date" = "end_of_week_reference_date",
        "age_group"
      )
    ) |>
    # nolint end
    mutate(
      pathogen = pathogen_i,
      model = model,
      pathogen_name = pathogen_name,
      nowcast_date = nowcast_date,
      scale_factor = scale_factor,
      prop_delay = prop_delay
    )
  return(nowcasts)
}

#' Derive multipliers using MADPH methods but within this codebase, using
#' their original implementation
#'
#' @param all_data Dataframe of daily cases by reference and report date
#'   stratified by age group
#' @param max_delay Integer indicating maximum delay in weeks
#' @param source Character string indicating where data is from and its method
#' @param this_age_group Selected age group
#' @param nowcast_wday Weekday integer of the date of the nowcast, e.g. Sun = 1,
#'   Mon = 2, Tues = 3, Wed = 4, Thurs = 5, Fri = 6, Sat = 7. Use to define the
#'   multiplier for 0 weeks ago, which then effects all subsequent multipliers.
#'   The 0 weeks ago multiplier is the multiplier for the partial week.
#'
#' @returns dataframe of median and 95% CI for the pmf at each delay (in weeks)
#' @autoglobal
get_mult_from_daily_data_orig <- function(all_data,
                                          max_delay,
                                          source,
                                          this_age_group = "00+",
                                          nowcast_wday = 4) {
  if (this_age_group == "00+") {
    all_data <- all_data |>
      group_by(
        reference_date, report_date,
        delay, pathogen
      ) |>
      summarise(
        count = sum(count),
        age_group = "00+"
      ) |>
      ungroup()
  } else {
    all_data <- filter(all_data, age_group == this_age_group)
  }

  multipliers <- all_data |>
    # The mistake in the code we observed was that an additional day was being
    # counted in the 0 weeks ago so we want to account for this e.g.
    # for a Wednesday nowcast date, delays from 0 to 4 will be counted as
    # 0 weeks ago, delays from 5-12 will be 1 week ago, etc.
    mutate(weeks_ago = pmax(0, floor(
      (as.numeric(report_date - reference_date) + (6 - nowcast_wday)) / 7
    ))) |>
    group_by(reference_date, pathogen) |> # group by day of arrival
    # sort earliest update first
    arrange(reference_date, report_date, pathogen) |>
    # cumulative received by time on that day
    mutate(
      cumreceived = cumsum(count),
      totalreceived = max(cumreceived),
      # maximum of those aka sum for the day
      percentreceived = (cumreceived / totalreceived)
    ) |>
    # percent of daily total received at each update
    group_by(reference_date, weeks_ago, pathogen) |>
    filter(percentreceived == max(percentreceived)) |>
    # for each combo date+weeks from visit, find the max cumulative sum
    group_by(weeks_ago, pathogen) |>
    summarise(
      "2.5%" = quantile(percentreceived, probs = 0.025),
      median = quantile(percentreceived, probs = 0.5),
      "97.5%" = quantile(percentreceived, probs = 0.975)
    ) |>
    mutate(
      source = source,
      delay = weeks_ago,
      age_group = this_age_group
    ) |>
    select(-weeks_ago)

  return(multipliers)
}

#' Derive multipliers using MADPH methods but within this codebase, using
#' their original implementation
#'
#' @param all_data Dataframe of daily cases by reference and report date
#'   stratified by age group
#' @param max_delay Integer indicating maximum delay in weeks
#' @param source Character string indicating where data is from and its method
#' @param this_age_group Selected age group
#' @param nowcast_wday Weekday integer of the date of the nowcast, e.g. Sun = 1,
#'   Mon = 2, Tues = 3, Wed = 4, Thurs = 5, Fri = 6, Sat = 7. Use to define the
#'   multiplier for 0 weeks ago, which then effects all subsequent multipliers.
#'   The 0 weeks ago multiplier is the multiplier for the partial week.
#'
#' @returns dataframe of median and 95% CI for the pmf at each delay (in weeks)
#' @autoglobal
get_mult_from_daily_data_rev <- function(all_data,
                                         max_delay,
                                         source,
                                         this_age_group = "00+",
                                         nowcast_wday = 4) {
  if (this_age_group == "00+") {
    all_data <- all_data |>
      group_by(
        reference_date, report_date,
        delay, pathogen
      ) |>
      summarise(
        count = sum(count),
        age_group = "00+"
      ) |>
      ungroup()
  } else {
    all_data <- filter(all_data, age_group == this_age_group)
  }

  multipliers <- all_data |>
    # Create a variable for the weeks ago column based on the weekday you are
    # producing a nowcast. E.g. if a nowcast is produced on a Wednesday, we
    # want delays of 0-3 days to be counted as 0 weeks ago as these are cases
    # for the current week, and we want the 1 week ago multiplier to be for
    # delays from 4-11 days.
    mutate(weeks_ago = floor(
      (as.numeric(report_date - reference_date) + (7 - nowcast_wday)) / 7
    )) |>
    group_by(reference_date, pathogen) |> # group by day of arrival
    # sort earliest update first
    arrange(reference_date, report_date, pathogen) |>
    # cumulative received by time on that day
    mutate(
      cumreceived = cumsum(count),
      totalreceived = max(cumreceived),
      # maximum of those aka sum for the day
      percentreceived = (cumreceived / totalreceived)
    ) |>
    # percent of daily total received at each update
    group_by(reference_date, weeks_ago, pathogen) |>
    filter(percentreceived == max(percentreceived)) |>
    # for each combo date+weeks from visit, find the max cumulative sum
    group_by(weeks_ago, pathogen) |>
    summarise(
      "2.5%" = quantile(percentreceived, probs = 0.025),
      median = quantile(percentreceived, probs = 0.5),
      "97.5%" = quantile(percentreceived, probs = 0.975)
    ) |>
    mutate(
      source = source,
      delay = weeks_ago,
      age_group = this_age_group
    ) |>
    select(-weeks_ago)

  return(multipliers)
}

#' Derive multipliers using MADPH methods but within this codebase
#'
#' @param all_data Dataframe of weekly cases by reference and report date
#'   stratified by age group
#' @param source Character string indicating where data is from and its method
#' @param this_age_group Selected age group
#'
#' @returns dataframe of median and 95% CI for the pmf at each delay (in weeks)
#' @autoglobal
get_multipliers <- function(all_data,
                            source,
                            this_age_group = "00+") {
  if (this_age_group == "00+") {
    all_data <- all_data |>
      group_by(
        end_of_week_reference_date, end_of_week_report_date,
        delay, pathogen
      ) |>
      summarise(
        count = sum(count),
        age_group = "00+"
      )
  }


  multipliers <- all_data |>
    filter(age_group == this_age_group) |>
    group_by(end_of_week_reference_date, pathogen) |> # group by day of arrival
    arrange(end_of_week_report_date) |> # sort earliest update first
    # cumulative received by time on that day
    mutate(
      cumreceived = cumsum(count),
      totalreceived = max(cumreceived),
      # maximum of those aka sum for the day
      percentreceived = (cumreceived / totalreceived)
    ) |>
    # percent of daily total received at each update
    group_by(end_of_week_reference_date, delay, pathogen) |>
    filter(percentreceived == max(percentreceived)) |>
    # for each combo date+weeks from visit, find the max cumulative sum
    group_by(delay, pathogen) |>
    summarise(
      "2.5%" = quantile(percentreceived, probs = 0.025),
      median = quantile(percentreceived, probs = 0.5),
      "97.5%" = quantile(percentreceived, probs = 0.975)
    ) |>
    mutate(
      source = source,
      age_group = this_age_group
    )

  return(multipliers)
}


#' Implement the MADPH method
#'
#' @param multipliers MADPH multipliers estimated from 2023 data
#' @param age_group Character string indicating age group to nowcast
#' @param all_data Clean weekly data for all age groups
#' @param nowcast_date Date of the nowcast
#' @param pathogen_i Character string indicating pathogen to nowcast
#' @param eval_horizon Integer indicating number of weeks to evaluate
#' @param max_delay Maximum delay
#' @param model_name Character string indicating name of the model
#' @importFrom tidyselect starts_with
#' @returns Nowcast dataframe
implement_madph_method <- function(multipliers,
                                   age_group,
                                   all_data,
                                   nowcast_date,
                                   pathogen_i,
                                   eval_horizon,
                                   max_delay,
                                   model_name) {
  if (age_group == "00+") {
    all_data <- mutate(all_data,
      age_group = "00+"
    )
  }
  this_data <- all_data |>
    filter(
      end_of_week_report_date <= nowcast_date,
      pathogen == pathogen_i
    ) |>
    group_by(
      end_of_week_reference_date,
      end_of_week_report_date,
      age_group,
      delay
    ) |>
    summarise(count = sum(count, na.rm = TRUE)) |>
    filter(delay <= max_delay) |>
    ungroup() |>
    rename(
      reference_date = end_of_week_reference_date,
      report_date = end_of_week_report_date
    )

  initial_data_summed <- this_data |>
    filter(reference_date >=
      max(reference_date) - weeks(eval_horizon)) |>
    group_by(
      reference_date,
      age_group
    ) |>
    summarise(initial_count = sum(count, na.rm = TRUE))

  final_data_summed <- all_data |>
    rename(reference_date = end_of_week_reference_date) |>
    filter(
      pathogen == pathogen_i,
      delay <= max_delay, # Might want to change this so that it is still
      # a rolling evaluation but its longer
      reference_date <= nowcast_date
    ) |>
    group_by(
      reference_date,
      age_group
    ) |>
    summarise(final_count = sum(count, na.rm = TRUE)) |>
    ungroup() |>
    filter(reference_date >=
      max(reference_date) - weeks(eval_horizon))
  pathogen_name <- all_data |>
    filter(pathogen == pathogen_i) |>
    distinct(pathogen_name) |>
    pull(pathogen_name)

  multipliers <- filter(multipliers, pathogen == pathogen_i)

  nowcast_df <- this_data |>
    group_by(reference_date, age_group) |>
    summarise(
      count = sum(count),
      delay = max(delay)
    ) |>
    left_join(multipliers, by = c("delay", "age_group")) |>
    # Nowcasting step: divide by the completeness multiplier!
    # nolint start
    mutate(
      `est_final_count_0.5` = count / median,
      `est_final_count_0.025` = count / `97.5%`,
      `est_final_count_0.975` = count / `2.5%`
    ) |>
    # nolint end
    ungroup() |>
    filter(reference_date >= max(reference_date) - weeks(eval_horizon)) |>
    pivot_longer(
      cols = starts_with("est_final_count_"),
      names_to = "quantile_level",
      names_prefix = "est_final_count_",
      values_to = "quantile_value"
    ) |>
    mutate(
      quantile_level = as.numeric(quantile_level),
      pathogen_name = pathogen_name,
      nowcast_date = nowcast_date,
      scale_factor = NA,
      prop_delay = NA,
      model_type = "dph our implementation",
      model = model_name
    ) |>
    left_join(initial_data_summed,
      by = c("reference_date", "age_group")
    ) |>
    left_join(final_data_summed,
      by = c("reference_date", "age_group")
    ) |>
    select(
      reference_date, quantile_value, quantile_level,
      pathogen, pathogen_name, nowcast_date,
      age_group, scale_factor, prop_delay, model_type,
      final_count, initial_count, model
    )

  return(nowcast_df)
}

#' Implement the MADPH method from daily data, using recent updates
#'
#' @param multipliers MADPH multipliers estimated from 2023 data
#' @param age_group Character string indicating age group to nowcast
#' @param all_data Clean daily data for all age groups
#' @param nowcast_date Date of the nowcast
#' @param pathogen_i Character string indicating pathogen to nowcast
#' @param eval_horizon Integer indicating number of weeks to evaluate
#' @param max_delay Maximum delay
#' @param model_name Character string indicating name of the model
#' @importFrom tidyselect starts_with
#' @importFrom lubridate ceiling_date ymd
#' @autoglobal
#' @returns Nowcast dataframe
impl_madph_method_from_daily <- function(multipliers,
                                         age_group,
                                         all_data,
                                         nowcast_date,
                                         pathogen_i,
                                         eval_horizon,
                                         max_delay,
                                         model_name) {
  max_delay_daily <- 7 * max_delay
  if (age_group == "00+") {
    all_data <- all_data |>
      group_by(
        reference_date, report_date,
        delay, pathogen,
        pathogen_name
      ) |>
      summarise(
        count = sum(count),
        age_group = "00+"
      ) |>
      ungroup()
  }

  this_data <- all_data |>
    filter(
      report_date <= nowcast_date,
      pathogen == pathogen_i
    ) |>
    mutate(
      end_of_week_reference_date = ceiling_date(reference_date,
        unit = "week",
        week_start = 6,
        change_on_boundary = FALSE
      ),
      end_of_week_report_date = ceiling_date(report_date,
        unit = "week",
        week_start = 6,
        change_on_boundary = FALSE
      )
    ) |>
    group_by(
      end_of_week_reference_date,
      end_of_week_report_date,
      age_group
    ) |>
    summarise(count = sum(count, na.rm = TRUE)) |>
    # Index delays at 1
    mutate(delay = (as.integer(
      ymd(end_of_week_report_date) - ymd(end_of_week_reference_date)
    )) / 7) |>
    filter(delay <= max_delay) |>
    ungroup() |>
    rename(
      reference_date = end_of_week_reference_date,
      report_date = end_of_week_report_date
    )

  initial_data_summed <- this_data |>
    group_by(
      reference_date,
      age_group
    ) |>
    summarise(initial_count = sum(count, na.rm = TRUE))

  final_data_summed <- all_data |>
    mutate(end_of_week_reference_date = ceiling_date(reference_date,
      unit = "week",
      week_start = 6
    )) |>
    filter(
      pathogen == pathogen_i,
      delay <= max_delay_daily, # Might want to change this so that it is still
      # a rolling evaluation but its longer
      reference_date <= nowcast_date
    ) |>
    group_by(
      end_of_week_reference_date,
      age_group
    ) |>
    summarise(final_count = sum(count, na.rm = TRUE)) |>
    ungroup() |>
    rename(reference_date = end_of_week_reference_date)

  pathogen_name <- all_data |>
    filter(pathogen == pathogen_i) |>
    distinct(pathogen_name) |>
    pull(pathogen_name)

  multipliers <- filter(multipliers, pathogen == pathogen_i)

  nowcast_df <- this_data |>
    filter(reference_date <= nowcast_date) |>
    group_by(reference_date, age_group) |>
    summarise(
      count = sum(count)
    ) |>
    mutate(delay = ceiling(as.integer(nowcast_date - reference_date) / 7)) |>
    left_join(multipliers, by = c("delay", "age_group")) |>
    # Nowcasting step: divide by the completeness multiplier!
    # nolint start
    mutate(
      `est_final_count_0.5` = count / median,
      `est_final_count_0.025` = count / `97.5%`,
      `est_final_count_0.975` = count / `2.5%`
    ) |>
    # nolint end
    ungroup() |>
    filter(reference_date >= max(reference_date) - weeks(eval_horizon)) |>
    pivot_longer(
      cols = starts_with("est_final_count_"),
      names_to = "quantile_level",
      names_prefix = "est_final_count_",
      values_to = "quantile_value"
    ) |>
    mutate(
      quantile_level = as.numeric(quantile_level),
      pathogen = pathogen_i,
      pathogen_name = pathogen_name,
      nowcast_date = nowcast_date,
      scale_factor = NA,
      prop_delay = NA,
      model_type = "dph our implementation",
      model = model_name
    ) |>
    left_join(initial_data_summed,
      by = c("reference_date", "age_group")
    ) |>
    left_join(final_data_summed,
      by = c("reference_date", "age_group")
    ) |>
    dplyr::select(
      reference_date, quantile_value, quantile_level,
      pathogen, pathogen_name, nowcast_date,
      age_group, scale_factor, prop_delay, model_type,
      final_count, initial_count, model
    )
  return(nowcast_df)
}
