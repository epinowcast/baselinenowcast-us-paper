#' Load in data
#'
#' Data for each pathogen comes from the combined file (all pathogens), with
#'   reference dates before the start of the combined file filled in from the
#'   earlier per-pathogen file. Reference dates with no visits for an age group
#'   are padded with a count of 0 reported on the reference date, so they are
#'   treated as zeros rather than missing.
#'
#' @param df Grouped data.frame
#' @param fp File.path of the folder containing the per-pathogen data
#' @param combined_data_file File.path of the combined data across pathogens
#' @importFrom readr read_csv
#' @importFrom janitor clean_names
#' @importFrom dplyr mutate filter select bind_rows arrange all_of anti_join
#' @importFrom tidyr expand_grid
#' @importFrom glue glue
#' @export
#' @autoglobal
read_pathogen_data <- function(df, fp, combined_data_file) {
  pathogen_i <- unique(df$pathogen)
  cols_to_keep <- c(
    "reference_date", "report_date", "age_group",
    "pathogen", "count"
  )
  combined_data <- read_csv(combined_data_file) |>
    clean_names() |>
    filter(pathogen == pathogen_i) |>
    select(all_of(cols_to_keep))
  earlier_data <- read_csv(file.path(fp, glue("{pathogen_i}.csv"))) |>
    clean_names() |>
    mutate(pathogen = pathogen_i) |>
    filter(reference_date < min(combined_data$reference_date)) |>
    select(all_of(cols_to_keep))

  raw_data <- bind_rows(earlier_data, combined_data)

  # Pad reference dates with no visits with a zero count
  zero_rows <- expand_grid(
    reference_date = seq(
      from = min(raw_data$reference_date),
      to = max(raw_data$reference_date),
      by = "day"
    ),
    age_group = unique(raw_data$age_group)
  ) |>
    anti_join(raw_data, by = c("reference_date", "age_group")) |>
    mutate(
      report_date = reference_date,
      pathogen = pathogen_i,
      count = 0
    )

  raw_data <- bind_rows(raw_data, zero_rows) |>
    arrange(reference_date, report_date, age_group)
  return(raw_data)
}

#' Get DPH nowcasts
#'
#' @param fp filepath of DPH nowcasts
#'
#' @returns Data.frame of nowcasts from DPH model
#' @export
#' @importFrom readr read_csv cols col_date col_character
#' @autoglobal
get_dph_nowcasts <- function(fp) {
  ma_nowcasts <- read_csv(fp)
  # State-level nowcasts are not stratified by age group
  if (!"age_group" %in% names(ma_nowcasts)) {
    ma_nowcasts <- mutate(ma_nowcasts, age_group = "00+")
  }
  ma_nowcasts <- ma_nowcasts |> # Add a fix for excel formatting issues
    mutate(
      quantile_level = as.numeric(quantile_level),
      age_group = ifelse(age_group == "May-17", "05-17", age_group),
      pathogen_name = ifelse(
        pathogen_name == "Broad Acute Respiratory Incidence",
        "Broad Acute Respiratory",
        pathogen_name
      )
    )

  return(ma_nowcasts)
}

#' Clean DPH nowcasts
#'
#' @param ma_nowcasts Raw nowcasts from DPH
#'
#' @returns only the MA nowcasts with only the columns required, with only
#'   the first copy of any duplicated forecasts
#' @importFrom dplyr select mutate filter distinct
#' @autoglobal
clean_dph_nowcasts <- function(ma_nowcasts,
                               eval_horizon) {
  ma_nowcasts_clean <- ma_nowcasts |>
    select(
      reference_date, quantile_value, quantile_level,
      pathogen, pathogen_name, nowcast_date, age_group, scale_factor,
      prop_delay, model_type, initial_count
    ) |>
    filter(model_type == "dph base") |>
    mutate(nowcast_date = nowcast_date + days(4)) |>
    # Some nowcast dates were run more than once, keep the first run
    distinct(pathogen, age_group, reference_date, nowcast_date, quantile_level,
      .keep_all = TRUE
    )

  return(ma_nowcasts_clean)
}

#' Clean DPH nowcasts by age group
#'
#' @param ma_nowcasts Raw nowcasts from DPH
#'
#' @returns only the MA nowcasts with only the columns required, with only
#'   the first copy of any duplicated forecasts
#' @autoglobal
clean_dph_nowcasts_ag <- function(ma_nowcasts,
                                  eval_horizon) {
  ma_nowcasts_clean <- ma_nowcasts |>
    select(
      reference_date, quantile_value, quantile_level,
      pathogen, pathogen_name, nowcast_date, age_group, scale_factor,
      prop_delay, model_type
    ) |>
    filter(
      model_type == "dph base",
      age_group != "Unknown"
    ) |>
    mutate(nowcast_date = nowcast_date + days(4)) |>
    # Some nowcast dates were run more than once, keep the first run
    distinct(pathogen, age_group, reference_date, nowcast_date, quantile_level,
      .keep_all = TRUE
    )
  return(ma_nowcasts_clean)
}

#' Add ground truth counts to nowcasts
#'
#' @param nowcasts Data.frame of nowcasts (any method) without initial/final
#'   counts
#' @param ground_truth_nowcasts Data.frame of nowcasts from a reference method
#'   (e.g., baselinenowcast) that contains initial_count and final_count columns
#' @param keep_initial_count Boolean indicating whether to keep the
#'   initial_count already in `nowcasts` (e.g. the data available in real-time)
#'   rather than replacing it with the ground truth initial_count. Default
#'   `FALSE`.
#'
#' @returns nowcasts with initial_count and final_count joined from the ground
#'   truth
#' @importFrom dplyr select left_join distinct any_of all_of
#' @export
#' @autoglobal
add_ground_truth_counts <- function(nowcasts, ground_truth_nowcasts,
                                    keep_initial_count = FALSE) {
  counts_to_replace <- if (keep_initial_count) {
    "final_count"
  } else {
    c("initial_count", "final_count")
  }
  # Extract initial and final counts from ground truth
  ground_truth_counts <- ground_truth_nowcasts |>
    select(
      reference_date, pathogen, nowcast_date, age_group,
      all_of(counts_to_replace)
    ) |>
    distinct()

  # Remove any existing counts that will be replaced from nowcasts
  nowcasts_clean <- nowcasts |>
    select(-any_of(counts_to_replace))

  # Join the ground truth counts
  nowcasts_with_truth <- nowcasts_clean |>
    left_join(ground_truth_counts,
      by = c("reference_date", "pathogen", "nowcast_date", "age_group")
    )

  return(nowcasts_with_truth)
}
