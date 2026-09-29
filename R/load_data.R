#' Load in data
#'
#' @param df Grouped data.frame
#' @param fp File.path of data
#' @importFrom readr read_csv
#' @importFrom janitor clean_names
#' @importFrom dplyr mutate
#' @importFrom glue glue
#' @export
#' @autoglobal
read_pathogen_data <- function(df, fp) {
  pathogen <- unique(df$pathogen)
  raw_data <- read_csv(file.path(fp, glue("{pathogen}.csv"))) |>
    clean_names() |>
    mutate(pathogen = pathogen)
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
  ma_nowcasts <- read_csv(fp) |> # Add a fix for excel formatting issues
    mutate(age_group = ifelse(age_group == "May-17", "05-17", age_group))

  return(ma_nowcasts)
}

#' Clean DPH nowcasts
#'
#' @param ma_nowcasts Raw nowcasts from DPH
#'
#' @returns only the MA nowcasts with only the columns required
#' @importFrom dplyr select mutate filter
#' @autoglobal
clean_dph_nowcasts <- function(ma_nowcasts,
                               eval_horizon) {
  ma_nowcasts_clean <- ma_nowcasts |>
    select(
      reference_date, quantile_value, quantile_level,
      pathogen, pathogen_name, nowcast_date, age_group, scale_factor,
      prop_delay, model_type
    ) |>
    filter(model_type == "dph base") |>
    mutate(nowcast_date = nowcast_date + days(4))

  return(ma_nowcasts_clean)
}

#' Clean DPH nowcasts by age group
#'
#' @param ma_nowcasts Raw nowcasts from DPH
#'
#' @returns only the MA nowcasts with only the columns required
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
    mutate(nowcast_date = nowcast_date + days(4))
  return(ma_nowcasts_clean)
}

#' Add ground truth counts to nowcasts
#'
#' @param nowcasts Data.frame of nowcasts (any method) without initial/final
#'   counts
#' @param ground_truth_nowcasts Data.frame of nowcasts from a reference method
#'   (e.g., baselinenowcast) that contains initial_count and final_count columns
#'
#' @returns nowcasts with initial_count and final_count joined from the ground
#'   truth
#' @importFrom dplyr select left_join distinct any_of
#' @export
#' @autoglobal
add_ground_truth_counts <- function(nowcasts, ground_truth_nowcasts) {
  # Extract initial and final counts from ground truth
  ground_truth_counts <- ground_truth_nowcasts |>
    select(
      reference_date, pathogen, nowcast_date, age_group,
      initial_count, final_count
    ) |>
    distinct()

  # Remove any existing initial/final counts from nowcasts
  nowcasts_clean <- nowcasts |>
    select(-any_of(c("initial_count", "final_count")))

  # Join the ground truth counts
  nowcasts_with_truth <- nowcasts_clean |>
    left_join(ground_truth_counts,
      by = c("reference_date", "pathogen", "nowcast_date", "age_group")
    )

  return(nowcasts_with_truth)
}
