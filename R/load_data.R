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
  raw_data <- read_csv(file.path(fp, glue::glue("{pathogen}.csv"))) |>
    janitor::clean_names() |>
    mutate(pathogen = pathogen)
  return(raw_data)
}

#' Get MADPH nowcasts
#'
#' @param fp filepath of MADPH nowcasts
#'
#' @returns Data.frame of nowcasts from MADPH model
#' @export
#' @importFrom readr read_csv cols col_date col_character
#' @autoglobal
get_madph_nowcasts <- function(fp) {
  ma_nowcasts <- read_csv(fp,
    col_types = cols(
      reference_date = col_date(format = "%d/%m/%Y"), # nolint
      nowcast_date = col_date(format = "%d/%m/%Y"), # nolint
      age_group = col_character()
    )
  ) |> # Add a fix for excel formatting issues
    mutate(age_group = ifelse(age_group == "May-17", "05-17", age_group))

  return(ma_nowcasts)
}

#' Clean MADPH nowcasts
#'
#' @param ma_nowcasts Raw nowcasts from MADPH
#'
#' @returns only the MA nowcasts with only the columns required
#' @importFrom dplyr select mutate filter
#' @autoglobal
clean_madph_nowcasts <- function(ma_nowcasts) {
  ma_nowcasts_clean <- ma_nowcasts |>
    select(
      reference_date, quantile_value, quantile_level,
      pathogen, pathogen_name, nowcast_date, age_group, scale_factor,
      prop_delay, model_type, final_count, initial_count
    ) |>
    filter(model_type == "dph base") |>
    mutate(
      quantile_level = case_when(
        quantile_level == 0.025 ~ 0.975,
        quantile_level == 0.975 ~ 0.025,
        TRUE ~ quantile_level
      )
    )


  return(ma_nowcasts_clean)
}

#' Clean MADPH nowcasts by age group
#'
#' @param ma_nowcasts Raw nowcasts from MADPH
#'
#' @returns only the MA nowcasts with only the columns required
#' @autoglobal
clean_madph_nowcasts_ag <- function(ma_nowcasts) {
  ma_nowcasts_clean <- ma_nowcasts |>
    select(
      reference_date, quantile_value, quantile_level,
      pathogen, pathogen_name, nowcast_date, age_group, scale_factor,
      prop_delay, final_count, initial_count,
      model
    ) |>
    filter(model == "dph base") |>
    mutate(
      quantile_level = case_when(
        quantile_level == 0.025 ~ 0.975,
        quantile_level == 0.975 ~ 0.025,
        TRUE ~ quantile_level
      )
    )


  return(ma_nowcasts_clean)
}
