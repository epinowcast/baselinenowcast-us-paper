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
#' @autoglobal
get_madph_nowcasts <- function(fp) {
  ma_nowcasts <- read_csv(fp,
    col_types = cols(
      reference_date = col_date(format = "%d/%m/%Y"),
      nowcast_date = col_date(format = "%d/%m/%Y")
    )
  ) |>
    select(
      reference_date, quantile_value, quantile_level,
      pathogen, pathogen_name, nowcast_date, age_group, scale_factor, prop_delay,
      model_type, final_count, initial_count
    ) |>
    filter(model_type == "dph base")

  return(ma_nowcasts)
}
