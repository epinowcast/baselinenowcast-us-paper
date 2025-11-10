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
