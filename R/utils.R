#' Suppress output and messages for code.
#' @param code Code to run quietly.
#' @return The result of running the code.
#' @export
#' @examples
#' result <- quiet(message("This message should be suppressed"))
#' print(result)
quiet <- function(code) {
  sink(nullfile())
  on.exit(sink())
  return(suppressMessages(code))
}

#' Load in data
#'
#' @param df Grouped data.frame
#' @param fp File.path of data
load_data <- function(df, fp) {
  pathogen <- unique(df$pathogen)
  data <- read_csv(file.path(fp, glue::glue("{pathogen}.csv")))
  return(data)
}
