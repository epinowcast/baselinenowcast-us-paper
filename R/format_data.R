format_data <- function(data, pathogen) {
  data <- data |> mutate(data = pathogen)
  return(data)
}
