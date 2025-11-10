load_data_targets <- list(
  tar_group_by(
    name = pathogens_grouped,
    command = pathogens,
    pathogen
  ),
  tar_target(
    name = raw_data,
    command = read_pathogen_data(
      pathogens_grouped,
      pathogen_data_fp
    ),
    pattern = pathogens_grouped
  )
)
