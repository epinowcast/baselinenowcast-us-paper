library(targets)
library(tarchetypes)
library(here)
library(purrr)
library(dplyr)
library(tibble)
library(lubridate)
library(ggplot2)
library(ggpattern)
library(readr)
library(tidyr)
library(glue)
library(zoo)
library(epinowcast)
library(baselinenowcast)
library(scoringutils)
library(RColorBrewer)
library(patchwork)
library(fs)

# load functions
functions <- list.files(here("R"), full.names = TRUE)
purrr::walk(functions, source)
rm("functions")

# load target modules
targets <- list.files(here("targets"), full.names = TRUE)
targets <- grep("*\\.R", targets, value = TRUE)
purrr::walk(targets, source)

tar_option_set(
  packages = c(
    "tibble", "dplyr", "lubridate",
    "targets", "ggplot2", "ggpattern",
    "baselinenowcast",
    "purrr",
    "readr", "tidyr",
    "zoo",
    "epinowcast",
    "scoringutils",
    "RColorBrewer",
    "patchwork",
    "fs"
  ),
  workspace_on_error = TRUE,
  storage = "worker",
  retrieval = "worker",
  memory = "transient",
  garbage_collection = TRUE,
  format = "parquet", # default storage format
  error = "null"
)

config <- list(
  config_targets
)

# Data------------------------------------------------------------------------
# Set up the date:location:pathogen:model permutations
set_up <- list(
  create_permutation_targets
)

# Load the data (for training and producing and evaluating) nowcasts
# for each pathogen
load_data <- list(
  load_data_targets
)

# # Results --------------------------------------------------------------------
#
# # Produce nowcasts for each pathogen
# nowcasts <- list(
#   state_nowcast_targets,
#   age_group_nowcast_targets
# )
#
# # Score nowcasts
# scores <- list(
#   score_targets
# )
#
# # Plots
#
# plots <- list(
#
#   ## Delay characterisation plot targets
#   delay_plot_targets,
#
#   ## State-level nowcast evaluation figs
#   state_nowcast_eval_plot_targets,
#
#   ## Age-group specific nowcast evaluation figs
#   ag_nowcast_eval_plot_targets
# )

list(
  config,
  set_up,
  load_data
  # nowcasts,
  # scores,
  # plots
)
