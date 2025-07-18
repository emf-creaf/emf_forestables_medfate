
# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)
library(crew)
library(stringr)

# Set target options:
tar_option_set(
  packages = c("tibble", "sf", "medfate", "medfateland", "terra", "dplyr", "cli"),
  format = "qs",
  memory = "transient",
  # Workers to be changed in server:
  controller = crew::crew_controller_local(workers = 10),
  iteration = "list"
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source()

# We start from the result of the emf_ifn_forestables pipeline, that is the
# forestables objects rds files
provinces <- seq(1, 50) |>
  as.character() |>
  stringr::str_pad(2, "left", "0")
versions <- c("ifn2", "ifn3", "ifn4")
forestables_files <- c(
  paste0(versions[1], "_", provinces, ".rds"),
  paste0(versions[2], "_", provinces, ".rds"),
  paste0(versions[3], "_", provinces, ".rds")
)

# emf_forestables_medfate target list
list(
  # input files target
  tar_target(input_files, forestables_files),
  # processing data target
  tar_target(
    processed_data, process_forestables_data(input_files),
    pattern = map(input_files),
    error = "null"
  ),
  # correcting soils target
  tar_target(
    corrected_data, correct_soils(processed_data),
    pattern = map(processed_data),
    error = "null"
  ),
  # writing target
  tar_target(
    written_files, write_medfateland_object(corrected_data),
    pattern = map(corrected_data),
    format = "file",
    error = "null"
  )
)