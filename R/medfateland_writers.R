write_medfateland_object <- function(corrected_data) {
  # abort if null before anything
  if (is.null(corrected_data)) {
    cli::cli_abort("No data")
  }
  # get the province and ifn version from the file name
  province <- substr(corrected_data$id, 1, 2) |>
    unique()
  version <- corrected_data$version |>
    unique()

  # file name
  output_filename <- paste0(
    Sys.getenv("EMF_DATASETS_PATH"), Sys.getenv("MEDFATELAND_DATA_ROOT"),
    "medfateland_", version, "_", province, "_soilmod_WGS84.rds"
  )

  # Before writing, we should check the object with a model run for one day
  # If the model fails, we gracefully exit
  # log
  cli::cli_inform(c(
    "i" = "Testing model for province {province} in {version}"
  ))
  control <- medfate::defaultControl()
  control$leafResults <- FALSE
  control$plantResults <- FALSE
  control$soilResults <- FALSE
  control$standResults <- FALSE
  control$snowResults <- FALSE
  test_data <- corrected_data |>
    medfateland::check_topography(missing_action = "default") |>
    medfateland::check_soils(missing_action = "default") |>
    medfateland::check_forests(
      SpParams = traits4models::SpParamsES,
      missing_action = "filter"
    )
  res_day <- try({
    medfateland::spwb_spatial_day(
      test_data,
      SpParams = traits4models::SpParamsES,
      meteo = medfate::examplemeteo, date = as.Date("2001-02-01"),
      local_control = control, progress = FALSE
    )
  })
  # stop if error
  if (inherits(res_day, "try-error")) {
    cli::cli_abort(c(
      x = "Model failed, aborting",
      i = res_day[1]
    ))
  }

  # write the object
  # log
  cli::cli_inform(c(
    "i" = "Writing data for province {province} in {version}"
  ))
  corrected_data |>
    dplyr::as_tibble() |>
    saveRDS(output_filename)

  return(output_filename)
}