test_province <- function(sf_province, 
                          ifn, province) {
  control = medfate::defaultControl()
  control$leafResults <- FALSE
  control$plantResults <- FALSE
  control$soilResults <- FALSE
  control$standResults <- FALSE
  control$snowResults <- FALSE
  cli::cli_h1(paste0("Testing ", ifn, " province ", province))
  cli::cli_progress_step("Checking")
  sf_province <- sf_province |> 
    medfateland::check_topography(missing_action = "default") |>
    medfateland::check_soils(missing_action = "default") |>
    medfateland::check_forests(SpParams = traits4models::SpParamsES, missing_action = "filter")
  cli::cli_progress_step("Testing")
  res_day <- medfateland::spwb_spatial_day(sf_province, SpParams = traits4models::SpParamsES, meteo=medfate::examplemeteo, date = as.Date("2001-02-01"),
                                           local_control = control, progress = FALSE)
  res_day$state <- NULL
  cli::cli_progress_done()
  return(res_day)
}
