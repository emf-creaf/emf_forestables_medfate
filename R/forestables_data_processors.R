process_forestables_data <- function(forestables_file) {

  # get the province and ifn version from the file name
  province <- stringr::str_sub(forestables_file, 6, 7)
  version <- stringr::str_sub(forestables_file, 1, 4)

  # log
  cli::cli_inform(c(
    "i" = "Processing {version} for province {province}"
  ))
  # first of all, if file doesn't exist, throw an error and exit gracefully
  forestables_path <- paste0(
    Sys.getenv("EMF_DATASETS_PATH"), Sys.getenv("IFN_FORESTABLES_PATH"),
    version, "_", province, ".rds"
  )
  if (!file.exists(forestables_path)) {
    cli::cli_abort(c(
      x = "{.file {forestables_path}} doesn't exist. Aborting..."
    ))
  }

  # load forestables data
  forestables_data <- readRDS(forestables_path)

  # load best coordinates for plots
  best_coords <- sf::read_sf(
    paste0(
      Sys.getenv("EMF_DATASETS_PATH"), Sys.getenv("IFN_COORD_PATH"),
      toupper(version),
      "_spain_best_ETRS89H30.gpkg"
    )
  ) |>
    # Remove columns not needed
    dplyr::select(
      -dplyr::any_of(c("IDPARCELA", "IDCLASE", "IDCLASE_IFN3", "ID_IFN3"))
    )

  # soilgrids and dem data.
  # In this case depends on the province, as canarias has another projection
  if (province %in% c("35", "38")) {
    soilgrids_path <- paste0(
      Sys.getenv("EMF_DATASETS_PATH"), Sys.getenv("SOILGRIDS_CAN_PATH")
    )
    dem_data <- terra::rast(
      paste0(
        Sys.getenv("EMF_DATASETS_PATH"), Sys.getenv("DEM_PATH"),
        "PNOA_MDT25_P", province, "_ETRS89_H28.tif"
      )
    )
  } else {
    soilgrids_path <- paste0(
      Sys.getenv("EMF_DATASETS_PATH"), Sys.getenv("SOILGRIDS_PEN_PATH")
    )
    dem_data <- terra::rast(
      paste0(
        Sys.getenv("EMF_DATASETS_PATH"), Sys.getenv("DEM_PATH"),
        "PNOA_MDT25_P", province, "_ETRS89_H30.tif"
      )
    )
  }

  # log
  cli::cli_inform(c(
    "    - Updating best coordinates"
  ))
  # updating coordinates for those plots we have best coordinates. This process
  # depends on version
  if (version %in% c("ifn3", "ifn4")) {
    join_by <- c(
      province_code = "Provincia", plot = "Estadillo",
      class = "Clase", subclass = "Subclase"
    )
    id_var <- rlang::quo(ID)
  } else {
    join_by <- c(province_code = "Provincia", plot = "Estadillo")
    id_var <- rlang::quo(ID_IFN2)
  }

  forestables_data_best <- forestables_data |>
    dplyr::mutate(province_code = substr(id_unique_code, 1, 2)) |>
    dplyr::select(-coordx, -coordy, -coord_sys, -crs) |>
    dplyr::inner_join(best_coords, by = join_by) |>
    dplyr::rename(geometry = geom) |>
    dplyr::select(-!!id_var) |>
    sf::st_as_sf() |>
    sf::st_transform(crs = 4326)

  # we need rows here, if not transform to NULL
  if (nrow(forestables_data_best) < 1) {
    forestables_processed_best <- NULL
  } else {
    forestables_processed_best <- forestables_data_best |>
      dplyr::mutate(version = version) |>
      medfateland::parse_forestable(keepUnfilteredCopy = TRUE,  progress = FALSE)
  }

  # now the rest of the plots without best coordinates
  forestables_data_others <- forestables_data |>
    dplyr::filter(!(id_unique_code %in% forestables_data_best$id_unique_code))
  # we need rows here, if not transform to NULL
  if (nrow(forestables_data_others) < 1) {
    forestables_processed_others <- NULL
  } else {
    forestables_processed_others <- forestables_data_others |>
      dplyr::mutate(version = version) |>
      medfateland::parse_forestable(keepUnfilteredCopy = TRUE,  progress = FALSE)
  }

  # join best coords and others plots
  forestables_processed_all <- dplyr::bind_rows(
    forestables_processed_best, forestables_processed_others
  )

  # we need rows here, if not abort
  if (nrow(forestables_processed_all) < 1) {
    cli::cli_abort(c(
      "x" = "No data available for {version} in province {province}"
    ))
  }

  # build the final res
  #  - add topo
  #  - add soils
  #  - check forests, check topo, check soils
  #  - return res
  # log
  cli::cli_inform(c(
    "    - Adding topo and soil"
  ))
  browser()
  res <- forestables_processed_all |>
    dplyr::arrange(id) |>
    dplyr::select(-slope, -aspect) |>
    medfateland::add_topography(dem = dem_data, progress = FALSE) |>
    medfateland::add_soilgrids(soilgrids_path = soilgrids_path, progress = FALSE)

  # checks
  # log
  cli::cli_inform(c(
    "    - Checking created objects"
  ))
  medfateland::check_forests(res)
  medfateland::check_topography(res)
  medfateland::check_soils(res)

  return(res)
}