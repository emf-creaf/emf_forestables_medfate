correct_soils <- function(processed_data) {
  # abort if null before anything
  if (is.null(processed_data)) {
    cli::cli_abort("No data")
  }

  # get the province and ifn version from the file name
  province <- substr(processed_data$id, 1, 2) |>
    unique()
  version <- processed_data$version |>
    unique()

  # log
  cli::cli_inform(c(
    "i" = "Correcting soil for province {province} in {version}"
  ))

  # Load soil depth data
  soil_depth_path <- paste0(
    Sys.getenv("EMF_DATASETS_PATH"), Sys.getenv("SOIL_DEPTH_PATH")
  )

  bdricm_data <- terra::rast(paste0(soil_depth_path, "BDRICM_M_250m_ll.tif"))
  bdrlog_data <- terra::rast(paste0(soil_depth_path, "BDRLOG_M_250m_ll.tif"))
  bdticm_data <- terra::rast(paste0(soil_depth_path, "BDTICM_M_250m_ll.tif"))

  # create extent of geometries to crop soil depth data
  processed_extent <- processed_data |>
    sf::st_geometry() |>
    sf::st_transform(terra::crs(bdricm_data)) |>
    terra::vect() |>
    terra::ext()

  bdricm_data <- terra::crop(bdricm_data, processed_extent, snap = "out")
  bdrlog_data <- terra::crop(bdrlog_data, processed_extent, snap = "out")
  bdticm_data <- terra::crop(bdticm_data, processed_extent, snap = "out")

  # modify soils
  # log
  cli::cli_inform(c(
    "    - Modifying soils"
  ))
  res <- medfateland::modify_soils(
    processed_data,
    soil_depth_map = (bdricm_data$BDRICM_M_250m_ll * 10) *
      (1 - (bdrlog_data$BDRLOG_M_250m_ll / 100)),
    depth_to_bedrock_map = bdticm_data * 10,
    progress = TRUE)

  # check the new soils
  # log
  cli::cli_inform(c(
    "    - Checking new soils"
  ))
  medfateland::check_soils(res)

  return(res)
}