write_medfateland_object <- function(corrected_data) {
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

  # write the object
  corrected_data |>
    dplyr::as_tibble() |>
    saveRDS(output_filename)

  return(output_filename)
}