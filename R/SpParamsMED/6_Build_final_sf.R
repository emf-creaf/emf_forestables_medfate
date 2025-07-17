## Builds final sf objects to be used with medfateland (or otherwise)

# Build sf objects --------------------------------------------------------
regions <- c("Catalunya", "Spain")
regions_small <- c("cat", "spain")
huses <- c("H31", "H30")
empty_forest <- medfate::emptyforest()
for(ir in 1:2) {
  region <- regions[ir]
  region_small <- regions_small[ir]
  huse <- huses[ir]
  for(ifn in c("IFN2", "IFN3", "IFN4")) {
    cat(paste0("\n Region: ", region, " IFN: ", ifn, "\n"))
    best_sf <- sf::read_sf(paste0("Products/", ifn,"/", region,"/", ifn,"_", region_small,"_best_topo_ETRS89", huse,".gpkg")) 
    forestlist <- readRDS(paste0("Products/", ifn,"/", region,"/", ifn,"_", region_small,"_forestlist.rds"))
    forestlist_allrecords <- readRDS(paste0("Products/", ifn,"/", region,"/", ifn,"_", region_small,"_forestlist_allrecords.rds"))
    soillist <- readRDS(paste0("Products/", ifn,"/", region,"/", ifn,"_", region_small,"_best_soilgrids_mod_ETRS89", huse,".rds"))
    names(soillist) <- NULL
    best_sf$soil <- soillist
    if(ifn == "IFN2") {
      df_forest <- tibble::as_tibble(data.frame(ID_IFN2 = names(forestlist)))
    } else {
      df_forest <- tibble::as_tibble(data.frame(ID = names(forestlist)))
    }
    names(forestlist) <- NULL
    df_forest$forest <- forestlist
    df_forest$forest_allrecords <- forestlist_allrecords
    cat(paste0("  Num plots with coordinates/topography: ", nrow(best_sf), "\n"))
    cat(paste0("  Num forests: ", nrow(df_forest), "\n"))
    if(ifn == "IFN2") {
      final_sf <- dplyr::left_join(best_sf, df_forest, by="ID_IFN2")
      final_sf$id <- final_sf$ID_IFN3
      final_sf <- final_sf[,c("Provincia", "Estadillo", "ID_IFN2", "IDCLASE_IFN3", "ID_IFN3","id", 
                              "elevation", "slope", "aspect", "soil", "forest",  "forest_allrecords","geom")]
    } else {
      final_sf <- dplyr::left_join(best_sf, df_forest, by="ID")
      final_sf$id <- final_sf$ID
      final_sf <- final_sf[,c("Provincia", "Estadillo", "Clase", "Subclase", "IDPARCELA", "IDCLASE", "ID", "id",
                              "elevation", "slope", "aspect", "soil", "forest", "forest_allrecords","geom")]
    }
    cat(paste0("  Num plots with coordinates/topography and soil data: ", sum(!sapply(final_sf$soil, is.null)), "\n"))
    null_forest <- sapply(final_sf$forest, is.null)
    null_forest_allrecords <- sapply(final_sf$forest_allrecords, is.null)
    cat(paste0("  Num plots with coordinates/topography and forest data: ", sum(!null_forest), "\n"))
    print(tapply(null_forest, final_sf$Provincia, sum))
    cat(paste0("  Filling ", sum(null_forest), " empty forests \n"))
    w_null <- which(null_forest)
    pb <- txtProgressBar(1, length(w_null), style = 3)
    for(i in 1:length(w_null)) {
      setTxtProgressBar(pb, i)
      final_sf$forest[[w_null[i]]] <- empty_forest
    }
    cat(paste0("  Filling ", sum(null_forest_allrecords), " empty forests (all records) \n"))
    w_null <- which(null_forest_allrecords)
    pb <- txtProgressBar(1, length(w_null), style = 3)
    for(i in 1:length(w_null)) {
      setTxtProgressBar(pb, i)
      final_sf$forest_allrecords[[w_null[i]]] <- empty_forest
    }
    cat("\n")
    saveRDS(final_sf, paste0("Products/", ifn,"/", region,"/", ifn,"_", region_small,"_final_ETRS89", huse,".rds"))
  }
}
