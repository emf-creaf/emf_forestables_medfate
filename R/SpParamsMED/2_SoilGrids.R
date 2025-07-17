# Fetches soil parameters from Soil depth database and SoilGrids
# IFN2 is not processed because soils are copied from IFN3 plots (A1, A3C, A4C, A6C)
library(medfateutils)


dataset_path = "~/OneDrive/EMF_datasets/"


# Soils ICGC --------------------------------------------------------------
soildepth_rf <- terra::rast(paste0(dataset_path, "Soils/Products/Catalunya/SoilDepth_mm_RF_ETRS89_UTM31N.tif"))

#### IFN3 - Catalonia ####
ifn3_best_sf <- sf::read_sf(paste0(dataset_path, "ForestInventories/IFN/Products/Coordinates/IFN3_cat_best_ETRS89H31.gpkg"))
ifn3_best_vect <- terra::vect(ifn3_best_sf |> sf::st_transform(terra::crs(soildepth_rf)))
ifn3_best_sf$soildepth_rf <- terra::extract(soildepth_rf, ifn3_best_vect)$predicted_prof_mm
sf::write_sf(ifn3_best_sf, "Products/IFN3/Catalunya/IFN3_cat_best_soildepth_rf_ETRS89H31.gpkg")


# Soil depth (Shangguan 2017) --------------------------------------------------------------

bdricm <- terra::rast(paste0(dataset_path, "Soils/Sources/Global/SoilDepth_Shangguan2017/BDRICM_M_250m_ll.tif"))
bdrlog <- terra::rast(paste0(dataset_path, "Soils/Sources/Global/SoilDepth_Shangguan2017/BDRLOG_M_250m_ll.tif"))
bdticm <- terra::rast(paste0(dataset_path, "Soils/Sources/Global/SoilDepth_Shangguan2017/BDTICM_M_250m_ll.tif"))

#### IFN3 - Catalonia ####
ifn3_best_sf <- sf::read_sf(paste0(dataset_path, "ForestInventories/IFN/Products/Coordinates/IFN3_cat_best_ETRS89H31.gpkg")) 
ifn3_best_vect <- terra::vect(ifn3_best_sf |> sf::st_transform(terra::crs(bdricm)))
ifn3_best_sf$bdricm <- terra::extract(bdricm, ifn3_best_vect)$BDRICM_M_250m_ll*10 #cm to mm
ifn3_best_sf$bdrlog <- terra::extract(bdrlog, ifn3_best_vect)$BDRLOG_M_250m_ll/100 #prob of occurence of R horizon
ifn3_best_sf$bdticm <- terra::extract(bdticm, ifn3_best_vect)$BDTICM_M_250m_ll*10 #cm to mm
sf::write_sf(ifn3_best_sf, "Products/IFN3/Catalunya/IFN3_cat_best_soildepth_ETRS89H31.gpkg")

#### IFN4 - Catalonia ####
ifn4_best_sf <- sf::read_sf(paste0(dataset_path, "ForestInventories/IFN/Products/Coordinates/IFN4_cat_best_ETRS89H31.gpkg")) 
ifn4_best_vect <- terra::vect(ifn4_best_sf |> sf::st_transform(terra::crs(bdricm)))
ifn4_best_sf$bdricm <- terra::extract(bdricm, ifn4_best_vect)$BDRICM_M_250m_ll*10 #cm to mm
ifn4_best_sf$bdrlog <- terra::extract(bdrlog, ifn4_best_vect)$BDRLOG_M_250m_ll/100 #prob of occurence of R horizon
ifn4_best_sf$bdticm <- terra::extract(bdticm, ifn4_best_vect)$BDTICM_M_250m_ll*10 #cm to mm
sf::write_sf(ifn4_best_sf, "Products/IFN4/Catalunya/IFN4_cat_best_soildepth_ETRS89H31.gpkg")

#### IFN3 - Spain ####
ifn3_best_sf <- sf::read_sf(paste0(dataset_path, "ForestInventories/IFN/Products/Coordinates/IFN3_spain_best_ETRS89H30.gpkg")) 
ifn3_best_vect <- terra::vect(ifn3_best_sf |> sf::st_transform(terra::crs(bdricm)))
ifn3_best_sf$bdricm <- terra::extract(bdricm, ifn3_best_vect)$BDRICM_M_250m_ll*10 #cm to mm
ifn3_best_sf$bdrlog <- terra::extract(bdrlog, ifn3_best_vect)$BDRLOG_M_250m_ll/100 #prob of occurence of R horizon
ifn3_best_sf$bdticm <- terra::extract(bdticm, ifn3_best_vect)$BDTICM_M_250m_ll*10 #cm to mm
sf::write_sf(ifn3_best_sf, "Products/IFN3/Spain/IFN3_spain_best_soildepth_ETRS89H30.gpkg")

#### IFN4 - Spain ####
ifn4_best_sf <- sf::read_sf(paste0(dataset_path, "ForestInventories/IFN/Products/Coordinates/IFN4_spain_best_ETRS89H30.gpkg")) 
ifn4_best_vect <- terra::vect(ifn4_best_sf |> sf::st_transform(terra::crs(bdricm)))
ifn4_best_sf$bdricm <- terra::extract(bdricm, ifn4_best_vect)$BDRICM_M_250m_ll*10 #cm to mm
ifn4_best_sf$bdrlog <- terra::extract(bdrlog, ifn4_best_vect)$BDRLOG_M_250m_ll/100 #prob of occurence of R horizon
ifn4_best_sf$bdticm <- terra::extract(bdticm, ifn4_best_vect)$BDTICM_M_250m_ll*10 #cm to mm
sf::write_sf(ifn4_best_sf, "Products/IFN4/Spain/IFN4_spain_best_soildepth_ETRS89H30.gpkg")


# Soilgrids ---------------------------------------------------------------
regions <- c("Catalunya", "Spain")
regions_small <- c("cat", "spain")
huses <- c("H31", "H30")
for(ir in 1:2) {
  region <- regions[ir]
  region_small <- regions_small[ir]
  huse <- huses[ir]
  for(ifn in c("IFN3", "IFN4")) {
    cat(paste0("\n Region: ", region, " IFN: ", ifn, "\n"))
    best_sf <- sf::read_sf(paste0(dataset_path, "ForestInventories/IFN/Products/Coordinates/", ifn,"_", region_small,"_best_ETRS89", huse, ".gpkg")) 
    soillist <- vector("list", nrow(best_sf))
    names(soillist) <- best_sf$ID
    pb=txtProgressBar(1, nrow(best_sf), style=3)
    for(i in 1:nrow(best_sf)) {
      setTxtProgressBar(pb, i)
      s <- medfateutils::soilgridsParams(best_sf[i,], c(300,700,1000,2000))
      if(!is.null(s)) soillist[[i]] <- s
    }
    saveRDS(soillist, paste0("Products/",ifn,"/", region, "/", ifn,"_", region_small,"_best_soilgrids_ETRS89", huse,".rds"))
  }
}