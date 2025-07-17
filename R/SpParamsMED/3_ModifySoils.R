#########################################################
# Modifies soil parameters
#########################################################
dataset_path = "~/OneDrive/EMF_datasets/"

source_path3 <- paste0(dataset_path,"ForestInventories/IFN/Sources/IFN3/BBDD-Campo")
source_path4 <- paste0(dataset_path,"ForestInventories/IFN/Sources/IFN4/BBDD-Campo")
provIFN4 <- c("05","07","09","15","24","26","27","28","30","31","32","33","34","36","37","39","40",
              "42","47","49")
ccaaIFN4 <- c("Catalunya", "PaisVasco", "Canarias","Extremadura")
provIFN3 = c("01","02","03","04","05","06","07", "08","09","10",
             "11","12","13","14","15","16","17", "18","19","20",
             "21","22","23","24","25","26","27", "28","29","30",
             "31","32","33","34","35","36","37", "38","39","40",
             "41","42","43","44","45","46","47", "48","49","50")

perc <- c(0, 12.5, 37.5, 55, 65)

modify_soils <- function(soillist, df, type = "1"){
  type <- match.arg(type, c("1","2"))
  soillist_mod <- soillist
  cnt_rfc <- 0
  for(i in 1:length(soillist_mod)) {
    sdf <- soillist_mod[[i]]

    if(is.null(sdf)) {
      sdf <- medfate::defaultSoilParams()
      message(paste0("Null soil at ",i))
    }
    if(any(is.na(sdf$clay)) || any(is.na(sdf$sand))|| any(is.na(sdf$rfc)) || any(is.na(sdf$bd))) {
      sdf <- medfate::defaultSoilParams()
      message(paste0("Missing soil data at ",i))
    }
    if(mean(sdf$rfc)>90) {
      cnt_rfc <- cnt_rfc + 1 
      print(cnt_rfc)
      sdf$rfc <- sdf$rfc/10
    }
    if(type=="1") {
      sdf <- medfateutils::modifySoilDepth(sdf, soildepth = df$bdricm[i]) 
      sfrock <- 0
      if(!is.na(df$Rocosid[i])) sfrock <- perc[as.numeric(df$Rocosid[i])]
      sdf <- medfateutils::modifySoilRockContent(sdf, surfacerock = sfrock) 
    } else if(type=="2") {
      soil_depth = df$soildepth_rf[i]
      if(is.na(soil_depth)) soil_depth = mean(df$soildepth_rf, na.rm=TRUE)
      sdf <- medfateutils::modifySoilRockContentFromSoilDepth(sdf, soildepth = soil_depth) 
    }
    medfate::soil(sdf) # For checking
    soillist_mod[[i]]<- sdf
  }
  return(soillist_mod)  
}

#### IFN23 - Catalonia ####
soillist <- readRDS("Products/IFN3/Catalunya/IFN3_cat_best_soilgrids_ETRS89H31.rds")
soildepth_rf <- sf::read_sf("Products/IFN3/Catalunya/IFN3_cat_best_soildepth_rf_ETRS89H31.gpkg")
soillist_mod <- modify_soils(soillist, soildepth_rf, type="2")
# soildepth <- sf::read_sf("Products/IFN3/Catalunya/IFN3_cat_best_soildepth_ETRS89H31.gpkg")
# plotData <- IFNread::readPCParcela(source_path3, ifn = 3, prov = c("08","17","25","43"))
# plotData <- plotData[,c("ID", "Rocosid")]
# df <- soildepth |> dplyr::left_join(plotData, by="ID")
# soillist_mod <- modify_soils(soillist, df, type="1")
ifn3_best_sf <- sf::read_sf(paste0(dataset_path, "ForestInventories/IFN/Products/Coordinates/IFN3_cat_best_ETRS89H31.gpkg"))
ifn3_best_sf$taw <- unlist(lapply(soillist_mod, function(x) {sum(medfate::soil_waterExtractable(medfate::soil(x)))}))

library(ggplot2)
ggplot(ifn3_best_sf)+
  geom_sf(ggplot2::aes(col = taw), size=0.5)+
  scale_color_binned(type="viridis", breaks = c(50,75,100,125, 150, 200, 400))+
  theme_bw()
  
saveRDS(soillist_mod, "Products/IFN3/Catalunya/IFN3_cat_best_soilgrids_mod_ETRS89H31.rds")

ifn2_best_sf <- sf::read_sf(paste0(dataset_path, "ForestInventories/IFN/Products/Coordinates/IFN2_cat_best_ETRS89H31.gpkg")) 
soillist_ifn2 <- soillist_mod[ifn2_best_sf$ID_IFN3]
names(soillist_ifn2) <- ifn2_best_sf$ID_IFN2
sum(sapply(soillist_ifn2, is.null))
saveRDS(soillist_ifn2, "Products/IFN2/Catalunya/IFN2_cat_best_soilgrids_mod_ETRS89H31.rds")

#### IFN4 - Catalonia ####
soillist <- readRDS("Products/IFN4/Catalunya/IFN4_cat_best_soilgrids_ETRS89H31.rds")
soildepth <- sf::read_sf("Products/IFN4/Catalunya/IFN4_cat_best_soildepth_ETRS89H31.gpkg")
plotData <- IFNread::readPCParcela(source_path4, ifn = 4, ccaa = "Catalunya")
plotData <- plotData[,c("ID", "Rocosid")]
df <- soildepth |> dplyr::left_join(plotData, by="ID")
soillist_mod <- modify_soils(soillist, df)
sum(sapply(soillist_mod, is.null))
saveRDS(soillist_mod, "Products/IFN4/Catalunya/IFN4_cat_best_soilgrids_mod_ETRS89H31.rds")


#### IFN23 - Spain ####
soillist <- readRDS("Products/IFN3/Spain/IFN3_spain_best_soilgrids_ETRS89H30.rds")
# ifn3_best_sf <- sf::read_sf(paste0(dataset_path, "ForestInventories/IFN/Products/Coordinates/IFN3_spain_best_ETRS89H30.gpkg")) 
# names(soillist)<- ifn3_best_sf$ID # Some IDs were lost
# saveRDS(soillist, "Products/IFN3/Spain/IFN3_spain_best_soilgrids_ETRS89H30.rds")
soildepth <- sf::read_sf("Products/IFN3/Spain/IFN3_spain_best_soildepth_ETRS89H30.gpkg")
plotData <- IFNread::readPCParcela(source_path3, ifn = 3, prov = provIFN3)
plotData <- plotData[,c("ID", "Rocosid")]
df <- soildepth |> dplyr::left_join(plotData, by="ID")
soillist_mod <- modify_soils(soillist, df)
sum(sapply(soillist_mod, is.null))
saveRDS(soillist_mod, "Products/IFN3/Spain/IFN3_spain_best_soilgrids_mod_ETRS89H30.rds")

ifn2_best_sf <- sf::read_sf(paste0(dataset_path, "ForestInventories/IFN/Products/Coordinates/IFN2_spain_best_ETRS89H30.gpkg"))
soillist_ifn2 <- soillist_mod[ifn2_best_sf$ID_IFN3]
names(soillist_ifn2) <- ifn2_best_sf$ID_IFN2
sum(sapply(soillist_ifn2, is.null))
saveRDS(soillist_ifn2, "Products/IFN2/Spain/IFN2_spain_best_soilgrids_mod_ETRS89H30.rds")

#### IFN4 - Spain ####
soillist <- readRDS("Products/IFN4/Spain/IFN4_spain_best_soilgrids_ETRS89H30.rds")
soildepth <- sf::read_sf("Products/IFN4/Spain/IFN4_spain_best_soildepth_ETRS89H30.gpkg")
plotData <- IFNread::readPCParcela(source_path4, ifn = 4, ccaa = ccaaIFN4, prov = provIFN4)
plotData <- plotData[,c("ID", "Rocosid")]
df <- soildepth |> dplyr::left_join(plotData, by="ID")
soillist_mod <- modify_soils(soillist, df)
saveRDS(soillist_mod, "Products/IFN4/Spain/IFN4_spain_best_soilgrids_mod_ETRS89H30.rds")

