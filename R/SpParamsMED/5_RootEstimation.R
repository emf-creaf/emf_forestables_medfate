#########################################################
# Estimates parameters Z50 and Z95 for roots in IFN plots
#########################################################

library(medfateutils)
data(SpParamsMED)


sf_climate <- sf::read_sf("ifn3_cat_climate_1976_2020.gpkg")

ifn <- "IFN3"
region <- "Catalunya"
region_small <- "cat"
huse <- "H31"
final_sf <- readRDS(paste0("Products/", ifn,"/", region,"/", ifn,"_", region_small,"_final_ETRS89", huse,".rds"))

f <- final_sf$forest[[1]]
s <- final_sf$soil[[1]]

a<- medfateutils::estimateRootingDepthList(final_sf$forest, 
                                       final_sf$soil, 
                                       sf_climate$MAPETsummer, 
                                       sf_climate$MAPsummer, SpParamsMED)

## Calculate summer climate predictors for all plots
years = 1995:2005
historical = "/media/miquel/Climate/Climate/Products/IFNplots/Catalunya/Historical/"

IFN3_SPT = readRDS("Products/Topography/IFN23_spt_cat_unique_ETRS89H31.rds")
df_pr_summer23 = data.frame(matrix(NA, nrow=length(IFN3_SPT), ncol=length(years)))
names(df_pr_summer23) = years
row.names(df_pr_summer23) = row.names(IFN3_SPT@data)
df_pet_summer23 = df_pr_summer23
for(i in 1:length(years)) {
  year = years[i]
  file_pr = paste0(historical, "Monthly/Precipitation/ifn23_cat_precipitation_monthly_",year,".rds")
  if(file.exists(file_pr)){
    pr = readRDS(file_pr)
    df_pr_summer23[,i] = rowSums(pr@data[,6:8])
  } else {
    stop(paste0("File not found"))
    
  }
  file_pet = paste0(historical, "Monthly/PET/ifn23_cat_pet_monthly_",year,".rds")
  if(file.exists(file_pet)){
    pet = readRDS(file_pet)
    df_pet_summer23[,i] = rowSums(pet@data[,6:8])
  } else {
    stop(paste0("File not found"))
  }
}

IFN4_SPT = readRDS("Products/Topography/IFN4_spt_cat_ETRS89H31.rds")
df_pr_summer4 = data.frame(matrix(NA, nrow=length(IFN4_SPT), ncol=length(years)))
names(df_pr_summer4) = years
row.names(df_pr_summer4) = row.names(IFN4_SPT@data)
df_pet_summer4 = df_pr_summer4
#Copy values for plots existing in IFN3
id34 = row.names(df_pr_summer4)[row.names(df_pr_summer4) %in% row.names(df_pr_summer23)]
df_pr_summer4[id34,] = df_pr_summer23[id34, ]
df_pet_summer4[id34,] = df_pet_summer23[id34, ]
for(i in 1:length(years)) {
  year = years[i]
  file_pr = paste0(historical, "Monthly/Precipitation/ifn4new_cat_precipitation_monthly_",year,".rds")
  if(file.exists(file_pr)){
    pr = readRDS(file_pr)
    df_pr_summer4[row.names(pr@data),i] = rowSums(pr@data[,6:8])
  } else {
    stop(paste0("File not found"))
    
  }
  file_pet = paste0(historical, "Monthly/PET/ifn4new_cat_pet_monthly_",year,".rds")
  if(file.exists(file_pet)){
    pet = readRDS(file_pet)
    df_pet_summer4[row.names(pet@data),i] = rowSums(pet@data[,6:8])
  } else {
    stop(paste0("File not found"))
  }
}


# Average values across years
df_pr_summer23$Mean = rowMeans(df_pr_summer23)
df_pet_summer23$Mean = rowMeans(df_pet_summer23)
df_pr_summer4$Mean = rowMeans(df_pr_summer4)
df_pet_summer4$Mean = rowMeans(df_pet_summer4)


#Load ACDC predictors
# climSPDF = readRDS("../Climate/Sources/ACDC/Rdata/climSPDF.rds")


#Not all plots have coordinates (10459)
# spdf_cat_ifn3_etrs89_unique = readRDS("Products/Coordinates/IFN23_cat_ETRS89H31_unique.rds")
# cc = as.data.frame(spdf_cat_ifn3_etrs89_unique@coords)
# row.names(cc) = as.character(as.numeric(spdf_cat_ifn3_etrs89_unique$ID))
# ids=names(IFN2_forestlist)
# sum(ids %in% row.names(cc))
# ids = ids[ids %in% row.names(cc)]
# cc = cc[ids,]
# 
# ifn_gridindex = getGridIndex(cc, climSPDF@grid)
# ifn_Psummer = climSPDF@data[as.character(ifn_gridindex),"Prec_summer"]
# ifn_Psummer[is.na(ifn_Psummer)] = mean(ifn_Psummer, na.rm = T)
# 
# ifn_PETsummer = climSPDF@data[as.character(ifn_gridindex),"PET_summer"]
# ifn_PETsummer[is.na(ifn_PETsummer)] = mean(ifn_PETsummer, na.rm = T)

#### IFN 2 ####
soillist_cat_ifn23_unique_mod = readRDS("Products/Soils/soillist_cat_ifn23_unique_mod.rds")
IFN2_forestlist = readRDS(file="Products/IFN2/Rdata/forestlist_IFN2_Catalunya.rds")
IFN2_forestlist_roots = IFN2_forestlist
ids = names(IFN2_forestlist)
nplots = length(ids)
# pb = txtProgressBar(0, nplots, style = 3)
for(p in 1:nplots) {
  # setTxtProgressBar(pb, p)
  id = ids[p]
  forest = IFN2_forestlist[[id]]
  soildata = soillist_cat_ifn23_unique_mod[[id]]
  ifn_Psummer = df_pr_summer23[id, "Mean"]
  ifn_PETsummer = df_pet_summer23[id, "Mean"]
  if((!is.na(ifn_Psummer)) && (!is.na(ifn_PETsummer)) && (!is.null(soildata))) {
    cat(".")
    IFN2_forestlist_roots[[id]] = medfateutils::estimateRootingDepth(forest, soildata, ifn_PETsummer, ifn_Psummer,
                                                                     SpParams = SpParamsMED, fillMissing = TRUE)
  } else {
    cat("n")
  }
}  
saveRDS(IFN2_forestlist_roots, file="Products/IFN2/Rdata/forestlist_roots_IFN2_Catalunya.rds")

#### IFN 3 ####
soillist_cat_ifn23_unique_mod = readRDS("Products/Soils/soillist_cat_ifn23_unique_mod.rds")
IFN3_forestlist = readRDS(file="Products/IFN3/Rdata/forestlist_IFN3_Catalunya.rds")
IFN3_forestlist_roots = IFN3_forestlist
ids = names(IFN3_forestlist)
nplots = length(ids)
# pb = txtProgressBar(0, nplots, style = 3)
for(p in 1:nplots) {
  # setTxtProgressBar(pb, p)
  id = ids[p]
  forest = IFN3_forestlist[[id]]
  soildata = soillist_cat_ifn23_unique_mod[[id]]
  ifn_Psummer = df_pr_summer23[id, "Mean"]
  ifn_PETsummer = df_pet_summer23[id, "Mean"]
  if((!is.na(ifn_Psummer)) && (!is.na(ifn_PETsummer)) && (!is.null(soildata))) {
    cat(".")
    IFN3_forestlist_roots[[id]] = estimateRootingDepth(forest, soildata, ifn_PETsummer, ifn_Psummer,
                                                       SpParams = SpParamsMED, fillMissing = TRUE)
  } else {
    cat("n")
  }
}  
saveRDS(IFN3_forestlist_roots, file="Products/IFN3/Rdata/forestlist_roots_IFN3_Catalunya.rds")


#### IFN 4 ####
soillist_cat_ifn4_mod = readRDS("Products/Soils/soillist_cat_ifn4_mod.rds")
IFN4_forestlist = readRDS(file="Products/IFN4/Rdata/forestlist_IFN4_Catalunya.rds")
IFN4_forestlist_roots = IFN4_forestlist
ids = names(IFN4_forestlist)
nplots = length(ids)
for(p in 1:nplots) {
  # setTxtProgressBar(pb, p)
  id = ids[p]
  forest = IFN4_forestlist[[id]]
  soildata = soillist_cat_ifn4_mod[[id]]
  ifn_Psummer = df_pr_summer4[id, "Mean"]
  ifn_PETsummer = df_pet_summer4[id, "Mean"]
  if((!is.na(ifn_Psummer)) && (!is.na(ifn_PETsummer)) && (!is.null(soildata))) {
    cat(".")
    IFN4_forestlist_roots[[id]] = estimateRootingDepth(forest, soildata, ifn_PETsummer, ifn_Psummer,
                                                       SpParams = SpParamsMED, fillMissing = TRUE)
  } else {
    cat("n")
  }
}  
saveRDS(IFN4_forestlist_roots, file="Products/IFN4/Rdata/forestlist_roots_IFN4_Catalunya.rds")

