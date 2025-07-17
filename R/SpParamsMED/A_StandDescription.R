#################################################
# Estimates Basal area and LAI
#################################################

library(medfateland)
library(openxlsx)
library(tidyverse)
data("SpParamsMED")



IFN_ingrowth_mortality_extraction<-function(sf_final, sf_initial, from_to = "IFN23") {
  sf_final<- sf_final[sf_final$IDCLASE=="A1",]
  forest_allrecords <- sf_final$forest_allrecords
  n_plots <- length(forest_allrecords)
  IFN_st <- data.frame(matrix(0, nrow=n_plots, ncol=8))
  names(IFN_st)<-c("N_ingrowth_1", "N_ingrowth_all", "BA_ingrowth_1", "BA_ingrowth_all", 
                   "N_mortality", "BA_mortality", "N_extraction", "BA_extraction")
  pb <- txtProgressBar(1, length(forest_allrecords), style=3)
  for(i in 1:length(forest_allrecords)) {
    setTxtProgressBar(pb, i)
    id <- sf_final$id[i]
    f <- forest_allrecords[[i]]
    td <- f$treeData
    if(nrow(td)>0){
      td$BA <- td$N*(td$DBH/200)^2*pi

      if(from_to=="IFN23") {
        td_ing <- td |> filter(!is.na(OrdenIf3), OrdenIf2=="000")
      } else {
        td_ing <- td |> filter(!is.na(OrdenIf4), OrdenIf3=="000")
      }
      td_ing_1 <- td_ing |> filter(DBH<12.5)
      IFN_st[i,"N_ingrowth_1"] <- sum(td_ing_1$N, na.rm=TRUE)
      IFN_st[i,"N_ingrowth_all"] <- sum(td_ing$N, na.rm=TRUE)
      IFN_st[i,"BA_ingrowth_1"] <- sum(td_ing_1$BA, na.rm=TRUE)
      IFN_st[i,"BA_ingrowth_all"] <- sum(td_ing$BA, na.rm=TRUE)
      
      if(from_to=="IFN23") {
        td_mort <- td |> filter(!is.na(OrdenIf3), OrdenIf3 %in% c("888", "999"), OrdenIf2!="000")
      } else {
        td_mort <- td |> filter(!is.na(OrdenIf4), OrdenIf4 %in% c("888", "999"), OrdenIf3!="000")
      }
      IFN_st[i,"N_mortality"] <- sum(td_mort$N, na.rm=TRUE)
      IFN_st[i,"BA_mortality"] <- sum(td_mort$BA, na.rm=TRUE)
      
      if(from_to=="IFN23") {
        td_extr <- td |> filter(!is.na(OrdenIf3), OrdenIf3=="000", OrdenIf2!="000")
      } else {
        td_extr <- td |> filter(!is.na(OrdenIf4), OrdenIf4=="000", OrdenIf3!="000")
      }
      if(nrow(td_extr)>0) {
        j <- which(sf_initial$id==id)
        if(length(j)==1) {
          td_initial <- sf_initial$forest[[j]]$treeData
          if(from_to=="IFN23") {
            td_extr_ini <- td_extr |> left_join(td_initial, by="OrdenIf2")
          } else {
            td_extr_ini <- td_extr |> left_join(td_initial, by="OrdenIf3")
          }
          td_extr_ini$BA <- td_extr_ini$N.y*(td_extr_ini$DBH.y/200)^2*pi
          IFN_st[i,"N_extraction"] <- sum(td_extr_ini$N.y, na.rm=TRUE)
          IFN_st[i,"BA_extraction"] <- sum(td_extr_ini$BA, na.rm=TRUE)
        }
      }
    }
  }
  return(data.frame(id = sf_final$id, IFN_st))
}

regions <- c("Catalunya", "Spain")
regions_small <- c("cat", "spain")
huses <- c("H31", "H30")
for(ir in 1:2) {
  region <- regions[ir]
  region_small <- regions_small[ir]
  huse <- huses[ir]
  
  sf_IFN2 <- readRDS(paste0("Products/IFN2/", region,"/IFN2_", region_small,"_final_ETRS89", huse,".rds"))
  IFN2_stats<-landscape_summary(sf_IFN2, "forest", summary.forest, SpParamsMED, progress=TRUE, unlist = TRUE)

  sf_IFN3 <- readRDS(paste0("Products/IFN3/", region,"/IFN3_", region_small,"_final_ETRS89", huse,".rds"))
  IFN3_stats <- landscape_summary(sf_IFN3, "forest", summary.forest, SpParamsMED, progress=TRUE, unlist = TRUE)

  sf_IFN4 <- readRDS(paste0("Products/IFN4/", region,"/IFN4_", region_small,"_final_ETRS89", huse,".rds"))
  IFN4_stats <- landscape_summary(sf_IFN4, "forest", summary.forest, SpParamsMED, progress=TRUE, unlist = TRUE)

  IFN23_ing_mort_extr <- IFN_ingrowth_mortality_extraction(sf_IFN3, sf_IFN2, from_to="IFN23")
  IFN34_ing_mort_extr <- IFN_ingrowth_mortality_extraction(sf_IFN4, sf_IFN3, from_to="IFN34")
  
  wb <- createWorkbook()
  addWorksheet(wb, "IFN2_stats")
  writeDataTable(wb, "IFN2_stats", sf::st_drop_geometry(IFN2_stats), rowNames = FALSE)
  addWorksheet(wb, "IFN3_stats")
  writeDataTable(wb, "IFN3_stats", sf::st_drop_geometry(IFN3_stats), rowNames = FALSE)
  addWorksheet(wb, "IFN4_stats")
  writeDataTable(wb, "IFN4_stats", sf::st_drop_geometry(IFN4_stats), rowNames = FALSE)
  addWorksheet(wb, "IFN23_ing_mort_extr")
  writeDataTable(wb, "IFN23_ing_mort_extr", IFN23_ing_mort_extr, rowNames = FALSE)
  addWorksheet(wb, "IFN34_ing_mort_extr")
  writeDataTable(wb, "IFN34_ing_mort_extr", IFN34_ing_mort_extr, rowNames = FALSE)
  saveWorkbook(wb,  paste0("Products/Statistics/IFN_stats_",region_small,".xlsx"), overwrite = T)
  
}
# 
# 
# IFN_LAI_sp<-function(forestlist) {
#   IFN_lai_sp = data.frame(matrix(0, nrow=length(forestlist), ncol=nrow(SpParamsMED)))
#   rownames(IFN_lai_sp)<-names(forestlist)
#   names(IFN_lai_sp)<-SpParamsMED$SpIndex
#   pb = txtProgressBar(1, length(forestlist), style=3)
#   for(i in 1:length(forestlist)) {
#     setTxtProgressBar(pb, i)
#     f = forestlist[[i]]
#     pl = plant_LAI(f, SpParamsMED)
#     sp = as.character(plant_species(f))
#     if(length(pl>0)) for(j in 1:length(pl)) IFN_lai_sp[i,sp[j]] = IFN_lai_sp[i,sp[j]] + pl[j]
#   }
#   return(IFN_lai_sp)
# }
# 
# IFN_treeBA_sp<-function(forestlist, minDBH = 5) {
#   IFN_ba_sp = data.frame(matrix(0, nrow=length(forestlist), ncol=nrow(SpParamsMED)))
#   rownames(IFN_ba_sp)<-names(forestlist)
#   names(IFN_ba_sp)<-SpParamsMED$SpIndex
#   pb = txtProgressBar(1, length(forestlist), style=3)
#   for(i in 1:length(forestlist)) {
#     setTxtProgressBar(pb, i)
#     f = forestlist[[i]]
#     f$shrubData = f$shrubData[numeric(0),]
#     pl = plant_basalArea(f)
#     pl[f$treeData$DBH<=minDBH] = 0
#     sp = as.character(plant_species(f))
#     if(length(pl>0)) for(j in 1:length(pl)) IFN_ba_sp[i,sp[j]] = IFN_ba_sp[i,sp[j]] + pl[j]
#   }
#   return(IFN_ba_sp)
# }
# 
# IFN_treeN_sp<-function(forestlist, minDBH = 7.5) {
#   IFN_n_sp = data.frame(matrix(0, nrow=length(forestlist), ncol=nrow(SpParamsMED)))
#   rownames(IFN_n_sp)<-names(forestlist)
#   names(IFN_n_sp)<-SpParamsMED$SpIndex
#   pb = txtProgressBar(1, length(forestlist), style=3)
#   for(i in 1:length(forestlist)) {
#     setTxtProgressBar(pb, i)
#     f = forestlist[[i]]
#     f$shrubData = f$shrubData[numeric(0),]
#     f$treeData = f$treeData[!is.na(f$treeData$N),]
#     f$treeData = f$treeData[!is.na(f$treeData$DBH),]
#     pl = f$treeData$N
#     pl[f$treeData$DBH<minDBH] = 0
#     sp = as.character(plant_species(f))
#     if(length(pl>0)) for(j in 1:length(pl)) IFN_n_sp[i,sp[j]] = IFN_n_sp[i,sp[j]] + pl[j]
#   }
#   return(IFN_n_sp)
# }
# 
# IFN_treeMeanDBH_sp<-function(forestlist, minDBH = 7.5) {
#   IFN_n_sp = data.frame(matrix(0, nrow=length(forestlist), ncol=nrow(SpParamsMED)))
#   rownames(IFN_n_sp)<-names(forestlist)
#   names(IFN_n_sp)<-SpParamsMED$SpIndex
#   IFN_mdbh_sp = data.frame(matrix(0, nrow=length(forestlist), ncol=nrow(SpParamsMED)))
#   rownames(IFN_mdbh_sp)<-names(forestlist)
#   names(IFN_mdbh_sp)<-SpParamsMED$SpIndex
#   pb = txtProgressBar(1, length(forestlist), style=3)
#   for(i in 1:length(forestlist)) {
#     setTxtProgressBar(pb, i)
#     f = forestlist[[i]]
#     f$shrubData = f$shrubData[numeric(0),]
#     f$treeData = f$treeData[!is.na(f$treeData$N),]
#     f$treeData = f$treeData[!is.na(f$treeData$DBH),]
#     f$treeData = f$treeData[f$treeData$DBH>=minDBH,]
#     n = f$treeData$N
#     ndbh = f$treeData$DBH*n
#     sp = as.character(plant_species(f))
#     if(length(n>0)) for(j in 1:length(n)) {
#       IFN_n_sp[i,sp[j]] = IFN_n_sp[i,sp[j]] + n[j]
#       IFN_mdbh_sp[i,sp[j]] = IFN_mdbh_sp[i,sp[j]] + ndbh[j]
#     } 
#   }
#   IFN_mdbh_sp = IFN_mdbh_sp/IFN_n_sp
#   IFN_mdbh_sp[is.na(IFN_mdbh_sp)] = NA
#   return(IFN_mdbh_sp)
# }
# 
# IFN_treesdDBH_sp<-function(forestlist, minDBH = 7.5) {
#   IFN_n_sp = data.frame(matrix(0, nrow=length(forestlist), ncol=nrow(SpParamsMED)))
#   rownames(IFN_n_sp)<-names(forestlist)
#   names(IFN_n_sp)<-SpParamsMED$SpIndex
#   IFN_mdbh_sp = data.frame(matrix(0, nrow=length(forestlist), ncol=nrow(SpParamsMED)))
#   rownames(IFN_mdbh_sp)<-names(forestlist)
#   names(IFN_mdbh_sp)<-SpParamsMED$SpIndex
#   IFN_sqdbh_sp = data.frame(matrix(0, nrow=length(forestlist), ncol=nrow(SpParamsMED)))
#   rownames(IFN_sqdbh_sp)<-names(forestlist)
#   names(IFN_sqdbh_sp)<-SpParamsMED$SpIndex
#   pb = txtProgressBar(1, length(forestlist), style=3)
#   for(i in 1:length(forestlist)) {
#     setTxtProgressBar(pb, i)
#     f = forestlist[[i]]
#     f$shrubData = f$shrubData[numeric(0),]
#     f$treeData = f$treeData[!is.na(f$treeData$N),]
#     f$treeData = f$treeData[!is.na(f$treeData$DBH),]
#     f$treeData = f$treeData[f$treeData$DBH>=minDBH,]
#     n = f$treeData$N
#     ndbh = f$treeData$DBH*n
#     sqdbh = f$treeData$DBH^2*n
#     sp = as.character(plant_species(f))
#     if(length(n>0)) for(j in 1:length(n)) {
#       IFN_n_sp[i,sp[j]] = IFN_n_sp[i,sp[j]] + n[j]
#       IFN_mdbh_sp[i,sp[j]] = IFN_mdbh_sp[i,sp[j]] + ndbh[j]
#       IFN_sqdbh_sp[i,sp[j]] = IFN_sqdbh_sp[i,sp[j]] + sqdbh[j]
#     } 
#   }
#   IFN_mdbh_sp = IFN_mdbh_sp/IFN_n_sp
#   IFN_mdbh_sp[is.na(IFN_mdbh_sp)] = NA
#   IFN_sqdbh_sp = IFN_sqdbh_sp/IFN_n_sp
#   IFN_sqdbh_sp[is.na(IFN_sqdbh_sp)] = NA
#   IFN_sqdbh_sp = sqrt(IFN_sqdbh_sp-(IFN_mdbh_sp^2))
#   IFN_sqdbh_sp[is.na(IFN_sqdbh_sp)] = NA
#   return(IFN_sqdbh_sp)
# }
# 
# IFN_meanheight_sp<-function(forestlist) {
#   IFN_height_sp = data.frame(matrix(NA, nrow=length(forestlist), ncol=nrow(SpParamsMED)))
#   rownames(IFN_height_sp)<-names(forestlist)
#   names(IFN_height_sp)<-SpParamsMED$SpIndex
#   pb = txtProgressBar(1, length(forestlist), style=3)
#   for(i in 1:length(forestlist)) {
#     setTxtProgressBar(pb, i)
#     f = forestlist[[i]]
#     ph = plant_height(f)
#     pl = plant_LAI(f, SpParamsMED)
#     sp = as.character(plant_species(f))
#     lai_sp = rep(0, length(unique(sp)))
#     names(lai_sp) = unique(sp)
#     if(length(ph>0)) {
#       for(j in 1:length(ph)) { #Weighed sum of heights with LAI weights
#         IFN_height_sp[i,sp[j]] = IFN_height_sp[i,sp[j]] + ph[j]*pl[j]
#         lai_sp[sp[j]] = lai_sp[sp[j]]+ pl[j]
#       }
#       for(j in 1:length(lai_sp)) { #Divide by species lay
#         n = names(lai_sp)[j]
#         IFN_height_sp[i,n] = IFN_height_sp[i,n]/lai_sp[j]
#       }
#     }
#   }
#   return(IFN_height_sp)
# }

#### IFN2 (Catalonia/Spain) ####

# IFN2_LAI_sp <- IFN_LAI_sp(IFN2_forestlist)
# IFN2_meanheight_sp <- IFN_meanheight_sp(IFN2_forestlist)
# IFN2_TreeBA_sp <- IFN_treeBA_sp(IFN2_forestlist)
# IFN2_TreeN_sp <- IFN_treeN_sp(IFN2_forestlist)
# IFN2_TreeMeanDBH_sp <- IFN_treeMeanDBH_sp(IFN2_forestlist)
# IFN2_TreesdDBH_sp <- IFN_treesdDBH_sp(IFN2_forestlist)
# write.table(IFN2_LAI_sp, paste0("Products/Statistics/IFN2/IFN2_LAI_sp_",target_eng,".txt"), sep="\t", quote=FALSE)
# write.table(IFN2_meanheight_sp, paste0("Products/Statistics/IFN2/IFN2_meanheight_sp_",target_eng,".txt"), sep="\t", quote=FALSE)
# write.table(IFN2_TreeBA_sp, paste0("Products/Statistics/IFN2/IFN2_TreeBA_sp_",target_eng,".txt"), sep="\t", quote=FALSE)
# write.table(IFN2_TreeN_sp, paste0("Products/Statistics/IFN2/IFN2_TreeN_sp_",target_eng,".txt"), sep="\t", quote=FALSE)
# write.table(IFN2_TreeMeanDBH_sp, paste0("Products/Statistics/IFN2/IFN2_TreeMeanDBH_sp_",target_eng,".txt"), sep="\t", quote=FALSE)
# write.table(IFN2_TreesdDBH_sp, paste0("Products/Statistics/IFN2/IFN2_TreesdDBH_sp_",target_eng,".txt"), sep="\t", quote=FALSE)


#### IFN3 (Catalonia/Spain) ####
# IFN3_LAI_sp <- IFN_LAI_sp(IFN3_forestlist)
# IFN3_meanheight_sp <- IFN_meanheight_sp(IFN3_forestlist)
# IFN3_TreeBA_sp <- IFN_treeBA_sp(IFN3_forestlist)
# IFN3_TreeN_sp <- IFN_treeN_sp(IFN3_forestlist)
# IFN3_TreeMeanDBH_sp <- IFN_treeMeanDBH_sp(IFN3_forestlist)
# IFN3_TreesdDBH_sp <- IFN_treesdDBH_sp(IFN3_forestlist)
# write.table(IFN3_LAI_sp, paste0("Products/Statistics/IFN3/IFN3_LAI_sp_",target_eng,".txt"), sep="\t", quote=FALSE)
# write.table(IFN3_meanheight_sp, paste0("Products/Statistics/IFN3/IFN3_meanheight_sp_",target_eng,".txt"), sep="\t", quote=FALSE)
# write.table(IFN3_TreeBA_sp, paste0("Products/Statistics/IFN3/IFN3_TreeBA_sp_",target_eng,".txt"), sep="\t", quote=FALSE)
# write.table(IFN3_TreeN_sp, paste0("Products/Statistics/IFN3/IFN3_TreeN_sp_",target_eng,".txt"), sep="\t", quote=FALSE)
# write.table(IFN3_TreeMeanDBH_sp, paste0("Products/Statistics/IFN3/IFN3_TreeMeanDBH_sp_",target_eng,".txt"), sep="\t", quote=FALSE)
# write.table(IFN3_TreesdDBH_sp, paste0("Products/Statistics/IFN3/IFN3_TreesdDBH_sp_",target_eng,".txt"), sep="\t", quote=FALSE)


#### IFN4 (Catalonia/Spain) ####
# IFN4_LAI_sp <- IFN_LAI_sp(IFN4_forestlist)
# IFN4_meanheight_sp <- IFN_meanheight_sp(IFN4_forestlist)
# IFN4_TreeBA_sp <- IFN_treeBA_sp(IFN4_forestlist)
# IFN4_TreeN_sp <- IFN_treeN_sp(IFN4_forestlist)
# IFN4_TreeMeanDBH_sp <- IFN_treeMeanDBH_sp(IFN4_forestlist)
# IFN4_TreesdDBH_sp <- IFN_treesdDBH_sp(IFN4_forestlist)
# write.table(IFN4_LAI_sp, paste0("Products/Statistics/IFN4/IFN4_LAI_sp_",target_eng,".txt"), sep="\t", quote=FALSE)
# write.table(IFN4_meanheight_sp, paste0("Products/Statistics/IFN4/IFN4_meanheight_sp_",target_eng,".txt"), sep="\t", quote=FALSE)
# write.table(IFN4_TreeBA_sp, paste0("Products/Statistics/IFN4/IFN4_TreeBA_sp_",target_eng,".txt"), sep="\t", quote=FALSE)
# write.table(IFN4_TreeN_sp, paste0("Products/Statistics/IFN4/IFN4_TreeN_sp_",target_eng,".txt"), sep="\t", quote=FALSE)
# write.table(IFN4_TreeMeanDBH_sp, paste0("Products/Statistics/IFN4/IFN4_TreeMeanDBH_sp_",target_eng,".txt"), sep="\t", quote=FALSE)
# write.table(IFN4_TreesdDBH_sp, paste0("Products/Statistics/IFN4/IFN4_TreesdDBH_sp_",target_eng,".txt"), sep="\t", quote=FALSE)


#### Merge results in XLS ####

# 
# wb <- createWorkbook()
# addWorksheet(wb, "IFN2_LAI_sp")
# writeDataTable(wb, "IFN2_LAI_sp", IFN2_LAI_sp, rowNames = T)
# addWorksheet(wb, "IFN3_LAI_sp")
# writeDataTable(wb, "IFN3_LAI_sp", IFN3_LAI_sp, rowNames = T)
# addWorksheet(wb, "IFN4_LAI_sp")
# writeDataTable(wb, "IFN4_LAI_sp", IFN4_LAI_sp, rowNames = T)
# addWorksheet(wb, "SpCodes")
# writeDataTable(wb, "SpCodes", SpParamsMED[,c("SpIndex","Name")], rowNames = F)
# saveWorkbook(wb,  paste0("Products/Statistics/IFN_LAI_sp_",target_eng,".xlsx"), overwrite = T)
# 
# wb <- createWorkbook()
# addWorksheet(wb, "IFN2_meanheight_sp")
# writeDataTable(wb, "IFN2_meanheight_sp", IFN2_meanheight_sp, rowNames = T)
# addWorksheet(wb, "IFN3_meanheight_sp")
# writeDataTable(wb, "IFN3_meanheight_sp", IFN3_meanheight_sp, rowNames = T)
# addWorksheet(wb, "IFN4_meanheight_sp")
# writeDataTable(wb, "IFN4_meanheight_sp", IFN4_meanheight_sp, rowNames = T)
# addWorksheet(wb, "SpCodes")
# writeDataTable(wb, "SpCodes", SpParamsMED[,c("SpIndex","Name")], rowNames = F)
# saveWorkbook(wb, paste0("Products/Statistics/IFN_meanheight_sp_",target_eng,".xlsx"), overwrite = T)
# 
# 
# wb <- createWorkbook()
# addWorksheet(wb, "IFN2_TreeBA_sp")
# writeDataTable(wb, "IFN2_TreeBA_sp", IFN2_TreeBA_sp, rowNames = T)
# addWorksheet(wb, "IFN3_TreeBA_sp")
# writeDataTable(wb, "IFN3_TreeBA_sp", IFN3_TreeBA_sp, rowNames = T)
# addWorksheet(wb, "IFN4_TreeBA_sp")
# writeDataTable(wb, "IFN4_TreeBA_sp", IFN4_TreeBA_sp, rowNames = T)
# addWorksheet(wb, "SpCodes")
# writeDataTable(wb, "SpCodes", SpParamsMED[,c("SpIndex","Name")], rowNames = F)
# saveWorkbook(wb, paste0("Products/Statistics/IFN_TreeBA_sp_",target_eng,".xlsx"), overwrite = T)
# 
# 
# wb <- createWorkbook()
# addWorksheet(wb, "IFN2_TreeN_sp")
# writeDataTable(wb, "IFN2_TreeN_sp", IFN2_TreeN_sp, rowNames = T)
# addWorksheet(wb, "IFN3_TreeN_sp")
# writeDataTable(wb, "IFN3_TreeN_sp", IFN3_TreeN_sp, rowNames = T)
# addWorksheet(wb, "IFN4_TreeN_sp")
# writeDataTable(wb, "IFN4_TreeN_sp", IFN4_TreeN_sp, rowNames = T)
# addWorksheet(wb, "SpCodes")
# writeDataTable(wb, "SpCodes", SpParamsMED[,c("SpIndex","Name")], rowNames = F)
# saveWorkbook(wb, paste0("Products/Statistics/IFN_TreeN_sp_",target_eng,".xlsx"), overwrite = T)
# 
# wb <- createWorkbook()
# addWorksheet(wb, "IFN2_TreeMeanDBH_sp")
# writeDataTable(wb, "IFN2_TreeMeanDBH_sp", IFN2_TreeMeanDBH_sp, rowNames = T)
# addWorksheet(wb, "IFN3_TreeMeanDBH_sp")
# writeDataTable(wb, "IFN3_TreeMeanDBH_sp", IFN3_TreeMeanDBH_sp, rowNames = T)
# addWorksheet(wb, "IFN4_TreeMeanDBH_sp")
# writeDataTable(wb, "IFN4_TreeMeanDBH_sp", IFN4_TreeMeanDBH_sp, rowNames = T)
# addWorksheet(wb, "SpCodes")
# writeDataTable(wb, "SpCodes", SpParamsMED[,c("SpIndex","Name")], rowNames = F)
# saveWorkbook(wb, paste0("Products/Statistics/IFN_TreeMeanDBH_sp_",target_eng,".xlsx"), overwrite = T)
# 
# wb <- createWorkbook()
# addWorksheet(wb, "IFN2_TreesdDBH_sp")
# writeDataTable(wb, "IFN2_TreesdDBH_sp", IFN2_TreesdDBH_sp, rowNames = T)
# addWorksheet(wb, "IFN3_TreesdDBH_sp")
# writeDataTable(wb, "IFN3_TreesdDBH_sp", IFN3_TreesdDBH_sp, rowNames = T)
# addWorksheet(wb, "IFN4_TreesdDBH_sp")
# writeDataTable(wb, "IFN4_TreesdDBH_sp", IFN4_TreesdDBH_sp, rowNames = T)
# addWorksheet(wb, "SpCodes")
# writeDataTable(wb, "SpCodes", SpParamsMED[,c("SpIndex","Name")], rowNames = F)
# saveWorkbook(wb, paste0("Products/Statistics/IFN_TreesdDBH_sp_",target_eng,".xlsx"), overwrite = T)
# 
