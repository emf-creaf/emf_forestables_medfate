#################################################
# Estimates FCCS fuel characteristics on IFN
#################################################

library(medfate)
data("SpParamsMED")


#### IFN2 (Catalonia) ####
#Plot data
pcParcelaIFN2 = readRDS("Products/IFN2/Rdata/plotDataIFN2_Catalunya.rds")
IFN2_forestlist = readRDS("Products/IFN2/Rdata/forestlist_IFN2_Catalunya.rds")

#Cover values
pcParcelaIFN2  = pcParcelaIFN2[names(IFN2_forestlist),]
pcParcelaIFN2$FRACCION1 = as.numeric(as.character(pcParcelaIFN2$FRACCION1))
pcParcelaIFN2$FRACCION2 = as.numeric(as.character(pcParcelaIFN2$FRACCION2))
pcParcelaIFN2$FRACCION1[is.na(pcParcelaIFN2$FRACCION1)] = pcParcelaIFN2$FRACCION2[is.na(pcParcelaIFN2$FRACCION1)]
pcParcelaIFN2$FRACCION3 = pmax(0, pcParcelaIFN2$FRACCION1 - pcParcelaIFN2$FRACCION2)



IFN2_fs = data.frame(matrix(NA, nrow=length(IFN2_forestlist), ncol=55))
WAF = rep(NA, length(IFN2_forestlist))
names(WAF) <-names(IFN2_forestlist)
names(IFN2_fs)<-paste(c("ca","sh","he","wo","li"),
                      c(rep("w",5),rep("cover",5), rep("hbc",5), rep("htc",5), rep("delta",5),rep("sigma",5), rep("h",5),
                        rep("etabetarel",5), rep("minFMC",5), rep("maxFMC",5), rep("pDead",5)), sep=".")
rownames(IFN2_fs)<-names(IFN2_forestlist)
pb = txtProgressBar(1, length(IFN2_forestlist), style=3)
for(i in 1:length(IFN2_forestlist)) {
  setTxtProgressBar(pb, i)
  f = IFN2_forestlist[[i]]
  f$treeData = f$treeData[f$treeData$Height>0, ,drop=FALSE]
  f$shrubData = f$shrubData[f$shrubData$Height>0, ,drop=FALSE]
  if(length(f$herbCover)==0) f$herbCover = NA
  if(length(f$herbHeight)==0) f$herbHeight = NA
  a = fuel_FCCS(f, ShrubCover = pcParcelaIFN2$FRACCION3[i], CanopyCover=pcParcelaIFN2$FRACCION2[i], 
                SpParams= SpParamsMED, bulkDensityThreshold = 0.04, depthMode = "absoluteprofile")
  IFN2_fs[i,1:5]  = a$w
  IFN2_fs[i,6:10]  = a$cover
  IFN2_fs[i,11:15]  = a$hbc
  IFN2_fs[i,16:20]  = a$htc
  IFN2_fs[i,21:25]  = a$delta
  IFN2_fs[i,26:30]  = a$sigma
  IFN2_fs[i,31:35]  = a$h
  IFN2_fs[i,36:40]  = a$etabetarel
  IFN2_fs[i,41:45]  = a$MinFMC
  IFN2_fs[i,46:50]  = a$MaxFMC
  IFN2_fs[i,51:55]  = a$pDead
  WAF[i] = fuel_windAdjustmentFactor(a$htc[2], a$hbc[1], a$htc[1],a$cover[1])
}
IFN2_fs$ca.gap = IFN2_fs$ca.hbc-IFN2_fs$sh.htc
IFN2_fs$ca.gap[is.na(IFN2_fs$ca.hbc)] = 15
IFN2_fs$ca.w[is.na(IFN2_fs$ca.w)] = 0.0
IFN2_fs$sh.w[is.na(IFN2_fs$sh.w)] = 0.0
IFN2_fs$he.w[is.na(IFN2_fs$he.w)] = 0.0
IFN2_fs$wo.w[is.na(IFN2_fs$wo.w)] = 0.0
IFN2_fs$li.w[is.na(IFN2_fs$li.w)] = 0.0
IFN2_fs$w = IFN2_fs$ca.w+IFN2_fs$sh.w+IFN2_fs$he.w+IFN2_fs$wo.w+IFN2_fs$li.w
IFN2_fs$WAF = WAF


#Bulk density
IFN2_fs$bd.sh = IFN2_fs$sh.w/IFN2_fs$sh.delta
IFN2_fs$bd.sh[IFN2_fs$sh.delta==0] = 0
IFN2_fs$bd.ca = IFN2_fs$ca.w/IFN2_fs$ca.delta
IFN2_fs$bd.ca[IFN2_fs$ca.delta==0] = 0
saveRDS(IFN2_fs, file="Products/IFN2/Rdata/IFN2_fccs_Catalunya.rds")
IFN2_fs = readRDS( file="Products/IFN2/Rdata/IFN2_fccs_Catalunya.rds")

#### IFN3 (Catalonia) ####
#Plot data
pcParcelaIFN3 = readRDS("Products/IFN3/Rdata/plotDataIFN3_Catalunya.rds")
IFN3_forestlist = readRDS("Products/IFN3/Rdata/forestlist_IFN3_Catalunya.rds")

#Cover values
pcParcelaIFN3  = pcParcelaIFN3[names(IFN3_forestlist),]
pcParcelaIFN3$FCCTOT[is.na(pcParcelaIFN3$FCCTOT)] = pcParcelaIFN3$FCCARB[is.na(pcParcelaIFN3$FCCTOT)]
pcParcelaIFN3$FCCSHR = pmax(0, pcParcelaIFN3$FCCTOT - pcParcelaIFN3$FCCARB)



IFN3_fs = data.frame(matrix(NA, nrow=length(IFN3_forestlist), ncol=55))
WAF = rep(NA, length(IFN3_forestlist))
names(WAF) <-names(IFN3_forestlist)
names(IFN3_fs)<-paste(c("ca","sh","he","wo","li"),
                      c(rep("w",5),rep("cover",5), rep("hbc",5), rep("htc",5), rep("delta",5),
                        rep("sigma",5), rep("h",5), rep("etabetarel",5), rep("minFMC",5), rep("maxFMC",5), rep("pDead",5)), sep=".")
rownames(IFN3_fs)<-names(IFN3_forestlist)
pb = txtProgressBar(1, length(IFN3_forestlist), style=3)
for(i in 1:length(IFN3_forestlist)) {
  setTxtProgressBar(pb, i)
  f = IFN3_forestlist[[i]]
  f$treeData = f$treeData[!is.na(f$treeData$DBH), ,drop=FALSE ]
  f$treeData = f$treeData[!is.na(f$treeData$N), ,drop=FALSE ]
  f$treeData = f$treeData[!is.na(f$treeData$Height), ,drop=FALSE ]
  f$treeData = f$treeData[f$treeData$Height>0, ,drop=FALSE ]
  f$shrubData = f$shrubData[!is.na(f$shrubData$Height), ,drop=FALSE ]
  f$shrubData = f$shrubData[f$shrubData$Height>0, ,drop=FALSE]
  if(length(f$herbCover)==0) f$herbCover = NA
  if(length(f$herbHeight)==0) f$herbHeight = NA
  a = fuel_FCCS(f, ShrubCover = pcParcelaIFN3$FCCSHR[i], CanopyCover=pcParcelaIFN3$FCCARB[i], 
                SpParams= SpParamsMED, bulkDensityThreshold = 0.04, depthMode = "absoluteprofile")
  IFN3_fs[i,1:5]  = a$w
  IFN3_fs[i,6:10]  = a$cover
  IFN3_fs[i,11:15]  = a$hbc
  IFN3_fs[i,16:20]  = a$htc
  IFN3_fs[i,21:25]  = a$delta
  IFN3_fs[i,26:30]  = a$sigma
  IFN3_fs[i,31:35]  = a$h
  IFN3_fs[i,36:40]  = a$etabetarel
  IFN3_fs[i,41:45]  = a$MinFMC
  IFN3_fs[i,46:50]  = a$MaxFMC
  IFN3_fs[i,51:55]  = a$pDead
  WAF[i] = fuel_windAdjustmentFactor(a$htc[2], a$hbc[1], a$htc[1],a$cover[1])
}
IFN3_fs$ca.gap = IFN3_fs$ca.hbc-IFN3_fs$sh.htc
IFN3_fs$ca.gap[is.na(IFN3_fs$ca.hbc)] = 15
IFN3_fs$ca.w[is.na(IFN3_fs$ca.w)] = 0.0
IFN3_fs$sh.w[is.na(IFN3_fs$sh.w)] = 0.0
IFN3_fs$he.w[is.na(IFN3_fs$he.w)] = 0.0
IFN3_fs$wo.w[is.na(IFN3_fs$wo.w)] = 0.0
IFN3_fs$li.w[is.na(IFN3_fs$li.w)] = 0.0
IFN3_fs$w = IFN3_fs$ca.w+IFN3_fs$sh.w+IFN3_fs$he.w+IFN3_fs$wo.w+IFN3_fs$li.w
IFN3_fs$WAF = WAF


#Bulk density
IFN3_fs$bd.sh = IFN3_fs$sh.w/IFN3_fs$sh.delta
IFN3_fs$bd.sh[IFN3_fs$sh.delta==0] = 0
IFN3_fs$bd.ca = IFN3_fs$ca.w/IFN3_fs$ca.delta
IFN3_fs$bd.ca[IFN3_fs$ca.delta==0] = 0
saveRDS(IFN3_fs, file="Products/IFN3/Rdata/IFN3_fccs_Catalunya.rds")


#### IFN4 (Catalonia) ####
#Plot data
pcParcelaIFN4 = readRDS("Products/IFN4/Rdata/plotDataIFN4_Catalunya.rds")
IFN4_forestlist = readRDS("Products/IFN4/Rdata/forestlist_IFN4_Catalunya.rds")

#Cover values
pcParcelaIFN4  = pcParcelaIFN4[names(IFN4_forestlist),]
pcParcelaIFN4$FccTot[is.na(pcParcelaIFN4$FccTot)] = pcParcelaIFN4$FccArb[is.na(pcParcelaIFN4$FccTot)]
pcParcelaIFN4$FCCShr = pmax(0, pcParcelaIFN4$FccTot - pcParcelaIFN4$FccArb)



IFN4_fs = data.frame(matrix(NA, nrow=length(IFN4_forestlist), ncol=55))
WAF = rep(NA, length(IFN4_forestlist))
names(WAF) <-names(IFN4_forestlist)
names(IFN4_fs)<-paste(c("ca","sh","he","wo","li"),
                      c(rep("w",5),rep("cover",5), rep("hbc",5), rep("htc",5), rep("delta",5),
                        rep("sigma",5), rep("h",5), rep("etabetarel",5), rep("minFMC",5), rep("maxFMC",5), rep("pDead",5)), sep=".")
rownames(IFN4_fs)<-names(IFN4_forestlist)
pb = txtProgressBar(1, length(IFN4_forestlist), style=3)
for(i in 1:length(IFN4_forestlist)) {
  setTxtProgressBar(pb, i)
  f = IFN4_forestlist[[i]]
  f$treeData = f$treeData[f$treeData$Height>0, ,drop=FALSE]
  f$shrubData = f$shrubData[f$shrubData$Height>0, ,drop=FALSE]
  if(length(f$herbCover)==0) f$herbCover = NA
  if(length(f$herbHeight)==0) f$herbHeight = NA
  a = fuel_FCCS(f, ShrubCover = pcParcelaIFN4$FCCShr[i], CanopyCover=pcParcelaIFN4$FccArb[i], 
                SpParams= SpParamsMED, bulkDensityThreshold = 0.04, depthMode = "absoluteprofile")
  IFN4_fs[i,1:5]  = a$w
  IFN4_fs[i,6:10]  = a$cover
  IFN4_fs[i,11:15]  = a$hbc
  IFN4_fs[i,16:20]  = a$htc
  IFN4_fs[i,21:25]  = a$delta
  IFN4_fs[i,26:30]  = a$sigma
  IFN4_fs[i,31:35]  = a$h
  IFN4_fs[i,36:40]  = a$etabetarel
  IFN4_fs[i,41:45]  = a$MinFMC
  IFN4_fs[i,46:50]  = a$MaxFMC
  IFN4_fs[i,51:55]  = a$pDead
  WAF[i] = fuel_windAdjustmentFactor(a$htc[2], a$hbc[1], a$htc[1],a$cover[1])
}
IFN4_fs$ca.gap = IFN4_fs$ca.hbc-IFN4_fs$sh.htc
IFN4_fs$ca.gap[is.na(IFN4_fs$ca.hbc)] = 15
IFN4_fs$ca.w[is.na(IFN4_fs$ca.w)] = 0.0
IFN4_fs$sh.w[is.na(IFN4_fs$sh.w)] = 0.0
IFN4_fs$he.w[is.na(IFN4_fs$he.w)] = 0.0
IFN4_fs$wo.w[is.na(IFN4_fs$wo.w)] = 0.0
IFN4_fs$li.w[is.na(IFN4_fs$li.w)] = 0.0
IFN4_fs$w = IFN4_fs$ca.w+IFN4_fs$sh.w+IFN4_fs$he.w+IFN4_fs$wo.w+IFN4_fs$li.w
IFN4_fs$WAF = WAF


#Bulk density
IFN4_fs$bd.sh = IFN4_fs$sh.w/IFN4_fs$sh.delta
IFN4_fs$bd.sh[IFN4_fs$sh.delta==0] = 0
IFN4_fs$bd.ca = IFN4_fs$ca.w/IFN4_fs$ca.delta
IFN4_fs$bd.ca[IFN4_fs$ca.delta==0] = 0
saveRDS(IFN4_fs, file="Products/IFN4/Rdata/IFN4_fccs_Catalunya.rds")



#### Merge results in XLS (Catalonia) ####
library(openxlsx)
wb = createWorkbook()
addWorksheet(wb, "IFN2_fs")
writeDataTable(wb, "IFN2_fs", IFN2_fs, rowNames = T)
addWorksheet(wb, "IFN3_fs")
writeDataTable(wb, "IFN3_fs", IFN3_fs, rowNames = T)
addWorksheet(wb, "IFN4_fs")
writeDataTable(wb, "IFN4_fs", IFN4_fs, rowNames = T)
saveWorkbook(wb, "Products/IFN_fccs_catalonia.xlsx", overwrite = T)
