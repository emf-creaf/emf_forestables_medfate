# Test with medfateland ---------------------------------------------------
library(medfateland)
data(SpParamsMED)
data(examplemeteo)

control <- defaultControl("Granier")
# control <- defaultControl("Sperry")

# medfateland -------------------------------------------------------------


regions <- c("Catalunya", "Spain")
regions_small <- c("cat", "spain")
huses <- c("H31", "H30")
nsampleplots <- 1000
for(ir in 1:2) {
  region <- regions[ir]
  region_small <- regions_small[ir]
  huse <- huses[ir]
  for(ifn in c("IFN2", "IFN3", "IFN4")) {
    cat(paste0("\n Region: ", region, " IFN: ", ifn, "\n"))
    final_sf <- readRDS(paste0("Products/", ifn,"/",region,"/",ifn,"_",region_small,"_final_ETRS89", huse,".rds"))
    sf_red <- final_sf[sample(1:nrow(final_sf), nsampleplots),]
    res_day <- spwb_spatial_day(sf_red, SpParams = SpParamsMED, meteo=examplemeteo, date = as.Date("2001-02-01"),
                                local_control = control)
  }
}

res <- spwb_spatial(final_sf[final_sf$ID=="070468_A1",], SpParams = SpParamsMED, meteo=examplemeteo,
                            local_control = control)
