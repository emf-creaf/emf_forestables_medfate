# Test with medfateland ---------------------------------------------------
library(medfateland)
library(ggplot2)
data(SpParamsMED)
data(examplemeteo)

control <- defaultControl("Granier")
# control <- defaultControl("Sperry")

regions <- c("Catalunya", "Spain")
regions_small <- c("cat", "spain")
huses <- c("H31", "H30")
nsampleplots <- 100
for(ir in 1:2) {
  region <- regions[ir]
  region_small <- regions_small[ir]
  huse <- huses[ir]
  for(ifn in c("IFN2", "IFN3", "IFN4")) {
    cat(paste0("\n Region: ", region, " IFN: ", ifn, "\n"))
    sf <- readRDS(paste0("Products/", ifn,"/",region,"/",ifn,"_",region_small,"_final_ETRS89", huse,".rds"))

    sf$BA_adults <- sapply(sf$forest, medfate::stand_basalArea)
    sf$BA_all <- sapply(sf$forest, medfate::stand_basalArea, minDBH = 0)
    sf$DominantSpecies <- sapply(sf$forest, medfate::stand_dominantTreeSpecies, SpParamsMED)
    pb <- txtProgressBar(1,nrow(sf), style = 3)
    sf$lai_adults <- rep(0, nrow(sf))
    sf$lai_sap <- rep(0, nrow(sf))
    sf$N_adults <- rep(0, nrow(sf))
    sf$N_sap <- rep(0, nrow(sf))
    sf$lai_shrubs <- rep(0, nrow(sf))
    for(i in 1:nrow(sf)) {
      if(i%%100 ==0) setTxtProgressBar(pb, i)
      nadults <- nrow(sf$forest[[i]]$treeData[sf$forest[[i]]$treeData$DBH>5.0,])
      nsap <- nrow(sf$forest[[i]]$treeData[sf$forest[[i]]$treeData$DBH<=5.0,])
      nshrubs <- nrow(sf$forest[[i]]$shrubData)
      lai_coh <- plant_LAI(sf$forest[[i]], SpParamsMED, bounded = TRUE)
      N_coh <- plant_density(sf$forest[[i]], SpParamsMED)
      if(nadults>0) {
        sf$lai_adults[i] <- sum(lai_coh[1:nadults],na.rm=TRUE)
        sf$N_adults[i] <- sum(N_coh[1:nadults],na.rm=TRUE)
      }
      if(nsap>0) {
        sf$lai_sap[i] <- sum(lai_coh[(nadults+1):(nadults+nsap)],na.rm=TRUE)
        sf$N_sap[i] <- sum(N_coh[(nadults+1):(nadults+nsap)],na.rm=TRUE)
      }
      if(nshrubs>0) {
        sf$lai_shrubs[i] <- sum(lai_coh[(nadults+nsap+1):(nadults+nsap+nshrubs)],na.rm=TRUE)
      }
    }
    cat("\n")
    
    sf$lai_trees <- sf$lai_sap + sf$lai_adults
    sf$lai_all <- sf$lai_trees + sf$lai_shrubs
    
    sf_plot <- sf[!is.na(sf$DominantSpecies),]
    tb_dom  <- table(sf_plot$DominantSpecies)
    tb_dom_perc  <- 100*tb_dom/sum(tb_dom)
    sp_dom <- names(tb_dom_perc[tb_dom_perc>0.5])
    sf_plot <- sf_plot[sf_plot$DominantSpecies %in% sp_dom, ]
    
    p1<- ggplot(sf_plot)+
      geom_point(aes(x=BA_adults, y = lai_adults), size = 0.1, alpha = 0.5)+
      geom_smooth(aes(x=BA_adults, y = lai_adults))+
      xlim(c(0,100))+ ylim(c(0,7))+
      theme_bw()+
      facet_wrap(vars( DominantSpecies))
    
    ggsave(paste0("Plots/", ifn,"_",region_small,"_test_lai_adults.png"),
           width = 8, height = 8, plot = p1)
    
    p2<- ggplot(sf_plot)+
      geom_point(aes(x=BA_all, y = lai_trees), size = 0.1, alpha = 0.5)+
      geom_smooth(aes(x=BA_all, y = lai_trees))+
      xlim(c(0,100))+ ylim(c(0,7))+
      theme_bw()+
      facet_wrap(vars( DominantSpecies))
    
    ggsave(paste0("Plots/", ifn,"_",region_small,"_test_lai_trees.png"),
           width = 8, height = 8, plot = p2)
    
    p3<- ggplot(sf_plot)+
      geom_point(aes(x=BA_all, y = lai_shrubs), size = 0.1, alpha = 0.5)+
      geom_smooth(aes(x=BA_all, y = lai_shrubs))+
      xlim(c(0,100))+ ylim(c(0,3))+
      theme_bw()+
      facet_wrap(vars( DominantSpecies))
    
    ggsave(paste0("Plots/", ifn,"_",region_small,"_test_lai_shrubs.png"),
           width = 8, height = 8, plot = p3)
  }
}
