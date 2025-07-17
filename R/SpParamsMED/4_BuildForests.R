#################################################
# Creates medfate forest lists from IFN rds
#################################################
library(medfateutils)

data(SpParamsMED)
data(IFN_species_mapping)
IFN_path <- "~/OneDrive/EMF_datasets/ForestInventories/IFN/"

# IFN2 Catalunya --------------------------------------------------------------------
piesMayoresIFN2 <- tibble::as_tibble(read.table(file=paste0(IFN_path, "Products/IFN2/piesMayoresDataIFN2_Catalunya.csv"), 
                                                sep="\t", header=TRUE, colClasses = c("character", "character", 
                                                                                      "character", "character", "character",
                                                                                      "numeric", "numeric", "numeric", "numeric")))
piesMenoresIFN2 <- tibble::as_tibble(read.table(file=paste0(IFN_path, "Products/IFN2/piesMenoresDataIFN2_Catalunya.csv"),
                                                sep="\t", header=TRUE, colClasses = c("character", "character", "character", 
                                                                                      "character", "integer", "integer")))
matorralIFN2 <- tibble::as_tibble(read.table(file=paste0(IFN_path, "Products/IFN2/shrubDataIFN2_Catalunya.csv"),
                                             sep="\t", header=TRUE, colClasses = c("character", "character", "character", 
                                                                                   "character", "numeric", "numeric")))
IFN2_forestlist <- IFN2forest(pies_mayores = piesMayoresIFN2, 
                              pies_menores = piesMenoresIFN2,
                              matorral = matorralIFN2, 
                              IFN_species_mapping = IFN_species_mapping,
                              SpParams = SpParamsMED,
                              setDefaults = TRUE, 
                              filterWrongRecords = TRUE, filterDeadTrees = TRUE)
saveRDS(IFN2_forestlist, file="Products/IFN2/Catalunya/IFN2_cat_forestlist.rds")
IFN2_forestlist <- IFN2forest(pies_mayores = piesMayoresIFN2, 
                              pies_menores = piesMenoresIFN2,
                              matorral = matorralIFN2, 
                              IFN_species_mapping = IFN_species_mapping,
                              SpParams = SpParamsMED,
                              setDefaults = TRUE, 
                              filterWrongRecords = FALSE, filterDeadTrees = FALSE, minDBH = NULL)
saveRDS(IFN2_forestlist, file="Products/IFN2/Catalunya/IFN2_cat_forestlist_allrecords.rds")


# IFN2 Spain --------------------------------------------------------------------
piesMayoresIFN2 <- tibble::as_tibble(read.table(file=paste0(IFN_path, "Products/IFN2/piesMayoresDataIFN2_Spain.csv"), 
                                                sep="\t", header=TRUE, colClasses = c("character", "character", 
                                                                                      "character", "character", "character",
                                                                                      "numeric", "numeric", "numeric", "numeric")))
piesMenoresIFN2 <- tibble::as_tibble(read.table(file=paste0(IFN_path, "Products/IFN2/piesMenoresDataIFN2_Spain.csv"),
                                                sep="\t", header=TRUE, colClasses = c("character", "character", "character", 
                                                                                      "character", "integer", "integer")))
matorralIFN2 <- tibble::as_tibble(read.table(file=paste0(IFN_path, "Products/IFN2/shrubDataIFN2_Spain.csv"),
                                             sep="\t", header=TRUE, colClasses = c("character", "character", "character", 
                                                                                   "character", "numeric", "numeric")))
IFN2_forestlist <- IFN2forest(pies_mayores = piesMayoresIFN2, 
                              pies_menores = piesMenoresIFN2,
                              matorral = matorralIFN2, 
                              IFN_species_mapping = IFN_species_mapping,
                              SpParams = SpParamsMED,
                              setDefaults = TRUE, 
                              filterWrongRecords = TRUE, filterDeadTrees = TRUE)
saveRDS(IFN2_forestlist, file="Products/IFN2/Spain/IFN2_spain_forestlist.rds")
IFN2_forestlist <- IFN2forest(pies_mayores = piesMayoresIFN2, 
                              pies_menores = piesMenoresIFN2,
                              matorral = matorralIFN2, 
                              IFN_species_mapping = IFN_species_mapping,
                              SpParams = SpParamsMED,
                              setDefaults = TRUE, 
                              filterWrongRecords = FALSE, filterDeadTrees = FALSE, minDBH = NULL)
saveRDS(IFN2_forestlist, file="Products/IFN2/Spain/IFN2_spain_forestlist_allrecords.rds")

# IFN3 Catalunya --------------------------------------------------------------------
mc <- read.table(paste0(IFN_path,"Sources/IFN3/ModelCombustible.txt"), sep="\t", header=T, colClasses = "character")
herbData <- mc[,c("IdParcela","IdClasse", "HerbaciFcc","HerbaciHm")]
herbData$ID <- paste0(mc$IdParcela, "_", mc$IdClasse)
herbData$HerbaciFcc <- as.numeric(herbData$HerbaciFcc)
herbData$HerbaciHm <- as.numeric(herbData$HerbaciHm)*10
herbData <- herbData[,c("ID", "HerbaciFcc","HerbaciHm")]
names(herbData) <- c("ID", "Cover", "Height")

piesMayoresIFN3 <- tibble::as_tibble(read.table(file=paste0(IFN_path, "Products/IFN3/treeDataIFN3_Catalunya.csv"),
                                                sep="\t", header=TRUE, colClasses = "character"))
regeneraIFN3 <- tibble::as_tibble(read.table(file=paste0(IFN_path, "Products/IFN3/regDataIFN3_Catalunya.csv"),
                                             sep="\t", header=TRUE, colClasses = "character"))
matorralIFN3 <- tibble::as_tibble(read.table(file=paste0(IFN_path, "Products/IFN3/shrubDataIFN3_Catalunya.csv"),
                                             sep="\t", header=TRUE, colClasses = "character"))

IFN3_forestlist <- IFN2forest(pies_mayores = piesMayoresIFN3, 
                              regenera = regeneraIFN3,
                              matorral = matorralIFN3,
                              herb_data = herbData,
                              IFN_species_mapping = IFN_species_mapping,
                              SpParams = SpParamsMED,
                              setDefaults = TRUE, filterWrongRecords = TRUE)
saveRDS(IFN3_forestlist, file="Products/IFN3/Catalunya/IFN3_cat_forestlist.rds")
IFN3_forestlist <- IFN2forest(pies_mayores = piesMayoresIFN3, 
                              regenera = regeneraIFN3,
                              matorral = matorralIFN3,
                              herb_data = herbData,
                              IFN_species_mapping = IFN_species_mapping,
                              SpParams = SpParamsMED,
                              setDefaults = TRUE, filterWrongRecords = FALSE,
                              filterDeadTrees = FALSE, minDBH = NULL)
saveRDS(IFN3_forestlist, file="Products/IFN3/Catalunya/IFN3_cat_forestlist_allrecords.rds")

# IFN3 Spain --------------------------------------------------------------------
piesMayoresIFN3 <- tibble::as_tibble(read.table(file=paste0(IFN_path, "Products/IFN3/treeDataIFN3_Spain.csv"),
                                                sep="\t", header=TRUE, colClasses = "character"))
regeneraIFN3 <- tibble::as_tibble(read.table(file=paste0(IFN_path, "Products/IFN3/regDataIFN3_Spain.csv"),
                                             sep="\t", header=TRUE, colClasses = "character"))
matorralIFN3 <- tibble::as_tibble(read.table(file=paste0(IFN_path, "Products/IFN3/shrubDataIFN3_Spain.csv"),
                                             sep="\t", header=TRUE, colClasses = "character"))
IFN3_forestlist <- IFN2forest(pies_mayores = piesMayoresIFN3, 
                              regenera = regeneraIFN3,
                              matorral = matorralIFN3,
                              IFN_species_mapping = IFN_species_mapping,
                              SpParams = SpParamsMED,
                              setDefaults = TRUE, filterWrongRecords = TRUE)
saveRDS(IFN3_forestlist, file="Products/IFN3/Spain/IFN3_spain_forestlist.rds")
IFN3_forestlist <- IFN2forest(pies_mayores = piesMayoresIFN3, 
                              regenera = regeneraIFN3,
                              matorral = matorralIFN3,
                              IFN_species_mapping = IFN_species_mapping,
                              SpParams = SpParamsMED,
                              setDefaults = TRUE, filterWrongRecords = FALSE,
                              filterDeadTrees = FALSE, minDBH = NULL)
saveRDS(IFN3_forestlist, file="Products/IFN3/Spain/IFN3_spain_forestlist_allrecords.rds")




# IFN4 Catalunya --------------------------------------------------------------------

piesMayoresIFN4 <- tibble::as_tibble(read.table(file=paste0(IFN_path, "Products/IFN4/treeDataIFN4_Catalunya.csv"),
                                                sep="\t", header=TRUE, colClasses = "character"))
regeneraIFN4 <- tibble::as_tibble(read.table(file=paste0(IFN_path, "Products/IFN4/regDataIFN4_Catalunya.csv"),
                                             sep="\t", header=TRUE, colClasses = "character"))
matorralIFN4 <- tibble::as_tibble(read.table(file=paste0(IFN_path, "Products/IFN4/shrubDataIFN4_Catalunya.csv"),
                                             sep="\t", header=TRUE, colClasses = "character"))
IFN4_forestlist <- IFN2forest(pies_mayores = piesMayoresIFN4, 
                              regenera = regeneraIFN4,
                              matorral = matorralIFN4,
                              IFN_species_mapping = IFN_species_mapping,
                              SpParams = SpParamsMED,
                              setDefaults = TRUE, filterWrongRecords = TRUE)
saveRDS(IFN4_forestlist, file="Products/IFN4/Catalunya/IFN4_cat_forestlist.rds")
IFN4_forestlist <- IFN2forest(pies_mayores = piesMayoresIFN4, 
                              regenera = regeneraIFN4,
                              matorral = matorralIFN4,
                              IFN_species_mapping = IFN_species_mapping,
                              SpParams = SpParamsMED,
                              setDefaults = TRUE, filterWrongRecords = FALSE,
                              filterDeadTrees = FALSE, minDBH = NULL)
saveRDS(IFN4_forestlist, file="Products/IFN4/Catalunya/IFN4_cat_forestlist_allrecords.rds")

# IFN4 Spain --------------------------------------------------------------------
piesMayoresIFN4 <- tibble::as_tibble(read.table(file=paste0(IFN_path, "Products/IFN4/treeDataIFN4_Spain.csv"),
                                                sep="\t", header=TRUE, colClasses = "character"))
regeneraIFN4 <- tibble::as_tibble(read.table(file=paste0(IFN_path, "Products/IFN4/regDataIFN4_Spain.csv"),
                                             sep="\t", header=TRUE, colClasses = "character"))
matorralIFN4 <- tibble::as_tibble(read.table(file=paste0(IFN_path, "Products/IFN4/shrubDataIFN4_Spain.csv"),
                                             sep="\t", header=TRUE, colClasses = "character"))
IFN4_forestlist <- IFN2forest(pies_mayores = piesMayoresIFN4, 
                              regenera = regeneraIFN4,
                              matorral = matorralIFN4,
                              IFN_species_mapping = IFN_species_mapping,
                              SpParams = SpParamsMED,
                              setDefaults = TRUE, filterWrongRecords = TRUE)
saveRDS(IFN4_forestlist, file="Products/IFN4/Spain/IFN4_spain_forestlist.rds")
IFN4_forestlist <- IFN2forest(pies_mayores = piesMayoresIFN4, 
                              regenera = regeneraIFN4,
                              matorral = matorralIFN4,
                              IFN_species_mapping = IFN_species_mapping,
                              SpParams = SpParamsMED,
                              setDefaults = TRUE, filterWrongRecords = FALSE,
                              filterDeadTrees = FALSE, minDBH = NULL)
saveRDS(IFN4_forestlist, file="Products/IFN4/Spain/IFN4_spain_forestlist_allrecords.rds")
