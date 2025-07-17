library(meteoland)

dataset_path = "~/OneDrive/EMF_datasets/"

#### Catalonia ####

#Load MDT for Catalonia
elevation <- terra::rast(paste0(dataset_path,"Topography/Products/Catalunya/MET30m_ETRS89_UTM31_ICGC.tif"))
slope <- terra::rast(paste0(dataset_path,"Topography/Products/Catalunya/MET30m_ETRS89_UTM31_ICGC_slope_degrees.tif"))
aspect <- terra::rast(paste0(dataset_path,"Topography/Products/Catalunya/MET30m_ETRS89_UTM31_ICGC_aspect_degrees.tif"))

for(ifn in c("IFN2", "IFN3", "IFN4")) {
  best_sf <- sf::read_sf(paste0(dataset_path, "ForestInventories/IFN/Products/Coordinates/", ifn,"_cat_best_ETRS89H31.gpkg")) 
  best_vect <- terra::vect(best_sf |> sf::st_transform(terra::crs(elevation)))
  best_sf$elevation <- terra::extract(elevation, best_vect)[,2]
  best_sf$slope <- terra::extract(slope, best_vect)[,2]
  best_sf$aspect <- terra::extract(aspect, best_vect)[,2]
  sf::write_sf(best_sf, paste0("Products/", ifn,"/Catalunya/", ifn,"_cat_best_topo_ETRS89H31.gpkg"))
}

#### Spain ####
#Load MDT for Spain
elevation <- terra::rast(paste0(dataset_path,"Topography/Products/Spain/Spain_elevation_100m_H30_ED50.tif"))
slope <- terra::rast(paste0(dataset_path,"Topography/Products/Spain/Spain_slope_100m_H30_ED50.tif"))
aspect <- terra::rast(paste0(dataset_path,"Topography/Products/Spain/Spain_aspect_100m_H30_ED50.tif"))

for(ifn in c("IFN2", "IFN3", "IFN4")) {
  best_sf <- sf::read_sf(paste0(dataset_path, "ForestInventories/IFN/Products/Coordinates/", ifn,"_spain_best_ETRS89H30.gpkg")) 
  best_vect <- terra::vect(best_sf |> sf::st_transform(terra::crs(elevation)))
  best_sf$elevation <- terra::extract(elevation, best_vect)$layer
  best_sf$slope <- terra::extract(slope, best_vect)$layer
  best_sf$aspect <- terra::extract(aspect, best_vect)$layer
  sf::write_sf(best_sf, paste0("Products/", ifn,"/Spain/", ifn,"_spain_best_topo_ETRS89H30.gpkg"))
}
