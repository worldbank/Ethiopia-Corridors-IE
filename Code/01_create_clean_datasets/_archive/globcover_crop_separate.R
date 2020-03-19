
# Load Data --------------------------------------------------------------------
eth_adm0 <- getData('GADM', country='ETH', level=0)

for(i in 1:24){
  print(i)
  globcover <- raster(file.path(rawdata_file_path, "esa_globcover", "scratch", "ESACCI-LC-L4-LCCS-Map-300m-P1Y-1992_2015-v2.0.7.tif"), i) %>% crop(eth_adm0)
  writeRaster(globcover, file.path(finaldata_file_path, "globcover", "cropped_to_ethiopia", paste0("globcover_", i+1991, ".tif")))
}
