# Download GADM Datasets

a <- readOGR(file.path(data_file_path, "Electricity Network", "RawData", 
                       "Ethiopia Electricity Transmission Network.shp"))

crs(a)

leaflet() %>%
  addTiles() %>%
  addPolylines(data = a)
