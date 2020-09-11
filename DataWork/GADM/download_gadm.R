# Download GADM Datasets

setwd(file.path(rawdata_file_path, "GADM"))
for(i in 0:3) getData('GADM', country='ETH', level=i)
