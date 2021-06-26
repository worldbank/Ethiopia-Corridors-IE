# Download GADM Datasets

setwd(file.path(data_file_path, "GADM", "RawData"))
for(i in 0:3) getData('GADM', country='ETH', level=i)



