# Clean Woreda Data 

## Load data
woreda <- readOGR(dsn = file.path(project_file_path, "Data", "Woreda Population", "RawData"),
                  layer = "Ethioworeda")

## Add woreda id
woreda$woreda_id <- 1:nrow(woreda)

## Export
saveRDS(woreda, file.path(project_file_path, "Data", "Woreda Population", "FinalData", "woreda.Rds"))
