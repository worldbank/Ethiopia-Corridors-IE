# Travel Time

SCALE_BY_AREA <- TRUE

for(SEP_ROAD_SHAPEFILES in c(TRUE, FALSE)){
  for(SCALE_BY_AREA in c(FALSE, TRUE)){
    
    # Load Data --------------------------------------------------------------------
    #### Full Woredas - For polygon neighbors
    if(DATASET_TYPE %in% "woreda_panel_hdx_csa_nearroad"){
      polygons <- readRDS(file.path(finaldata_file_path, DATASET_TYPE, "individual_datasets", "points_cropped_only.Rds"))
    } else{
      polygons <- readRDS(file.path(finaldata_file_path, DATASET_TYPE, "individual_datasets", "points_all.Rds"))
    }
    
    polygons <- spTransform(polygons, CRS(UTM_ETH))
    
    #### Woreda PolygonArea
    polygons_area <- readRDS(file.path(finaldata_file_path, DATASET_TYPE, "individual_datasets", "points_area.Rds"))
    
    #### Road Length
    if(SEP_ROAD_SHAPEFILES %in% T){
      woreda_rdlngth <- readRDS(file.path(finaldata_file_path, DATASET_TYPE, "individual_datasets", "points_roadlength_km_rdsep.Rds"))
    } else{
      woreda_rdlngth <- readRDS(file.path(finaldata_file_path, DATASET_TYPE, "individual_datasets", "points_roadlength_km.Rds"))
    }

    woreda_rdlngth <- merge(woreda_rdlngth, polygons_area, by = "uid", all.x=T, all.y=F)
    
    ## Vectors for (1) road length var and (2) road speed numbers
    road_length_vars <- names(woreda_rdlngth)[grepl("road_length_", names(woreda_rdlngth))] 
    
    road_lengths <- road_length_vars %>% 
      str_replace_all("road_length_", "") %>% 
      as.numeric() %>% 
      sort()
    road_lengths <- road_lengths[road_lengths > 0]
    
    # Prep Road Length Variables ---------------------------------------------------
    ## Replace NAs with 0 road length
    for(var in road_length_vars){
      woreda_rdlngth[[var]][is.na(woreda_rdlngth[[var]])] <- 0
    }
    
    ## Create road_length_[speed]over: length of road of speed and above
    for(speed_i in road_lengths){
      print(speed_i)
      road_lengths_speedi_over <- road_lengths[road_lengths >= speed_i]
      
      woreda_rdlngth[[paste0("road_length_", speed_i, "over")]] <- 
        apply(woreda_rdlngth %>%
                dplyr::select(paste0("road_length_",road_lengths_speedi_over)), 
              1, 
              FUN = sum)
    }
    
    ## Density variable: sum of speed * length
    woreda_rdlngth$road_length_X_speed <- woreda_rdlngth$road_length_10 * 10 + 
      woreda_rdlngth$road_length_15 * 15 + 
      woreda_rdlngth$road_length_20 * 20 + 
      woreda_rdlngth$road_length_25 * 25 +
      woreda_rdlngth$road_length_30 * 30 +
      woreda_rdlngth$road_length_35 * 35 +
      woreda_rdlngth$road_length_45 * 45 + 
      woreda_rdlngth$road_length_50 * 50 + 
      woreda_rdlngth$road_length_70 * 70 + 
      woreda_rdlngth$road_length_120 * 120 
    
    # Scale by Area ----------------------------------------------------------------
    if(SCALE_BY_AREA){
      #polygons$area <- gArea(polygons, byid=T) / 1000^2
      #woreda_rdlngth <- merge(woreda_rdlngth, polygons@data, by="uid")
      
      ## Save version
      # Will have area and non area, to merge in later
      woreda_rdlngth_area <- woreda_rdlngth
      
      road_vars <- names(woreda_rdlngth)[grepl("road_", names(woreda_rdlngth))]
      for(var in road_vars){
        woreda_rdlngth[[var]] <- woreda_rdlngth[[var]] / woreda_rdlngth$area_polygon
      }
      
      for(var in road_vars){
        woreda_rdlngth_area[[paste0(var, "_area")]] <- woreda_rdlngth_area[[var]] / woreda_rdlngth_area$area_polygon
      }
    }
    
    
    # Indicators Around Neighbors --------------------------------------------------
    poly_neighbor <- poly2nb(polygons)
    
    neighbor_stats <- function(i, polygons, poly_neighbor){
      
      if((i %% 10) == 0) print(i)
      
      polygons_neigh_withi <- polygons[c(i, poly_neighbor[[i]]),]
      polygons_neigh <- polygons[c(poly_neighbor[[i]]),]
      
      woreda_rdlngth_neigh_withi <- woreda_rdlngth %>%
        filter(uid %in% polygons_neigh_withi$uid) %>%
        group_by(year) %>%
        summarise(road_length_X_speed_neigh_withi = mean(road_length_X_speed),
                  road_length_10over_neigh_withi = mean(road_length_10over),
                  road_length_15over_neigh_withi = mean(road_length_15over),
                  road_length_20over_neigh_withi = mean(road_length_20over),
                  road_length_25over_neigh_withi = mean(road_length_25over),
                  road_length_30over_neigh_withi = mean(road_length_30over),
                  road_length_35over_neigh_withi = mean(road_length_35over),
                  road_length_45over_neigh_withi = mean(road_length_45over),
                  road_length_50over_neigh_withi = mean(road_length_50over),
                  road_length_70over_neigh_withi = mean(road_length_70over),
                  road_length_120over_neigh_withi = mean(road_length_120over)) %>%
        mutate(uid = polygons$uid[i])
      
      woreda_rdlngth_neigh <- woreda_rdlngth %>%
        filter(uid %in% polygons_neigh$uid) %>%
        group_by(year) %>%
        summarise(road_length_X_speed_neigh = mean(road_length_X_speed),
                  road_length_10over_neigh = mean(road_length_10over),
                  road_length_15over_neigh = mean(road_length_15over),
                  road_length_20over_neigh = mean(road_length_20over),
                  road_length_25over_neigh = mean(road_length_25over),
                  road_length_30over_neigh = mean(road_length_30over),
                  road_length_35over_neigh = mean(road_length_35over),
                  road_length_45over_neigh = mean(road_length_45over),
                  road_length_50over_neigh = mean(road_length_50over),
                  road_length_70over_neigh = mean(road_length_70over),
                  road_length_120over_neigh = mean(road_length_120over)) %>%
        mutate(uid = polygons$uid[i])
      
      out_df <- merge(woreda_rdlngth_neigh, 
                      woreda_rdlngth_neigh_withi, 
                      by=c("uid", "year"))
      
      return(out_df)
    }
    
    
    neighbor_stat_df <- lapply(1:nrow(polygons), neighbor_stats,
                               polygons,
                               poly_neighbor) %>%
      bind_rows()
    
    # Renmae variables if use by area ----------------------------------------------
    if(SCALE_BY_AREA){
      names(neighbor_stat_df)[!(names(neighbor_stat_df) %in% c("uid", "year"))] <- 
        paste0(names(neighbor_stat_df)[!(names(neighbor_stat_df) %in% c("uid", "year"))], "_area")
      
      # Merge in non-neighbor vars
      woreda_rdlngth_area$area_polygon <- NULL
      
      woreda_rdlngth_area <- woreda_rdlngth_area %>%
        select(matches("uid|year|over|_area|road_length_X_speed"))
      
      neighbor_stat_df <- merge(neighbor_stat_df, 
                                woreda_rdlngth_area,
                                by = c("uid", "year"))
      
    }
    
    # Export -----------------------------------------------------------------------
    if(SEP_ROAD_SHAPEFILES){
      out_add <- "_rdsep"
    } else{
      out_add <- ""
    }
    
    saveRDS(neighbor_stat_df, file.path(finaldata_file_path, DATASET_TYPE, "individual_datasets", paste0("road_accessibility_scaleArea",SCALE_BY_AREA,out_add,".Rds")))
    
  }
}

