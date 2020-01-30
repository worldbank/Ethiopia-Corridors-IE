# Analysis: Coefficient Each Year - Figures

# Makes figures based on dataframe of results

p_dodge_width <- .5

# Load Data --------------------------------------------------------------------
results_df <- readRDS(file.path(finaldata_file_path, DATASET_TYPE, "results", "results_coef_each_year.Rds"))

# Facet over region type -------------------------------------------------------
for(dv in c("globcover_urban", "dmspols_ihs", "dmspols_zhang_ihs", "globcover_cropland", "ndvi",
            "dmspols_zhang_2", "dmspols_zhang_6")){
  for(addis_distance in c("All", "Far")){
    for(phase in c("phase_all",  "phase_1", "phase_2", "phase_3", "phase_4")){
      for(ntl_group in c("All", "1", "2", "3")){
        
        print(paste(dv, addis_distance, phase))
        
        if(dv == "globcover_urban")    dv_title <- "Globcover: Urban"
        if(dv == "globcover_cropland") dv_title <- "Globcover: Cropland"
        if(dv == "ndvi")               dv_title <- "NDVI"
        if(dv == "dmspols_ihs")        dv_title <- "NTL - DMSPOLS (Log)"
        if(dv == "dmspols_zhang_ihs")  dv_title <- "NTL - DMSPOLS (Log)"
        if(dv == "dmspols_zhang_2")    dv_title <- "DMSPOLS >= 2"
        if(dv == "dmspols_zhang_6")    dv_title <- "DMSPOLS >= 6"
        
        p <- ggplot(data = results_df[(results_df$dv %in% dv) & 
                                        (results_df$addis_distance %in% addis_distance) & 
                                        (results_df$phase %in% phase) & 
                                        (results_df$ntl_group %in% ntl_group),], 
                    aes(x=years_since_improved, y=b, ymin=p025, ymax=p975,
                        group = var, color = var)) + 
          geom_vline(xintercept=0,size=3,alpha=0.15) +
          geom_hline(yintercept=0,size=1,alpha=0.15) +
          geom_point(position = position_dodge(width = p_dodge_width),size=1.5) + 
          geom_linerange(position = position_dodge(width = p_dodge_width),size=1) +
          labs(x="Years Since Improved Road Constructed",
               y="Coefficient",
               color="Road\nType",
               title = dv_title) +
          theme_minimal() +
          theme(plot.title = element_text(face="bold", hjust=.5)) +
          facet_wrap(~region, scales="fixed", nrow=1)
        ggsave(p, filename = file.path(figures_file_path, paste0("regressions_eachyear_regionfacet_",dv,"_addis",addis_distance,"_",phase,"_ntl",ntl_group,".png")),
               height = 3.5, width =11)
        
      }
    }
  }
}

# Facet over ntl group (excluding all) -----------------------------------------
for(dv in c("globcover_urban", "dmspols_ihs", "dmspols_zhang_ihs", "globcover_cropland", "ndvi",
            "dmspols_zhang_2", "dmspols_zhang_6")){
  for(addis_distance in c("All", "Far")){
    for(phase in c("phase_all",  "phase_1", "phase_2", "phase_3", "phase_4")){
      for(region in c("All", "Sparse", "Dense")){ 
        
        print(paste(dv, addis_distance, phase))
        
        if(dv == "globcover_urban")    dv_title <- "Globcover: Urban"
        if(dv == "globcover_cropland") dv_title <- "Globcover: Cropland"
        if(dv == "ndvi")               dv_title <- "NDVI"
        if(dv == "dmspols_ihs")        dv_title <- "NTL - DMSPOLS (Log)"
        if(dv == "dmspols_zhang_ihs")  dv_title <- "NTL - DMSPOLS (Log)"
        if(dv == "dmspols_zhang_2")    dv_title <- "DMSPOLS >= 2"
        if(dv == "dmspols_zhang_6")    dv_title <- "DMSPOLS >= 6"
        
        results_df$ntl_group[results_df$ntl_group %in% "1"] <- "Zero"
        results_df$ntl_group[results_df$ntl_group %in% "2"] <- "Below Median"
        results_df$ntl_group[results_df$ntl_group %in% "3"] <- "Above Median"
        
        p <- ggplot(data = results_df[(results_df$ntl_group %in% c("Zero", "Below Median", "Above Median")) &
                                        (results_df$dv %in% dv) & 
                                        (results_df$addis_distance %in% addis_distance) & 
                                        (results_df$phase %in% phase) & 
                                        (results_df$region %in% region),], 
                    aes(x=years_since_improved, y=b, ymin=p025, ymax=p975,
                        group = var, color = var)) + 
          geom_vline(xintercept=0,size=3,alpha=0.15) +
          geom_hline(yintercept=0,size=1,alpha=0.15) +
          geom_point(position = position_dodge(width = p_dodge_width),size=1.5) + 
          geom_linerange(position = position_dodge(width = p_dodge_width),size=1) +
          labs(x="Years Since Improved Road Constructed",
               y="Coefficient",
               color="Road\nType",
               title = dv_title) +
          theme_minimal() +
          theme(plot.title = element_text(face="bold", hjust=.5)) +
          facet_wrap(~ntl_group, scales="fixed", nrow=1)
        ggsave(p, filename = file.path(figures_file_path, paste0("regressions_eachyear_ntlfacet_",dv,"_addis",addis_distance,"_",phase,"_region",region,".png")),
               height = 3.5, width =11)
        
      }
    }
  }
}



