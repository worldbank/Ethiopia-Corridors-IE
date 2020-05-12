# Analysis: Coefficient Each Year - Figures

# Makes figures based on dataframe of results

p_dodge_width <- .5
dv_list <- c("viirs_mean", "dmspols_zhang_6", "globcover_urban", "globcover_cropland", "ndvi", "ndvi_cropland")
dv_list <- c("viirs_mean", "viirs_mean_above1", "viirs_median", "globcover_urban", "globcover_cropland", "ndvi", "ndvi_cropland")

# Load Data --------------------------------------------------------------------
results_df <- readRDS(file.path(finaldata_file_path, DATASET_TYPE, "results", "results_coef_each_yearviirs_time.Rds"))
results_df <- results_df[!(results_df$variable %in% c("temp_avg", "precipitation")),]

if(DATASET_TYPE %in% "woreda_panel_hdx_csa"){
  unit <- "_woreda"
} else{
  unit <- ""
}

# Facet over region type -------------------------------------------------------
#for(addis_distance in c("All", "Far")){
#  for(phase in c("phase_all",  "phase_1", "phase_2", "phase_3", "phase_4")){
#    for(ntl_group in c("All", "1", "2", "3")){

for(addis_distance in c("All", "Far")){
  for(phase in c("phase_all")){
    for(ntl_group in c("All")){    
      
      # Skip some that don't need results for
      if((phase != "phase_all")  & (ntl_group != "All")) next
      if((addis_distance == "Far") & (phase != "phase_all")) next
      if((addis_distance == "Far") & (ntl_group != "All")) next
      
      print(paste(ntl_group, addis_distance, phase))

      figures_list <- list()
      i <- 1
      for(dv in dv_list){
        
        if(dv == "globcover_urban")    dv_title <- "Globcover: Urban"
        if(dv == "globcover_cropland") dv_title <- "Globcover: Cropland"
        if(dv == "ndvi")               dv_title <- "NDVI"
        if(dv == "dmspols_ihs")        dv_title <- "NTL - DMSPOLS (Log)"
        if(dv == "dmspols_zhang_ihs")  dv_title <- "NTL - DMSPOLS (Log)"
        if(dv == "dmspols_zhang_2")    dv_title <- "DMSPOLS >= 2"
        if(dv == "dmspols_zhang_6")    dv_title <- "DMSPOLS >= 6"
        if(dv == "ndvi")               dv_title <- "NDVI"
        if(dv == "ndvi_cropland")      dv_title <- "NDVI: Cropland Areas"
        
        if(dv == "viirs_mean")      dv_title <- "VIIRS: Mean"
        if(dv == "viirs_median")      dv_title <- "VIIRS: Median"
        if(dv == "viirs_max")      dv_title <- "VIIRS: Max"
        if(dv == "viirs_mean_above1")      dv_title <- "VIIRS: Mean Above 1"
        
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
          labs(x="",
               y="Coefficient",
               color="Road\nType",
               title = dv_title) +
          theme_minimal() +
          theme(plot.title = element_text(face="bold", hjust=.5)) +
          facet_wrap(~region, scales="free", nrow=1)
        
        if(i == length(dv_list)){
          p <- p + labs(x="Years Since Improved Road Constructed")
        }
        
        figures_list[[i]] <- p
        
        i <- i + 1
      }
      
      p_all <- ggarrange(figures_list[[1]],
                figures_list[[2]],
                figures_list[[3]],
                figures_list[[4]],
                figures_list[[5]],
                figures_list[[6]],
                figures_list[[7]],
                nrow = 7,
                common.legend = T,
                legend = "right")

      ggsave(p_all, filename = file.path(figures_file_path, paste0("regressions_eachyear_regionfacet_addis",addis_distance,"_",phase,"_ntl",ntl_group,unit,"_viirs_time.png")),
             height = 14, width =11)
      
    }
  }
}


# Facet over ntl group (excluding all) -----------------------------------------
#for(addis_distance in c("All", "Far")){
#  for(phase in c("phase_all",  "phase_1", "phase_2", "phase_3", "phase_4")){
#    for(region in c("All", "Sparse", "Dense")){ 
      
for(addis_distance in c("All", "Far")){
  for(phase in c("phase_all")){
    for(region in c("All")){ 
      
      # Skip some that don't need results for
      if((phase != "phase_all")  & (region != "All")) next
      if((addis_distance == "Far") & (phase != "phase_all")) next
      if((addis_distance == "Far") & (region != "All")) next
      
      print(paste(region, addis_distance, phase))

      results_df$ntl_group[results_df$ntl_group %in% "1"] <- "Zero"
      results_df$ntl_group[results_df$ntl_group %in% "2"] <- "Below Median"
      results_df$ntl_group[results_df$ntl_group %in% "3"] <- "Above Median"
      
      results_df$ntl_group <- results_df$ntl_group %>% factor(levels = c("All",
                                                                         "Zero",
                                                                         "Below Median",
                                                                         "Above Median"))
      
      
      figures_list <- list()
      i <- 1
      for(dv in dv_list){
        if(dv == "globcover_urban")    dv_title <- "Globcover: Urban"
        if(dv == "globcover_cropland") dv_title <- "Globcover: Cropland"
        if(dv == "ndvi")               dv_title <- "NDVI"
        if(dv == "dmspols_ihs")        dv_title <- "NTL - DMSPOLS (Log)"
        if(dv == "dmspols_zhang_ihs")  dv_title <- "NTL - DMSPOLS (Log)"
        if(dv == "dmspols_zhang_2")    dv_title <- "DMSPOLS >= 2"
        if(dv == "dmspols_zhang_6")    dv_title <- "DMSPOLS >= 6"
        if(dv == "ndvi")               dv_title <- "NDVI"
        if(dv == "ndvi_cropland")      dv_title <- "NDVI: Cropland Areas"
        if(dv == "viirs_mean")         dv_title <- "VIIRS: Mean"
        if(dv == "viirs_median")       dv_title <- "VIIRS: Median"
        if(dv == "viirs_max")          dv_title <- "VIIRS: Max"
        if(dv == "viirs_mean_above1")  dv_title <- "VIIRS: Mean Above 1"
        
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
          labs(x="",
               y="Coefficient",
               color="Road\nType",
               title = dv_title) +
          theme_minimal() +
          theme(plot.title = element_text(face="bold", hjust=.5)) +
          facet_wrap(~ntl_group, scales="free", nrow=1)
        
        
        if(i == length(dv_list)){
          p <- p + labs(x="Years Since Improved Road Constructed")
        }
        
        figures_list[[i]] <- p
        
        i <- i + 1
        
      }
      
      p_all <- ggarrange(figures_list[[1]],
                         figures_list[[2]],
                         figures_list[[3]],
                         figures_list[[4]],
                         figures_list[[5]],
                         figures_list[[6]],
                         figures_list[[7]],
                         nrow = 7,
                         common.legend = T,
                         legend = "right")
      
      ggsave(p_all, filename = file.path(figures_file_path, paste0("regressions_eachyear_ntlfacet_addis",addis_distance,"_",phase,"_region",region,unit,"_viirs_time.png")),
             height = 14, width =11)
      
    }
  }
}




