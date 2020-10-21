# Commonly used functions across project

lm_confint_tidy <- function(lm, years_since_variable){
  lm_confint <- confint(lm) %>% 
    as.data.frame
  names(lm_confint) <- c("p025", "p975")
  lm_confint$b <- (lm_confint$p025 + lm_confint$p975)/2
  lm_confint$variable <- row.names(lm_confint)
  
  lm_confint <- lm_confint[!grepl("cluster_id)|year)|Intercept)", lm_confint$variable),]
  lm_confint$years_since_improved <- gsub(years_since_variable, "", lm_confint$variable) %>% as.numeric
  
  return(lm_confint)
}

lm_post_confint_tidy <- function(lm){
  
  lm_confint <- confint(lm) %>% 
    as.data.frame
  names(lm_confint) <- c("p025", "p975")
  lm_confint$b <- (lm_confint$p025 + lm_confint$p975)/2
  lm_confint$variable <- row.names(lm_confint)
  
  lm_confint$tvalue <- summary(lm)$coefficients[,3] %>% as.vector()
  lm_confint$pvalue <- summary(lm)$coefficients[,4] %>% as.vector()
  
  return(lm_confint)
}

pause_gc <- function(GRID_DATASET){
  if(GRID_DATASET){
    Sys.sleep(1)
    gc()
    Sys.sleep(1)
    gc()
    Sys.sleep(1)
    gc()
    Sys.sleep(1)
  } 
  return(NULL)
}