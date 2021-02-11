adj_iv_var_names <- function(lm){
  # Adjust IV variable names to match with OLS versions
  
  rownames(lm$coefficients) <- rownames(lm$coefficients) %>% 
    str_replace_all("\\(fit\\)|\\`", "") 
  
  rownames(lm$beta) <- rownames(lm$beta) %>% 
    str_replace_all("\\(fit\\)|\\`", "") 
  
  return(lm)
}

adj_ols_var_names <- function(lm){
  # Adjust IV variable names to match with OLS versions
  
  rownames(lm$coefficients) <- rownames(lm$coefficients) %>% 
    str_replace_all("MA_var_exc", "MA_var")
  
  rownames(lm$beta) <- rownames(lm$beta) %>% 
    str_replace_all("MA_var_exc", "MA_var")
  
  return(lm)
}