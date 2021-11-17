# Add Probe sequence version (There are different sequences of rotation pattern) 
# 
# Author: Taisei Sugiyama

add_prb_ver <- function (df){
  
  ## Load packages
  library(dplyr)
  library(purrr)
  
  subs = as.numeric(unique(df$sub_id))
  source("function/set_exp_param.R")
  
  prb_ver_list = vector("list", length(subs))
  csub = 0
  
  for (soi in subs){
    csub = csub + 1
    
    # Convert to string with prefix identifier
    if (soi < 10){
      sid_str = sprintf("S0%d",soi)
    } else {
      sid_str = sprintf("S%d",soi) 
    }
    
    data_fpath = sprintf("%s/%s",data_dir,sid_str)
    
    # Get Probe sequence version
    prb_ver_list[[csub]] = read.csv(sprintf("%s/data_sand_prb.csv",data_fpath)) %>%
      group_by(blk) %>%
      dplyr::filter(row_number() == 1) %>% # only one data per block is needed 
      ungroup %>%
      mutate(sub_id = soi) %>%
      dplyr::select(sub_id, blk,prb_ver) 

  }
  
  prb_ver_df = reduce(prb_ver_list,rbind)
  
  df_edit = df %>%
    left_join(prb_ver_df, by=c("sub_id","blk"))
  
}