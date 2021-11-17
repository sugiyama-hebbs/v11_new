# Calculate target-direction bias from the Intervention phase in the Baseline block

calc_bias_int <-function(data_tri_clean, data_tgt){
  
  library(dplyr)
  
  source("function/set_exp_param.R")

  # get trials of interest (baseline block & intervention phase)
  tois <- data_tgt %>%
    dplyr::filter(blk == baseblk & phase == phase_int) %>%
    .$total_tri
  
  data_tgt_bias_int <- data_tri_clean %>%
    dplyr::filter(total_tri %in% tois) %>% 
    group_by(tgt) %>%
    summarise(tgt_bias_int = mean(error_hand, na.rm = TRUE)) %>% # no need to remove s trial because its values are all NA
    ungroup()
  
  
}