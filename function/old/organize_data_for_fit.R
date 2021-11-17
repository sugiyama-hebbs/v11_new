# This organizes data for fit.

#### input ####
## soi: subject of interest
## boi: block of interest
## set_type: which part of Probe is used for fitting
##        whole: the whole Probe (default)
##        first: 1st half
##        second: 2nd half
##        init: initial (some washout + initial part)
## pre_pos: which data to extract, pre or post (default)
##
#### output ####
## results: a list containing
##       param, data: function inputs
##       x: estimated latent variable (typically memory of perturbation in visuomotor task)
##       est_hand: estimated output (hand direction)
##       r2: coefficient of determination
##       rmse: root mean squared error
##
#### Authour ####
## Taisei Sugiyama

organize_data_for_fit <- function(soi,boi,set_type = "whole", pre_pos = "pos"){

  ## Load in packages
  library(dplyr)
  
  # Set key parameters
  ## Make sure the variables are set appropriately in the source script
  source("script/ind_process/lsse_fit_param.R")

  ## Read in data
  
  # Convert to string with prefix identifier
  if (soi < 10){
    sid_str = sprintf("S0%d",soi)
  } else {
    sid_str = sprintf("S%d",soi) 
  }
  
  data_fpath = sprintf("data/processed/%s",sid_str)
  
  # Get sequence file data
  data_tgt = read.csv(sprintf("%s/data_tgt.csv",data_fpath))
  
  # Get parameter file data
  data_tri_clean = read.csv(sprintf("%s/data_tri_clean.csv",data_fpath))
  
  ## Organize data
  data_fit = data_tri_clean %>%
    mutate(phase = data_tgt$phase) %>%
    # mutate(prb = data_tgt$prb) %>%
    dplyr::filter(blk ==boi) 
  
  
  # Get a last few washout trial numbers 
  pre_tri = data_fit %>%
    group_by(blk,phase) %>%
    dplyr::filter(phase == phase_pre-1 | phase == phase_pos-1 ) %>% # Get the phase before Probe phase
    top_n(num_wo_inc,total_tri) %>% # Get the last few trials right before Probe and trim the rest
    ungroup() %>%
    .$total_tri
  
  # Get probe trial numbers
  prb_tri = data_fit %>%
    group_by(blk,phase) %>%
    dplyr::filter(phase == phase_pre | phase == phase_pos) %>%
    .$total_tri
  
  # Combine and sort
  toi = sort(c(pre_tri,prb_tri)) 

  # Add some flags for filtering
  data_prb = data_fit %>%
    dplyr::filter(total_tri %in% toi) %>%
    mutate(prepos = ifelse(phase <= phase_pre,"pre","pos")) %>%
    group_by(prepos,blk) %>%
    mutate(initial = ifelse(row_number()<=num_init,1,0)) %>%
    # mutate(half = ifelse(row_number()>num_wo_inc & row_number()-num_wo_inc<= floor(prb_len/2),"first",
    #                      ifelse(row_number() > floor(prb_len/2),"second",NA))) %>%
    ungroup()
  
  # Whole probe data
  data_prb_whole = data_prb %>%
    group_by(prepos,blk) # %>%
    # dplyr::filter(half == "first" | half == "second")
  
  # First half
  # data_prb_first = data_prb %>%
  #   group_by(prepos,blk) %>%
  #   dplyr::filter(half == "first")
  # 
  # # Second half
  # data_prb_second = data_prb %>%
  #   group_by(prepos,blk) %>%
  #   dplyr::filter(half == "second")
  
  # Initial phase
  data_prb_init = data_prb %>%
    group_by(prepos,blk) %>%
    dplyr::filter(initial == 1) %>%
    ungroup()
  
  # Return an appropriate data
  if (set_type =="whole"){
    data = data_prb_whole
  
  # } else if (set_type =="first") {
  #   data = data_prb_first
  # } else if (set_type =="second") {
  #   data = data_prb_second
  } else if (set_type =="init")  {
    data = data_prb_init
  } else {
    data = NULL
  }
  
  return(data)
  
  

  
}