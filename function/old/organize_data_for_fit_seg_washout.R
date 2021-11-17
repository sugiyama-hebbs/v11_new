# This organizes data for fit on segments.

#### input ####
## soi: subject of interest
## boi: block of interest
## set_type: which part of Probe is used for fitting. Should be fixed to "seg"
##
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

organize_data_for_fit_seg_washout <- function(soi,boi,set_type = "seg", pre_pos = "pos"){

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
  data_tri_clean = read.csv(sprintf("%s/data_tri_clean_nb_fam.csv",data_fpath))
  
  data_fit = data_tri_clean %>%
    mutate(phase = data_tgt$phase) %>%
    mutate(shift = data_tgt$shift/1000) %>%
    dplyr::filter(blk >= init_boi & blk <= last_boi)  
  
  # Get a last few washout trial numbers 
  pre_tri = data_fit %>%
    group_by(blk,phase) %>%
    dplyr::filter(phase == phase_pre-1 | phase == phase_pos-1 ) %>%
    top_n(num_wo_inc,total_tri) %>%
    ungroup() %>%
    .$total_tri
  
  # Get probe trial numbers
  prb_tri = data_fit %>%
    group_by(blk,phase) %>%
    dplyr::filter(phase == phase_pre | phase == phase_pos) %>%
    .$total_tri
  
  # Combine and sort
  # toi = sort(c(pre_tri,prb_tri)) 
  toi = prb_tri # For now, no pre-washout trials are included
  
  # Clean the data and do some processing
  data_fit_clean_washout = data_fit %>%
    dplyr::filter(total_tri %in% toi) %>% # Extract Probe trials (+ some preceding washout)
    select(-gain,-mt,-rt,-rc_xpass,-rc_ypass) %>%
    mutate(prepos = ifelse(phase <= phase_pre,"pre","pos")) %>%
    group_by(blk) %>%
    # mutate(include = ifelse((phase %in% c(phase_pre,phase_pos) & shift != 0), 1, 
    #                         ifelse(shift[(row_number()+num_wo_inc)] !=0,1,0))) %>% # Flag trials to be included
    mutate(include = ifelse((phase %in% c(phase_pre,phase_pos) & shift == 0), 1,0)) %>% # Flag trials to be included
    ungroup() %>%
    dplyr::filter(include == 1) %>% # filter out non-included trials
    group_by(blk,prepos) %>%
    mutate(row_num = row_number()) %>%
    mutate(seg_num = (row_num-1)%/%seg_len_wo + 1) %>% # segment number
    dplyr::filter(blk %in% boi)
  
  # Return an appropriate data
  return(data_fit_clean_washout)
  

}