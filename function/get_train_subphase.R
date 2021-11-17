# This gets sub_phase in Train phase (Each sub-phase consists of the trials with the same rotation direction and size)
# Make sure to run this after you load data_tgt

get_train_subphase <- function(data_tgt){
  
  library(dplyr)
  
  source("function/set_exp_param.R")

  # Get the subphase and the last trial index of each subphase (except the last phase)
  sub_phase_idx = data_tgt %>%
    dplyr::filter(phase == phase_int, blk == trainblk[1], wait == 1) %>%
    dplyr::select(blk_tri) %>%
    mutate(sub_phase = row_number())
  
  # Train phase trial indices
  phase_int_tri = data_tgt %>%
    dplyr::filter(phase == phase_int, blk == trainblk[1]) %>%
    .$blk_tri
  
  # Now run a loop to get subphase indices for each Train phase trial
  temp_sub_phase = rep(0,length(phase_int_tri))
  ctri = 0
  for (tri in phase_int_tri){
    ctri = ctri + 1 # update counter
    
    for (this_sub_phase in sub_phase_idx$sub_phase){
      if (tri <= sub_phase_idx$blk_tri[this_sub_phase]){
        temp_sub_phase[ctri] = sub_phase_idx$sub_phase[this_sub_phase]
        break
      }
      
      # If tri is bigger than the largest index in the df, that means the tri belongs to the last subphase
      if (tri > max(sub_phase_idx$blk_tri)){
        temp_sub_phase[ctri] = max(sub_phase_idx$sub_phase)+1
      }
    }
  }
  
  # Create a dataframe and attach it to the input dataframe
  sub_phase_df = data.frame(blk_tri = phase_int_tri, sub_phase = temp_sub_phase, phase = phase_int)  
  
  # Done!
  return(sub_phase_df)
  
}