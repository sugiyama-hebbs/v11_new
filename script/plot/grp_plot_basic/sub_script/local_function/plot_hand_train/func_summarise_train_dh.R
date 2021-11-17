

## Prepos-S
summarise_train_dh_preorpos_cond_ind <- function(df){
  df_return <- df %>% 
    group_by(sub_id, blk,rot, cond, td, valence) %>% 
    summarise(ind_m = mean(voi, na.rm = T), n = length(voi)) %>% 
    ungroup()
}

summarise_train_dh_preorpos_td_ind  <- function(df){
  df_return <- df %>% 
    group_by(sub_id, blk,rot, td) %>% 
    summarise(ind_m = mean(voi, na.rm = T), n = length(voi)) %>% 
    ungroup()
}

summarise_train_dh_preorpos_val_ind  <- function(df){
  df_return <- df %>% 
    group_by(sub_id, blk,rot, cond, td, valence) %>% 
    summarise(ind_m = mean(voi, na.rm = T), n = length(voi)) %>% 
    ungroup()
}


summarise_train_dh_prepos_cond_ind <- function(df){
  df_return <- df %>% 
    group_by(sub_id, blk,rot, trial, cond, td, valence) %>% 
    summarise(ind_m = mean(voi, na.rm = T), n = length(voi)) %>% 
    ungroup()
}

summarise_train_dh_prepos_td_ind  <- function(df){
  df_return <- df %>% 
    group_by(sub_id, blk,rot, trial, td) %>% 
    summarise(ind_m = mean(voi, na.rm = T), n = length(voi)) %>% 
    ungroup()
}

summarise_train_dh_prepos_val_ind  <- function(df){
  df_return <- df %>% 
    group_by(sub_id, blk,rot, trial, cond, td, valence) %>% 
    summarise(ind_m = mean(voi, na.rm = T), n = length(voi)) %>% 
    ungroup()
}

summarise_train_dh_prepos_cond <- function(df){
  df_return <- df %>% 
    group_by(blk,rot, trial, cond, td, valence) %>% 
    summarise(m = mean(voi, na.rm = T), sd = sd(voi, na.rm =T), se= sd(voi, na.rm = T)/sqrt(length(voi)), n = length(voi)) %>% 
    ungroup()
}

summarise_train_dh_prepos_td <- function(df){
  df_return <- df %>% 
    group_by(blk,rot, trial, td) %>% 
    summarise(m = mean(voi, na.rm = T), sd = sd(voi, na.rm =T), se= sd(voi, na.rm = T)/sqrt(length(voi)), n = length(voi)) %>% 
    ungroup()
}

summarise_train_dh_prepos_val <- function(df){
  df_return <- df %>% 
    group_by(blk,rot, trial, cond, td, valence) %>% 
    summarise(m = mean(voi, na.rm = T), sd = sd(voi, na.rm =T), se= sd(voi, na.rm = T)/sqrt(length(voi)), n = length(voi)) %>% 
    ungroup()
}






## dh
summarise_train_dh_cond_ind <- function(df){
  df_return <- df %>% 
    group_by(sub_id, blk,rot, cond, td, valence) %>% 
    summarise(ind_m = mean(voi, na.rm = T), n = length(voi)) %>% 
    ungroup()
}

summarise_train_dh_td_ind  <- function(df){
  df_return <- df %>% 
    group_by(sub_id, blk,rot, td) %>% 
    summarise(ind_m = mean(voi, na.rm = T), n = length(voi)) %>% 
    ungroup()
}

summarise_train_dh_val_ind  <- function(df){
  df_return <- df %>% 
    group_by(sub_id, blk,rot, cond, td, valence) %>% 
    summarise(ind_m = mean(voi, na.rm = T), n = length(voi)) %>% 
    ungroup()
}

summarise_train_dh_cond <- function(df){
  df_return <- df %>% 
    group_by(blk,rot, cond, td, valence) %>% 
    summarise(m = mean(voi, na.rm = T), sd = sd(voi, na.rm =T), se= sd(voi, na.rm = T)/sqrt(length(voi)), n = length(voi)) %>% 
    ungroup()
}

summarise_train_dh_td <- function(df){
  df_return <- df %>% 
    group_by(blk,rot, td) %>% 
    summarise(m = mean(voi, na.rm = T), sd = sd(voi, na.rm =T), se= sd(voi, na.rm = T)/sqrt(length(voi)), n = length(voi)) %>% 
    ungroup()
}

summarise_train_dh_val <- function(df){
  df_return <- df %>% 
    group_by(blk,rot, cond, td, valence) %>% 
    summarise(m = mean(voi, na.rm = T), sd = sd(voi, na.rm =T), se= sd(voi, na.rm = T)/sqrt(length(voi)), n = length(voi)) %>% 
    ungroup()
}

