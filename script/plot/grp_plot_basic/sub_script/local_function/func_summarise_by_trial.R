




summarise_by_trial_cond <- function(df){
  df_return <- df %>% 
    group_by(cond,td, valence, blk,tri,show_cur,tsize,rot,block_phase, m_tri) %>% 
    summarise(m = mean(voi, na.rm = T), sd = sd(voi, na.rm =T), se= sd(voi, na.rm = T)/sqrt(length(voi)), n = length(voi)) %>% 
    ungroup()
}

summarise_by_trial_td <- function(df){
  df_return <- df %>% 
    mutate(rot = ifelse(m_tri == "y", 0, rot), voi = ifelse(tsize == 0, NA, voi)) %>% 
    group_by(td, blk,tri,show_cur,tsize,rot,block_phase, m_tri) %>% 
    summarise(m = mean(voi, na.rm = T), sd = sd(voi, na.rm =T), se= sd(voi, na.rm = T)/sqrt(length(voi)), n = length(voi)) %>% 
    ungroup()
}

summarise_by_trial_val <- function(df){
  df_return <- df %>% 
    group_by(valence, blk,tri,show_cur,tsize,rot,block_phase, m_tri) %>% 
    summarise(m = mean(voi, na.rm = T), sd = sd(voi, na.rm =T), se= sd(voi, na.rm = T)/sqrt(length(voi)), n = length(voi)) %>% 
    ungroup()
}

