

df_data_probe_pre_bl <- df_data_ind %>% 
  dplyr::filter(lead(block_phase, prb_pre_null_inc) == phase_prb_prebl | block_phase == phase_prb_prebl) %>% 
  group_by(sub_id) %>% 
  mutate(prb_tri = row_number(), run = 1) %>% 
  ungroup()

df_data_probe_ind <- df_data_ind %>% 
  dplyr::filter(lead(block_phase, prb_pre_null_inc) == phase_prb | block_phase == phase_prb) %>% 
  group_by(sub_id, blk) %>% 
  mutate(prb_tri = row_number()) %>% 
  ungroup() %>% 
  mutate(run = blk) %>% 
  rbind(df_data_probe_pre_bl,.) %>% 
  dplyr::select(-blk) %>% 
  dplyr::rename(blk = run) %>% 
  arrange(sub_id,blk)


# individual train 
df_data_train_ind <- df_data_ind %>%
  dplyr::filter(block_phase == phase_train) %>% 
  mutate(rot = ifelse(m_tri == "y", 0, rot), herr = ifelse(tsize == 0, NA, herr)) %>% 
  group_by(sub_id) %>% 
  mutate(total_cycle = (row_number()-1)%/% tpc + 1) %>% # some modifications for further processing/plotting
  ungroup() %>% 
  group_by(sub_id,blk) %>% 
  mutate(block_cycle = (row_number()-1)%/% tpc + 1, train_tri = row_number()) %>% 
  ungroup() %>% 
  group_by(sub_id,total_cycle) %>% 
  mutate(cycle_tri = row_number()) %>% 
  ungroup()


# individual washout
# df_data_wo_ind <- df_data_ind %>% 
#   dplyr::select(-herr_cross, -task_break,-train_type, -difficulty, -score) %>%
#   dplyr::filter(rot == 0, show_cur != 3, block_phase == 6) %>% # remove rotation trials and clamp trials
#   group_by(sub_id, blk) %>%
#   mutate(wo_tri = row_number()) %>% 
#   ungroup()

### group average ###
source(sprintf("script/plot/%s/sub_script/local_function/count_num_each_cond.R",main_dir)) 

## all trials
df_data <- df_data_ind %>% 
  mutate(rot = ifelse(m_tri == "y", 0, rot), herr = ifelse(tsize == 0, NA, herr)) %>% 
  dplyr::rename(voi = herr, tri = blk_tri) %>% 
  summarise_by_trial_cond()

df_data_td <- df_data_ind %>% 
  mutate(rot = ifelse(m_tri == "y", 0, rot), herr = ifelse(tsize == 0, NA, herr)) %>% 
  dplyr::rename(voi = herr, tri = blk_tri) %>% 
  summarise_by_trial_td()

df_data_val <- df_data_ind %>% 
  mutate(rot = ifelse(m_tri == "y", 0, rot), herr = ifelse(tsize == 0, NA, herr)) %>% 
  dplyr::rename(voi = herr, tri = blk_tri) %>% 
  summarise_by_trial_val()

## Probe
df_data_probe <- df_data_probe_ind %>% 
  dplyr::rename(voi = herr, tri = blk_tri) %>% 
  summarise_by_trial_cond()

df_data_probe_td <- df_data_probe_ind %>% 
  dplyr::rename(voi = herr, tri = blk_tri) %>% 
  summarise_by_trial_td()

df_data_probe_val <- df_data_probe_ind %>% 
  dplyr::rename(voi = herr, tri = blk_tri) %>% 
  summarise_by_trial_val()

## Train
df_data_train <- df_data_train_ind %>% 
  dplyr::rename(voi = herr, tri = blk_tri) %>% 
  summarise_by_trial_cond()

df_data_train_td <- df_data_train_ind %>% 
  dplyr::rename(voi = herr, tri = blk_tri) %>% 
  summarise_by_trial_td()

df_data_train_val <- df_data_train_ind %>% 
  dplyr::rename(voi = herr, tri = blk_tri) %>% 
  summarise_by_trial_val()
