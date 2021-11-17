num_lr <- df_data_ind %>% 
  dplyr::filter(cond == "LR") %>% 
  .$sub_id %>% 
  unique() %>% 
  length()

num_lp <- df_data_ind %>% 
  dplyr::filter(cond == "LP") %>% 
  .$sub_id %>% 
  unique() %>% 
  length()

num_nr <- df_data_ind %>% 
  dplyr::filter(cond == "NR") %>% 
  .$sub_id %>% 
  unique() %>% 
  length()

num_np <- df_data_ind %>% 
  dplyr::filter(cond == "NP") %>% 
  .$sub_id %>% 
  unique() %>% 
  length()


num_all <- length(sub_inc_main)

if (num_all != (num_lr + num_lp + num_nr + num_np))
  warning("Labeling of conditions seems incorrect!")
