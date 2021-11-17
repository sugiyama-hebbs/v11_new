# Get subject ids of the group of interest (condition)
# return dataframe with group tag instead of just subject id
#### input ####
# id_all: all subject ids 
# cond: condition name ("gw","ga","nw","na","bs","all","go","nogo","rwd","pun","main")
#
#### output ####
# dataframe
#
#### Author ####
# Taisei Sugiyama

get_grp_soi_df <- function(id_all,cond){
  
  ## Preparation
  library(dplyr)
  library(purrr)
  source("function/get_cond.R")
  
  # create a df with sub_id and condition tags
  df_cond <- lapply(id_all, get_cond) %>% 
    reduce(rbind) %>% 
    mutate(group_raw = ifelse(ctag %in% c("gw","ga","nw","na"), toupper(ctag), 
                              ifelse(ctag == "bs", "Base", "Rnd"))) %>% 
    mutate(group = factor(group_raw, levels = c("GW","GA","NW","NA","Base","Rnd"))) %>% 
    dplyr::select(-group_raw)

  # Set condition filter
  if (cond == "go"){
    conds <- c("gw","ga")
  } else if (cond == "nogo"){
    conds <- c("nw","na")
  } else if (cond == "main") { # main 4 group
    conds <- c("gw","ga","nw","na")
  } else if (cond == "rwd"){ # reward groups
    conds <- c("gw","nw")
  } else if (cond == "pun"){ # punishment groups (not including control punishment (rd))
    conds <- c("ga","na")
  } else if (cond == "all"){ # all groups
    conds <- c("gw","ga","nw","na","bs","rd")
  } else { # 1 condition specified by function input
    conds <- cond
  }
  
  # Create a temporary dataframe and perform filtering
  return_df = df_cond %>%
    dplyr::filter(ctag %in% conds) %>% 
    dplyr::select(sub_id, task_demand, valence, group)
  
  
}