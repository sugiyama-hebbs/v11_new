# Get subject ids of the group of interest (condition)
#### input ####
# id_all: all subject ids 
# cond: condition name ("ga", "na")
#
#### output ####
# numeric vector 
#
#### Author ####
# Taisei Sugiyama

get_grp_soi <- function(id_all,cond){
  
  # No processing is needed if all subjects are included
  if (cond == "all") {
    return(id_all)
  }
  
  ## Preparation
  library(dplyr)
  library(purrr)
  source("function/get_cond.R")
  
  # create a df with sub_id and condition tags
  df_cond <- lapply(id_all, get_cond) %>% 
    reduce(rbind)

  # Set condition filter
  if (cond == "go"){
    conds <- c("gw","ga")
  } else if (cond == "nogo"){
    conds <- c("nw","na")
  } else if (cond == "main") { # main 4 group
    conds <- c("gw","ga","nw","na")
  } else if (cond == "rwd"){ # reward groups
    conds <- c("gw","nw")
  } else if (cond == "pun"){ # punishment groups (not including control punishment)
    conds <- c("ga","na")
  } else if (cond == "pun2"){ # punishmet groups with control 
    conds <- c("ga","na","rd")
  } else { # 1 condition specified by function input
    conds <- cond
  }
  
  # Create a temporary dataframe and perform filtering
  tmp_df = df_cond %>%
    dplyr::filter(ctag %in% conds)
  
  # Now return soi
  return(tmp_df$sub_id)

  
}