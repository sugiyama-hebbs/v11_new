# This gets condition from subject id
# soi: subject id (int)
#
# Author: Taisei Sugiyama

get_cond <- function(soi){
  
  ## Set key values manually
  tgtname_iv1_pos <- 8 # the letter position in target file specifying the independent variable 1 (task demand)
  tgtname_iv2_pos <- 9 # the letter position in target file specifying the independent variable 2 (valence)
  
  ## Read in subject data

  # preparation
  source("function/set_exp_param.R")
  data_dir <- "data/processed"
  
  if (soi < 10){
    tmp_str <- sprintf("S0%d",soi)
  } else {
    tmp_str <- sprintf("S%d",soi) 
  }
  
  data_fpath <- sprintf("%s/%s",data_dir,tmp_str)
  
  # Get training condition as string 
  exp_condition <- read.csv(sprintf("%s/data_para.csv",data_fpath)) %>%
    .$tgt_fname %>%
    as.vector(.) %>%
    .[trainblk[1]] %>% 
    substr(.,tgtname_iv1_pos,tgtname_iv2_pos )
  
  # Organize returning value
  
  if (is.na(exp_condition)){
    iv1 <- "None"
    iv2 <- "None" 
    ctag <- "no"
    
  } else if (exp_condition == "gw"){ # Go to Win
    iv1 <- "Go"
    iv2 <- "Win" 
    ctag <- "gw"
  } else if (exp_condition == "ga"){ # Go to Avoid
    iv1 <- "Go"
    iv2 <- "Avoid" 
    ctag <- "ga"
  } else if (exp_condition == "nw"){ # Nogo to Win
    iv1 <- "Nogo"
    iv2 <- "Win" 
    ctag <- "nw"
  } else if (exp_condition == "na"){ # Nogo to Avoid
    iv1 <- "Nogo"
    iv2 <- "Avoid" 
    ctag <- "na"
  } else {
    iv1 <- "None"
    iv2 <- "None" 
    ctag <- "no"
    
  }
  
  
  # else if (exp_condition == "BAS"){ # Base (No reward)
  #   iv1 <- "Base"
  #   iv2 <- "Base" 
  #   ctag <- "bs"
  # } else if (exp_condition == "RND"){ # Randon Avoid
  #   iv1 <- "Rnd"
  #   iv2 <- "Avoid" 
  #   ctag <- "rd"
  # } else{ # Inverse Go to Avoid (depreciate)
  #   iv1 <- "IGo"
  #   iv2 <- "IAvoid"
  #   ctag <- "ig"
  # } 
  
  
  df <- data.frame(sub_id = soi, task_demand = iv1, valence = iv2, ctag = ctag) # set column names according to IV name
  
  ##  return
  return(df)
  
}