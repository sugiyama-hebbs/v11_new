# Add experiment conditions to data frame
# 
# Author: Taisei Sugiyama

add_condition <- function (df, cond){
  
  ## Load packages
  library(dplyr)
  library(purrr)
  
  subs = as.numeric(unique(df$sub_id))
  
  conditions_list = vector("list", length(subs))
  
  csub = 0
  for (sub in subs){
    csub = csub + 1
    conditions_list[[csub]] = get_condition(sub) %>%
      mutate(sub_id = sub)
  }
  
  conditions_df = reduce(conditions_list,rbind)
  

  df_edit = df %>%
    left_join(conditions_df, by="sub_id")

}