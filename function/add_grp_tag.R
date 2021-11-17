# This adds grouping tag (i.e., independent variable: Task-demand & valence) to dataframe
#
#### Input ####
# dfoi: dataframe of interest (to which tags are added)
#
#### Output ####
# df_return: input dataframe with added tags 
#
# Author: Taisei Sugiyama

add_grp_tag <- function(dfoi) {
  
  # Load packages
  library(dplyr)
  source("function/set_exp_param.R")
  source("function/get_grp_soi_df.R")
  
  # get dataframe containing all subject ids and conditions
  df_tag <- get_grp_soi_df(all_subs,"all")
 
  # now attach the df to the input df
  df_return = dfoi %>%
    left_join(df_tag, by = "sub_id")
  
}
