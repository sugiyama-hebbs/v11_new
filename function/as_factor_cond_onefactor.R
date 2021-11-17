# Convert experiment conditions in one factor
# Assuming that df already has experiment condition tags in string
#
# Author: Taisei Sugiyama

as_factor_cond_onefactor <- function (df){
  
  # Load packages
  
  df_edit = df %>%
    mutate(tds = as.character(task_demand), vs = as.character(valence)) %>% # convert back to string if they are factor
    mutate(group_str = ifelse(tds == "Go" & vs == "Win", "GW",
                       ifelse(tds == "Go" & vs == "Avoid", "GA",
                       ifelse(tds == "Nogo" & vs == "Win", "NW",
                       ifelse(tds == "Nogo" & vs == "Avoid", "NA",
                       ifelse(tds == "Base", "Base",
                       ifelse(tds == "Rnd", "Rnd", NA
                       ))))))) %>%
    mutate(group = factor(group_str, levels = c("GW","GA","NW","NA","Base","Rnd"))) %>%
    dplyr::select(-group_str,-tds,-vs)

  return(df_edit)
  
}