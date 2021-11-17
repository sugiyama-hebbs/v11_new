# Convert stan output to dataframe
#
# input #
# stan_res: object that contains MCMC output from stan function (Perhaps output from other MCMC packages works as well)
# varname: character representing specific parameter with the matching name to be returned. Be careful that it is case sensitive and partially matching variables are also included

stan_res_df <- function(stan_res, varname = NA) {
  
  if(is.na(varname)){
    res_summary_df_raw <- as.data.frame(summary(stan_res, probs = c(.025, .25, .5, .75, .975))$summary) 
  } else {
    res_summary_df_raw <- as.data.frame(summary(stan_res, pars = varname, probs = c(.025, .25, .5, .75, .975))$summary)
  }
  
  return_df <- res_summary_df_raw %>% 
    tibble::rownames_to_column()
  
  colnames(return_df) <- c("var","mean","se_mean","sd","lb","twentyfive","fifty","seventyfive", "ub","n.eff","Rhat")
  
  return(return_df)
  
}
