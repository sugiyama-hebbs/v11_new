# Function to extract pd and DIC values from jags output
# Should be included in jagstool but somehow not, so create this locally...
#
# Source: https://rdrr.io/github/johnbaums/jagstools/src/R/pd_dic.R

pd_dic <- function(x) {
  if(!any(is(x) %in% c('rjags.parallel', 'rjags')))
    stop('x must be an mcmc.list or rjags  object.')
  if(!x$DIC) list(pD=NA, DIC=NA) else list(pd=x$BUGSoutput$pD, DIC=x$BUGSoutput$DIC)
}