## This calculates the sum of squared error (sse) with a given set of parameters for 
## single exponential fitting.  
## Tnis version is for fitting data in Train phase where performance error (cur - tgt) cannot be used and therefore
## estimated sensory prediction is used instead. 
##
## x(t) = a(1-e((t-1)/tau)) + x1
## 
#### input ####
## param: a vector containing model parameters (tau,a,x0)
##        tau: time constant (smaller, the faster learning) 
##        a: amplitude
##        x1: initial value at t = 1
## 
## data: a vector containing values for fitting
#### output ####
## sse: sum of squared error
##
#### Authour ####
## Taisei Sugiyama

calc.sse.exp.train <- function(param, data){
  
  x = data$val
  tau = param[1]
  a = param[2]
  x0 = param[3]
  
  # Get model estimate
  tm = 1:length(x)
  est_x = a*(1 - exp(-(tm-1)/tau))+x0
  
  # Calculate sse
  sse = sum((x - est_x)^2) 
  
  return(sse)
  
} 