# Run exponential fitting with a given set of parameters and return the results.
# 
#### Model ####
## x(t) = a(1-e((t-1)/tau)) + x1
## 
#### input ####
## param: a vector containing model parameters (tau,a,x0)
##        tau: time constant (smaller, the faster learning) 
##        a: amplitude
##        x1: initial value at t = 1
## 
## data: a dataframe with the following observable data
##       val: value used for fitting (typicall hand error)
##       tgt: target direction
##       hand: hand direction
##
#### output ####
## sse: sum of squared error
##
#### Authour ####
## Taisei Sugiyama
#### output ####
## results: a list containing
##       param, data: function inputs
##       est_hand: estimated output (hand direction)
##       r2: coefficient of determination
##       rmse: root mean squared error
##
#### Authour ####
## Taisei Sugiyama

run.exp.train <- function(param = NA, data) {
  
  # Do some preparation
  if (is.na(param)){ # parameter is included in data
    tau = data$alpha[1]
    a = data$beta[1]
    x1 = data$x0[1]
    
  } else { # parameter is supplied
    tau = param[1]
    a = param[2]
    x1 = param[3]
  }
  
  x = data$val
  tgt = data$tgt
  # cur = data$cur
  hand = data$hand

  # Get model estimate
  tm = 1:length(x)
  est_x = a*(1 - exp(-(tm-1)/tau))+x0
  
  est_hand = tgt - est_x
  
  hand_align = hand - (tgt - 90) # aligned to 90 degree-tgt
  
  r2 = 1 - sum((hand-est_hand)^2)/sum((hand_align-mean(hand_align))^2)
  
  if (r2 < 0){
    r2 = 0
  }
  
  
  rmse = sqrt(mean((est_hand - hand)^2, na.rm = TRUE)) # root mean squared error
  
  results = list(param = param, data = data, x = x, est_hand = est_hand, r2 = r2, rmse = rmse)
  
  return(results)
}