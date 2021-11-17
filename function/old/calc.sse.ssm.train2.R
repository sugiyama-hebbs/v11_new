## This calculates the sum of squared error (sse) with a given set of parameters for 
## simple one state-space model (ssm) with a latent variable x 
## Tnis version is for fitting data in Train phase where performance error (cur - tgt) cannot be used and therefore
## estimated sensory prediction is used instead. 
##
## This now considers x0 as one "trial" before the first trial (which is not based on actually measured hand point
## but estimated solely from fitting)

## u(k) = PTB(k) - x(k) # error as input 
## x(k+1) = ax(k) + bu(k) # update x
##
#### input ####
## param: a vector containing model parameters (a,b,x0)
##        a: retention rate
##        b: learning rate
##        x0: initial value
## 
## data: a dataframe with the following observable data
##       tgt: target direction
##       err: estimated sensory prediction error
##       hand: hand direction
#### output ####
## sse: sum of squared error
##
#### Authour ####
## Taisei Sugiyama

calc.sse.ssm.train2 <- function(param,data){
  a = param[1]
  b = param[2]
  x0 = param[3]
  
  tgt = data$tgt
  # cur = data$cur
  hand = data$hand
  err = data$est_spe
  
  num_tri = length(tgt) # number of trials (time steps)
  x = vector(mode="integer", length = num_tri) # initialize
  u = vector(mode="integer", length = num_tri) # initialize
  
  # x[1] = x0 
  
  # Get model estimate
  for (tri in 1:num_tri){
    
    # Temporarily substitute NA data with tgt direction (so treating it as 0 input)
    if (is.na(hand[tri])) { 
      # cur[tri] = tgt[tri]
      hand[tri] = tgt[tri]
      u[tri] = 0 # assume no input (and no update)
    }
    
    u[tri] = err[tri] # Performance error

    if (tri == 1){
      x[tri] = x0 + b*u[tri]
    } else {
      x[tri] = a*x[tri-1] + b*u[tri] # Update x
    }
    
  }
  
  # Calculate sse
  sse = sum((x - (tgt-hand))^2) # Suppose tgt = hand + x
  
  return(sse)
  
} 