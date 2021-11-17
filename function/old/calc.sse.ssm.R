## This calculates the sum of squared error (sse) with a given set of parameters for 
## simple one state-space model (ssm) with a latent variable x 
## u(k) = cur(k) - tgt(k) # error as input 
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
##       cur: cursor direction
##       hand: hand direction
#### output ####
## sse: sum of squared error
##
#### Authour ####
## Taisei Sugiyama

calc.sse.ssm <- function(param,data){
  a = param[1]
  b = param[2]
  x0 = param[3]
  
  tgt = data$tgt
  cur = data$cur
  hand = data$hand
  
  num_tri = length(tgt) # number of trials (time steps)
  x = vector(mode="integer", length = num_tri) # initialize
  u = vector(mode="integer", length = num_tri) # initialize
  
  x[1] = x0 
  
  # Get model estimate
  for (tri in 1:num_tri){
    
    # Temporarily substitute NA data with tgt direction (so treating it as 0 input)
    if (is.na(cur[tri])) { 
      cur[tri] = tgt[tri]
      hand[tri] = tgt[tri]
    }
    
    u[tri] = cur[tri] - tgt[tri] # Performance error
    if (tri != num_tri)
      x[tri+1] = a*x[tri] + b*u[tri] # Update x
  }
  
  # Calculate sse
  sse = sum((x - (tgt-hand))^2) # Suppose tgt = hand + x
  
  return(sse)
  
} 