## This calculates the sum of squared error (sse) with a given set of parameters for 
## simple one state-space model (ssm) with a latent variable x 
## Tnis version is for fitting data in Train phase where performance error (cur - tgt) cannot be used and therefore
## estimated sensory prediction is used instead. 
##
## u(k) = PTB(k) - x(k) # error as input 
## x(k+1) = ax(k) + bu(k) # update x
##
#### input ####
## param: a vector containing model parameters (a,b,x0)
##        a: retention rate
##        b: learning rate
##        x0: initial value
## 
## data: a dataframe 
##
#### output ####
## sse: sum of squared error
##
#### Authour ####
## Taisei Sugiyama

calc.sse.ssm.train_new <- function(param,data){
  a = param[1]
  b = param[2]
  x0 = param[3]
  
  hand <- data$ypos
  spe <- data$spe
  
  num_tri = dim(data)[1] # number of trials (time steps)
  x = vector(mode="integer", length = num_tri) # initialize
  
  x[1] = x0 
  
  # Get model estimate
  for (tri in 1:num_tri){
    
    # Temporarily substitute NA data with tgt direction (so treating it as 0 input)
    if (is.na(hand[tri])) { 
      hand[tri] = 0
    }
    
    if (tri != num_tri)
      x[tri+1] = a*x[tri] + b*spe[tri] # Update x
  }
  
  # Calculate sse
  sse = sum((x - hand)^2) # Suppose tgt = hand + x
  
  return(sse)
  
} 