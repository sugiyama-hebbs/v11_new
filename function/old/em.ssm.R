## This estimates the parameters of state-space model by Expectation-Maximizaiton (EM) algorithm, as 
## presented in some literature (Ghahramani 1996, Shumway & Stoffer Time Series Analysis and Its Applications)
## 
## u(k) = cur(k) - tgt(k) # error as input 
## x(k+1) = ax(k) + bu(k) + w # update latent variable x
## y = Cx(k) + v # observed variable (Currently, C is fixed to 1)
##
#### input ####
## 
## data: a dataframe with the following observable data
##       tgt: target direction
##       cur: cursor direction
##       hand: hand direction
##
## fix_alpha (Optional): whether or not to fix alpha to 1 (no forgetting). Boolean
##
## max_iter (Optional): maximum number of iterations. Natural number
##
## thresh (Optional): threshold for checking the convergence
#### output ####
## est_para: estimated parameter
##      Q,R,C,Pi,V1,B
##
#### Authour ####
## Taisei Sugiyama

em.ssm <- function(data, max_iter = 500, tol = 1.0e-3, fix_alpha = TRUE) {
  
  ## Load in packages & sources (Check later if this is needed in fuction call)
  source("function/EM1.input.R")
  
  ## Get key values
  num = dim(data)[1] # number of trials
  
  tgt = data$tgt
  hand = data$hand
  cur = data$cur
  
  # Check any invalid (Null) values. For those, basically treat it as no input (u = 0)
  # and no change in observation (i.e., y(k) = y(k-1) where y(k) is Null)
  u = cur - tgt # Error as input
  
  y = tgt - hand # Observation (Hand error), which is equal to x (given C = 1)
  
  for (tri in 1:num){
    if (is.na(u[tri])){
      u[tri] = 0
      
      if (tri > 1){
        y[tri] = y[tri-1]
      } else {
        y[tri] = 0 # No previous trial to copy, so just put 0
      }
    }
    
  }
  
  input = c(0, u[1:num-1])
  
  if (fix_alpha){
    A0 = 1
  } else {
    A0 = 0.8
  }
  
  # A: state-transition matrix (alpha)
  # Ups: state input matrix (beta)
  # cQ, cR: Cholesky-type decomposed state/observation error covariance matrix
  # mu0: initial state mean
  # Sigma0: initial state covariance matrix
  results =  EM1.input(num = num, y = y, mu0 = 0.5, Sigma0 = 1, A0 = A0, B0 = 0.1, C0 = 1, Q0 = 0.1, R0 = 0.1, input = input,
                       max.iter = max_iter, tol = tol, fix_alpha = fix_alpha) 
  
  estpara = list(solution = c(results$A,results$B,results$mu0), C = results$C, Sigma0 = results$Sigma0, R = results$R, Q = results$Q, ks = results$ks)
  
  return(estpara)
  
} 