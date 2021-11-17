## This function is adapted from EM1 in Stoffer's astsa package and accepts input.
## This only works on 1-dimensional latent & obsrvation variables (x & y)
## Some additional modification besides accepting inputs:
## There is an option to fix alpha (A) 
## 
## Author: Taisei Sugiyama

source("function/Ksmooth1_1d.R")

EM1.input <- function (num, y, mu0, Sigma0, A0, B0, C0, Q0, R0, input, max.iter = 100, 
          tol = 0.001, fix_alpha = TRUE) 
{
  input0 = 0 # Initial input (one time-step before the inital observation). For now, set to 0

  A = as.matrix(A0)
  B = B0
  C = C0 
  Q = Q0
  R = R0
  
  pdim = nrow(A)
  y = as.matrix(y)
  qdim = ncol(y)
  cvg = 1 + tol
  like = matrix(0, max.iter, 1)
  # miss = ifelse(abs(y) > 0, 0, 1)
  # cat("iteration", "   -loglikelihood", "\n")
  for (iter in 1:max.iter) {
    
    # E-step
    ks = Ksmooth1_1d(num, y, mu0, Sigma0, A, B, C, Q, R, input) 

    Pcs = array(NA, dim = c(1,num)) # Backward recursion with two time steps. Corresponds to V2 in MATLAB code

    Pcs[num] = (1 - ks$Kn %*% C) %*% A %*% ks$Pf[num - 1] # Last time-step
    
    # Backward recursion with two time steps
    for (k in num:3) {
      Pcs[k - 1] = ks$Pf[k - 1] %*% t(ks$J[k - 2]) + ks$J[k - 1] %*% 
        (Pcs[k] - A %*% ks$Pf[k - 1]) %*% t(ks$J[k - 2])
    }
    
    # t = 1
    Pcs[1] = ks$Pf[1] %*% t(ks$J0) + ks$J[1] %*% (Pcs[2] - A %*% ks$Pf[1]) %*% t(ks$J0)
    
    # Calculate some values for updating the parameters
    # Initialize and get values at time-step 1 
    S11 = ks$xs[1] %*% t(ks$xs[1]) + ks$Ps[1]
    S10 = ks$xs[1] %*% t(ks$x0n) + Pcs[1]
    S00 = ks$x0n %*% t(ks$x0n) + ks$P0n
    
    Sxu00 = ks$x0n*input0
    Sxu10 = ks$xs[1] %*% t(input0)
    input00 = input0^2
    
    resid = y[1] - C %*% ks$xs[1] # Residual 
    tmpR = resid %*% t(resid) + C %*% ks$Ps[1] %*% t(C)
    
    # Sum up the 2nd to the end of time-step (can't it just use sum function?) 
    for (i in 2:num) {
      S11 = S11 + ks$xs[i] %*% t(ks$xs[i]) + ks$Ps[i]
      S10 = S10 + ks$xs[i] %*% t(ks$xs[i - 1]) + Pcs[i]
      S00 = S00 + ks$xs[i - 1] %*% t(ks$xs[i - 1]) + ks$Ps[i - 1]
      Sxu00 = Sxu00 + ks$xs[i - 1] %*% t(input[i-1])
      Sxu10 = Sxu10 + ks$xs[i] %*% t(input[i-1])
      input00 = input00 + input[i-1]^2
      
      resid = y[i] - C %*% ks$xs[i] 
      tmpR = tmpR + resid %*% t(resid) + C %*% ks$Ps[i] %*% t(C)
    }
    
    C = C # Fixed
    R = tmpR/num
    
    # Update (M-step)
    if (fix_alpha)
      A = 1
    # else
      # A = S10 %*% solve(S00) # Check how to update A if not fixed
    
    B = (Sxu10 - A*Sxu00)*(1/input00)
    Q = (S11 + A^2*S00 + B^2*input00 - 2*A*S10 + 2*A*B*Sxu00- 2*B*Sxu10)/num
    
    mu0 = ks$x0n
    Sigma0 = ks$P0n
    
    # Check expected log likelihood
    
    like[iter] = -1/2*sum((t(y)-C*ks$xs)^2)/R - num/2*log(abs(R)) -
      1/2*sum( ((ks$xs[2:num])-A*ks$xs[1:num-1] - c(B)*input[1:num-1])^2 ) +
      ( (ks$xs[1]-A*mu0-B*input0)^2 )/Q -
      num/2*log(abs(Q)) -
      1/2*((ks$x0n-mu0)^2)/Sigma0 -
      1/2*log(abs(Sigma0)) -
      num/2*log(2*pi)
    
    
    if (iter > 1){
      cvg = abs((like[iter - 1] - like[iter])/abs(like[iter - 1]))
      if (cvg < 0) 
        stop("Likelihood Not Increasing") # Something is wrong. Stop EM estimation
      if (abs(cvg) < tol) # Threshold reached
        break
      }
  
  }
  
  list(A = A, B = B, C= C, Q = Q, R = R, mu0 = mu0, Sigma0 = Sigma0,  
       like = like[1:iter], niter = iter, cvg = cvg, ks = ks) 
  
}