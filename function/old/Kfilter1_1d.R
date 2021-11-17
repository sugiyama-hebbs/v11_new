# This is a modifed Kfilter1 function that takes 1-dimensional input and output.

Kfilter1_1d <- function (num, y, mu0, Sigma0, A, B, C, Q, R, input) 
{
  A = as.matrix(A)
  pdim = nrow(A)
  y = as.matrix(y)
  qdim = ncol(y)
  rdim = ncol(as.matrix(input))
  B = as.matrix(B)
  ut = input
  xp = array(NA, dim = c(1, num)) # predicted correspond to x(t-1,t) in MATLAB code
  Pp = array(NA, dim = c(1, num))
  xf = array(NA, dim = c(1, num)) # filtered correspond to x(t,t) in MATLAB code
  Pf = array(NA, dim = c(1, num))
  innov = array(NA, dim = c( 1, num))
  sig = array(NA, dim = c(1, num))
  x00 = mu0
  P00 = Sigma0
  
  # Get values at t = 1 
  xp[1] = A %*% x00 + B %*% ut[1]
  Pp[1] = A %*% P00 %*% t(A) + Q
  K = Pp[1] %*% t(C) %*% (1/(C*Pp[1]*t(C)+R))
  innov[1] = y[1] - C %*% xp[1]
  xf[1] = xp[1] + K %*% innov[1]
  Pf[1] = Pp[1] - K %*% C %*% Pp[1]

  for (i in 2:num) {
    if (num < 2) 
      break
    xp[i] = A %*% xf[i - 1] + B %*% ut[i]
    Pp[i] = A %*% Pf[i - 1] %*% t(A) + Q
    K = Pp[i] %*% t(C) %*% (1/(C*Pp[i]*t(C)+R))
    innov[i] = y[i] - C %*% xp[i] 
    xf[i] = xp[i] + K %*% innov[i]
    Pf[i] = Pp[i] - K %*% C %*% Pp[i]
  }
  
  list(xp = xp, Pp = Pp, xf = xf, Pf = Pf, innov = innov, Kn = K)
}