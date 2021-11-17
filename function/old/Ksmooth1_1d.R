# This is a modifed Ksmooth1 function that takes 1-dimensional input and output.

Ksmooth1_1d <- function (num, y, mu0, Sigma0, A, B, C, Q, R, input) 
{
  
  source("function/Kfilter1_1d.R")
  
  kf = Kfilter1_1d(num, y, mu0, Sigma0, A, B, C, Q, R, input)
  
  pdim = nrow(as.matrix(A))
  xs = array(NA, dim = c(1, num))
  Ps = array(NA, dim = c(1, num))
  J = array(NA, dim = c(1, num))
  
  # Backward recursion to smooth the data 
  xs[, num] = kf$xf[num] # In the last time step, smoothed and filtered values match
  Ps[, num] = kf$Pf[num] # In the last time step, smoothed and filtered values match
  
  for (k in num:2) {
    J[k - 1] = (kf$Pf[k - 1] %*% t(A)) %*% (1/kf$Pp[k])
    xs[k - 1] = kf$xf[k - 1] + J[k - 1] %*% (xs[k] - kf$xp[k])
    Ps[k - 1] = kf$Pf[k - 1] + J[k - 1] %*% (Ps[k] - kf$Pp[k]) %*% t(J[k - 1])
  }
  
  x00 = mu0
  P00 = Sigma0
  
  J0 = as.matrix((P00 %*% t(A)) %*% (1/(kf$Pp[1])), nrow = 1, ncol = 1)
  x0n = as.matrix(x00 + J0 %*% (xs[1] - kf$xp[1]), nrow = 1, ncol = 1)
  P0n = P00 + J0 %*% (Ps[1] - kf$Pp[1]) %*% t(J0)
  list(xs = xs, Ps = Ps, x0n = x0n, P0n = P0n, J0 = J0, J = J, 
       xp = kf$xp, Pp = kf$Pp, xf = kf$xf, Pf = kf$Pf, Kn = kf$K)
}