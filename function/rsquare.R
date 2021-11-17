# Calculate coefficient of determination (R^2) for goodness of fitting
#### input ####
##
## actual: actual data
## predict: predicted data (model estimate)
##
#### output ####
##
## rsq: r-square  (coefficient of determination)
## rmse: root mean squared error
##
#### Authour ####
## Taisei Sugiyama


rsquare = function(actual, predict){
  
  y = actual # actual data
  f = predict # fit
  
  # added as of 3/4/2021
  y <- y[!is.na(y)]
  f <- f[!is.na(f)]
  # f <- f[!is.na(y)]
  
  # Goodness of fitting
  r2 = 1 - sum((y-f)^2)/sum((y-mean(y))^2)
  
  # check
  if (r2 < 0){
    r2 = 0
  }
  
  # RMSE
  rmse = sqrt(mean((y - f)^2, na.rm = TRUE)) # root mean squared error
  
  gof = list(r2 = r2, rmse = rmse)
  
  return(gof)
  
}