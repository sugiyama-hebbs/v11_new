# https://stackoverflow.com/questions/7549694/add-regression-line-equation-and-r2-on-graph
# GET EQUATION AND R-SQUARED AS STRING
# SOURCE: https://groups.google.com/forum/#!topic/ggplot2/1TgH-kG5XMA
# https://stackoverflow.com/questions/5587676/pull-out-p-values-and-r-squared-from-a-linear-regression
# edited for my purpose

lm_eqn <- function(df, bias = T){
  
  if (bias){
    m <- lm(y ~ x, df)
    
    # get overall p-value (model p-value, not independent p for intercept or slope)
    f <- summary(m)$fstatistic
    p <- pf(f[1],f[2],f[3],lower.tail=F)
    attributes(p) <- NULL
    
    if(p < 0.001){
      p_lab <- sprintf("<10^%d",(ceiling(log10(p))))
    } else {
      p_lab <- sprintf("%1.3f",p)
    }
    
    return_label <- sprintf("y = %1.2fx+%1.2f, R2 = %1.2f, p = %s",coef(m)[1],coef(m)[2], summary(m)$r.squared, p_lab)
  } else {
    m <- lm(y ~ x+0, df)
    # get overall p-value (model p-value, not independent p for intercept or slope)
    f <- summary(m)$fstatistic
    p <- pf(f[1],f[2],f[3],lower.tail=F)
    attributes(p) <- NULL
    
    if(p < 0.001){
      p_lab <- sprintf("<10^%d",(ceiling(log10(p))))
    } else {
      p_lab <- sprintf("%1.3f",p)
    }
    
    return_label <- sprintf("y = %1.2fx, R2 = %1.2f, p = %s",coef(m)[1], summary(m)$r.squared, p_lab)
  }
  
}
