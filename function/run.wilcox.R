# perform wilcoxon signed rank test and return a list of 
# v : V-statistics
# p : p-value
# es : effect size (Cohen's d)

run.wilcox <- function (vec1, vec2 = NULL){
  
  # compare with hypothetical mean of 0
  if (is.null(vec2)){
    res <- wilcox.test(vec1, mean = 0)
  } else {
    res <- wilcox.test(vec1, vec2)
  }
  
  p = res$p.value
  v = as.numeric(res$statistic)
  
  z_val = qnorm(p) # z-value
  
  es = abs(z_val)/sqrt(length(vec1))
  
  l <- list(p = p, v = v, es = es)
  
  return(l)
  
}