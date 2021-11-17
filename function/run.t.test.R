# perform wilcoxon signed rank test and return a list of 
# v : V-statistics
# p : p-value
# es : effect size (Cohen's d)

run.t.test <- function (vec1, vec2 = NULL, na.if.violate = T){
  
  library(effsize) # to compute Cohen's d
  
  
  violate <- 0 # flag
  
  ## warning if it vilotes assumptions
  if (shapiro.test(vec1)$p < 0.05){
    warning("run.t.test: normality assumption violated for the 1st data set.")
    violate <- 1
    normality <- "No"
  } else {
    normality <- "Yes"
  }
  
  # compare with hypothetical mean of 0
  if (is.null(vec2)){
    res <- t.test(vec1)
    es = cohen.d(vec1, NA)
    equal_var <- "NA"
  } else {
    
    if (shapiro.test(vec2)$p < 0.05){
      warning("run.t.test: normality assumption violated for the 2nd data set.")
      violate <- 1
      normality <- "No"
    } 
    
    if (var.test(vec1,vec2)$p.value < 0.05){
      warning("run.t.test: equal variance assumption violated")     
      violate <- 1
      equal_var <- "No"
    } else {
      equal_var <- "Yes"
    }
      
    res <- t.test(vec1, vec2)
    es = cohen.d(vec1, vec2)
  }
  
  p = res$p.value
  v = as.numeric(res$statistic)
  
  z_val = qnorm(p) # z-value
  
  
  if (violate == 1 & na.if.violate) {
    l <- list(p = NA, v = NA, es = NA, normality = normality, equal_variance = equal_var)
  } else {
    l <- list(p = p, v = v, es = es$estimate, normality = normality, equal_variance = equal_var)
  }
  
  return(l)
  
}