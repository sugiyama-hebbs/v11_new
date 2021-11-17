# Calculate R2 for linear model
r2_lm <- function(m) {
    lmfit <-  lm(model.response(model.frame(m)) ~ fitted(m))
    summary(lmfit)$r.squared
}