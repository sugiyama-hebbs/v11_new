# score_func_lrn <- function(df){
score_func_lr <- function(dh,rot){
  
  max_score <- 20
  min_score <- 0
  base_penalty <- 10
  reward_zone <- 0.7
  bonus_score <- 0
  
  # accum_factor <- ifelse(count_s >= 5, 3, ifelse(count_s >= 3, 2, 1))
  accum_factor <- 1
  difficulty <- 1
  adapt_ratio <- dh/(rot*accum_factor)
  
  
  num_toi <- length(rot)
  score <- rep(0,num_toi)
  
  target_beta <-1.0
  
  for (tri in 1:num_toi){
    
    # if (adapt_ratio[tri] >= target_beta){
    #   score[tri] <- max_score
    # } else {
    #   # score[tri] <- max_score - floor(abs(reward_zone*(target_beta - adapt_ratio[tri])*difficulty[tri])) + bonus_score
    #   # score[tri] <- max_score - ceiling(abs((dh[tri] - rot[tri]))/reward_zone) + bonus_score
    #   score[tri] <- max_score - ceiling(abs((dh[tri] - rot[tri]))/reward_zone) + bonus_score
    #   # score[tri] <- max_score - ceiling((target_beta - adapt_ratio[tri])*difficulty) + bonus_score
    # }    
    
    
    score[tri] <- ceiling(max_score - (1 - adapt_ratio[tri])*10) + bonus_score
    # score[tri] <- max_score - ceiling(abs((dh[tri] - rot[tri])/-0.7)) + bonus_score

    
    if (score[tri] < min_score){
      score[tri] <-  min_score
    } else if (score[tri] > max_score){
      score[tri] <-  max_score
    }
    
  }
  
  calc_score <- score
  return(calc_score)
  
}