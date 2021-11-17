### Preparation ###
source("function/score_func/score_func_lr.R")
source("function/score_func/score_func_lp.R")
source("function/score_func/score_func_nr.R")
source("function/score_func/score_func_np.R")

if (cond_tag %in% c("LR","NR")){
  yrange_a <- c(0,20)
} else {
  yrange_a <- c(-20,0)
}


### Organize data
df_score <- df_data_train %>% 
  dplyr::select(-cur, -tgt_f, -terr, -show_cur, - tsize, -block_phase, -train_type) %>% 
  group_by(total_cycle) %>% 
  mutate(ref_rot = rot[1], cond = cond_tag, dh_for_score = tgt - hand_v) %>% 
  ungroup() %>% 
  dplyr::filter(m_tri == "y") %>% 
  mutate(calc_score = ifelse(cond == "LR", score_func_lr(dh_for_score, ref_rot),
                             ifelse(cond == "LP", score_func_lp(dh_for_score, ref_rot),
                                    ifelse(cond == "NR", score_func_nr(dh_for_score, ref_rot),
                                           ifelse(cond == "NP", score_func_np(dh_for_score, ref_rot),NA))))) %>% 
  
  group_by(total_cycle) %>% 
  mutate(m_trial = row_number()) %>% 
  ungroup()


check_score <- df_score %>% 
  dplyr::filter(blk %in% train_blk) %>% 
  mutate(calc_dif =gain - calc_score)


# if (max(abs(check_score$calc_dif))>1){
#   warning("There is a considerable difference between calculated score and actual score during the experiment. Make sure you set the appropriate calculation function")
# } else if (max(abs(check_score$calc_dif))==1){
#   warning("There is minor difference between calculated score and actual score during the experiment. This could be just due to rounding, but make sure it's not mis-calculation")
# }


#### Plot

## trial-by-trial

plot_blk <- function(boi){
  
  tmp_df <- subset(df_score, blk == boi)
  
  tmp_plot.pre <- ggplot(tmp_df, aes(x = train_tri)) +
    geom_line(aes(y=calc_score), color="black") +
    geom_point(aes(y=calc_score)) 
  
  tmp_plot <- format_gg(tmp_plot.pre, xlabel = "Trial", ylabel = "Score", 
                        ptitle = sprintf("Block %d",boi), pos.leg = "bl",
                        xlimit=(range(tmp_df$train_tri)+c(-1,1)), ylimit = yrange_a, 
                        expand_coord = T)
  
}

nr <-  1  # number of rows
nc <-  1 # number of columns
plot_list <- plyr::alply(main_blk, 1, plot_blk)
plot_list_save = marrangeGrob(plot_list, nrow = nr, ncol = nc, layout_matrix = matrix(1:(nr*nc),nr,nc,T),
                              top = sprintf("Trial by Trial Score %s", ind_tag),
                              left = "",
                              bottom = "") # convert the list of plots

save_plots(tgt_plot = plot_list_save, fname = sprintf("score"), pdf_only = T, readme_content = "")



## block average
sdf_score <- df_score %>%
  group_by(blk) %>% 
  summarise(m = mean(calc_score, na.rm=T), sd=sd(calc_score,na.rm=T), n = length(calc_score)) %>% 
  ungroup()

plot1.pre <- ggplot(sdf_score, aes(x = blk)) +
  geom_errorbar(aes(ymin = m-sd, ymax=m+sd), size = 1, width = .3) +
  geom_line(aes(y = m), size = 1)

plot1 <- format_gg(plot1.pre, "Block", "Block Score per Trial", "Mean & SD Block Score", ylimit = yrange_a)


## block average, M1
sdf_score_m1 <- df_score %>%
  dplyr::filter(m_trial == 1) %>% 
  group_by(blk) %>% 
  summarise(m = mean(calc_score, na.rm=T), sd=sd(calc_score,na.rm=T), n = length(calc_score)) %>% 
  ungroup()

plot2.pre <- ggplot(sdf_score_m1, aes(x = blk)) +
  geom_errorbar(aes(ymin = m-sd, ymax=m+sd), size = 1, width = .3) +
  geom_line(aes(y = m), size = 1)

plot2 <- format_gg(plot2.pre, "Block", "Block Score per Trial (M1)", "Mean & SD Block Score (M1)", ylimit = yrange_a)

## block average, M4
sdf_score_m4 <- df_score %>%
  dplyr::filter(m_trial == 4) %>% 
  group_by(blk) %>% 
  summarise(m = mean(calc_score, na.rm=T), sd=sd(calc_score,na.rm=T), n = length(calc_score)) %>% 
  ungroup()

plot3.pre <- ggplot(sdf_score_m4, aes(x = blk)) +
  geom_errorbar(aes(ymin = m-sd, ymax=m+sd), size = 1, width = .3) +
  geom_line(aes(y = m), size = 1)

plot3 <- format_gg(plot3.pre, "Block", "Block Score per Trial (M4)", "Mean & SD Block Score (M4)", ylimit = yrange_a)

save_plots(tgt_plot = plot1, fname = sprintf("score_blk"), pdf_only = T, readme_content = "")
save_plots(tgt_plot = plot2, fname = sprintf("score_blk_m1"), pdf_only = T, readme_content = "")
save_plots(tgt_plot = plot3, fname = sprintf("score_blk_m4"), pdf_only = T, readme_content = "")

### Over M
sdf_score_over_m <- df_score %>%
  group_by(blk, m_trial) %>% 
  summarise(m = mean(calc_score, na.rm=T), sd=sd(calc_score,na.rm=T), n = length(calc_score)) %>% 
  ungroup() %>% 
  mutate(blk = factor(blk))

plot4.pre <- ggplot(sdf_score_over_m, aes(m_trial, color = blk)) +
  # geom_errorbar(aes(ymin = m-sd, ymax=m+sd), size = 1, width = .3) +
  geom_line(aes(y = m), size = .75) +
  geom_point(aes(y = m), size = 1)

plot4 <- format_gg(plot4.pre, "M Trial", "Block Score per Trial", "Mean & SD Score over M", ylimit = yrange_a, pos.leg = "bl")

save_plots(tgt_plot = plot4, fname = sprintf("score_blk_over_m"), pdf_only = T, readme_content = "")
