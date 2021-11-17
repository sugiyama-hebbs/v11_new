yrange_a <- c(-20,20)
nr <-  1  # number of rows
nc <-  1 # number of columns

sub_mdir <- sprintf("%s/%s",main_dir,sub_dir) # sub-main directory name
sub_sdir <- "plot_hand_bias" # sub-sub directory name

local_fname_tag <- "bias"

source("function/create_sub_dir.R")
create_sub_dir("figure",main_dir)
create_sub_dir(sprintf("figure/%s",main_dir),sub_dir)

# This uses all trials in Baseline block. 
# Since there aren't enough null trials to reliably estimate bias, 
# we use all Baseline block trials (except S, where there is an arc instead of target) although they include m trials and probe-rotation trials.
# It was how bias was calculated back in the "original" processing/plotting years ago.


df_bias <- df_data_ind %>% 
  dplyr::filter(blk == pre_blk, tsize > 0) %>% # extract baseline while removing S trials (no tgt)
  mutate(tgt_f = factor(tgt)) %>% 
  group_by(sub_id,tgt_f, cond) %>% 
  summarise(ind_m = mean(herr, na.rm=T), n = length(herr)) %>% 
  ungroup()


sdf_bias <- df_bias %>% 
  group_by(cond, tgt_f) %>% 
  summarise(m = mean(ind_m, na.rm=T), sd = sd(ind_m, na.rm=T), se = sd(ind_m, na.rm=T)/sqrt(length(ind_m))) %>% 
  ungroup()



plot1.pre <- ggplot(sdf_bias, aes(x = tgt_f, color = cond)) +
  geom_hline(yintercept = 0, color = "gray") +
  geom_point(aes(y = m)) +
  geom_errorbar(aes(ymin=m-se, ymax=m+se), width = .3) +
  geom_line(aes(y = m, group = cond), linetype ="31")
# geom_errorbar(aes(ymin=m-sd, ymax=m+sd), width=.3) +

plot1 <- format_gg(plot1.pre, xlabel = "Target [deg]", ylabel = "Bias (Tgt - Hand) [deg]", 
                   ptitle = sprintf("Group Mean & SE Target Bias (Mean Error in Baseline)"), pos.leg = "bl",
                   ylimit = yrange_a, pcol = pcol_4grp,
                   expand_coord = T)

save_plots(tgt_plot = plot1, fname = sprintf("%s",local_fname_tag), mdir = sub_mdir, sdir = sub_sdir, pdf_only = T, readme_content = "")


# each group separately + individual mean



plot_list <- lapply(unique(sdf_bias$cond), function(coi){
  
  
  tmp_df <- subset(df_bias, cond == coi)
  tmp_sdf <- subset(sdf_bias, cond == coi)
  
  tmp_plot.pre <- ggplot(tmp_sdf, aes(x =tgt_f))+
    geom_line(data = tmp_df, aes(y = ind_m, group = sub_id), color = "gray", alpha = .5) +
    geom_hline(yintercept = 0, color = "gray") +
    geom_point(aes(y = m), color = pcol_4grp[as.numeric(coi)]) +
    geom_line(aes(y = m, group = NA), color = pcol_4grp[as.numeric(coi)]) 
  
  tmp_plot <- format_gg(tmp_plot.pre, xlabel = "", ylabel = "", 
                     ptitle = sprintf("%s",coi), pos.leg = "bl",
                     ylimit = yrange_a, pcol = pcol_4grp,
                     expand_coord = T)
    
  
  
  
})


nr <- 2
nc <- 2

plot_list_save = marrangeGrob(plot_list, nrow = nr, ncol = nc, layout_matrix = matrix(1:(nr*nc),nr,nc,T),
                               top = sprintf("Group Mean & SE Target Bias (Mean Error in Baseline)"),
                               left = "Bias (Tgt - Hand) [deg]",
                               bottom = "Target [deg]") # convert the list of plots



save_plots(tgt_plot = plot_list_save, fname = sprintf("%s_each_cond",local_fname_tag), 
           mdir = sub_mdir, sdir = sub_sdir,
           pdf_only = T, readme_content = "")
