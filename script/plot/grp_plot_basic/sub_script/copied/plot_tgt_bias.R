# plot target location-dependent bias
# This uses all trials in Baseline block. 
# Since there aren't enough null trials to reliably estimate bias, 
# we use all Baseline block trials (except S, where there is an arc instead of target) although they include m trials and probe-rotation trials.
# It was how bias was calculated back in the "original" processing/plotting years ago.

yrange_a <- c(-20,20)

df_bias <- df_data %>% 
  dplyr::filter(blk == pre_blk, tsize > 0) # extract baseline while removing S trials (no tgt)


sdf_bias <- df_bias %>% 
  group_by(tgt_f) %>% 
  summarise(m = mean(herr, na.rm=T), sd = sd(herr)) %>% 
  ungroup()



plot1.pre <- ggplot(sdf_bias, aes(x = tgt_f)) +
  geom_hline(yintercept = 0, color = "gray") +
  geom_point(data=df_bias, aes(y=herr), size =.5, alpha = .5, color = "gray") +
  geom_point(aes(y = m)) +
  geom_line(aes(y = m, group=NA), linetype ="31")
  # geom_errorbar(aes(ymin=m-sd, ymax=m+sd), width=.3) +

plot1 <- format_gg(plot1.pre, xlabel = "Target [deg]", ylabel = "Bias (Tgt - Hand) [deg]", 
                      ptitle = sprintf("Target Bias (Mean Hand Error in Baseline)"), pos.leg = "bl",
                      ylimit = yrange_a, 
                      expand_coord = T)

save_plots(tgt_plot = plot1, fname = sprintf("tgt_bias"), pdf_only = T, readme_content = "")

# for (i in 1:length(plot_list))
#   save_plots(tgt_plot = plot_list[[i]], fname = sprintf("hand_block_%d",i), pdf_only = T, readme_content = "", png_only = T)

