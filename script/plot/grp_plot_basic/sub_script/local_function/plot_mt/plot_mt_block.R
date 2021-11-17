yrange_a <- c(0,.5)
local_fname_tag <- "mt_block"

df_mt <- df_mt_raw %>% 
  dplyr::filter(blk %in% main_blk) %>% 
  group_by(sub_id,blk, cond) %>% 
  summarise(ind_m = mean(time_elapse, na.rm=T), n = length(time_elapse[!is.na(time_elapse)])) %>% 
  ungroup()


sdf_mt <- df_mt %>% 
  group_by(blk, cond) %>% 
  summarise(m = mean(ind_m, na.rm=T), sd = sd(ind_m, na.rm=T), se = sd(ind_m, na.rm=T)/sqrt(length(ind_m))) %>% 
  ungroup()



plot1.pre <- ggplot(sdf_mt, aes(x = blk, color = cond)) +
  geom_hline(yintercept = 0, color = "gray") +
  geom_point(aes(y = m)) +
  geom_errorbar(aes(ymin=m-se, ymax=m+se), width = .3) +
  geom_line(aes(y = m, group = cond), linetype ="31")
# geom_errorbar(aes(ymin=m-sd, ymax=m+sd), width=.3) +

plot1 <- format_gg(plot1.pre, xlabel = "Block", ylabel = "Mean Time [s]", 
                   ptitle = sprintf("Group Mean & SE Time from Initiation"), pos.leg = "tr", leg.dir = "h",
                   ylimit = yrange_a, pcol = pcol_4grp,
                   expand_coord = T)

save_plots(tgt_plot = plot1, fname = sprintf("%s",local_fname_tag), mdir = sub_mdir, sdir = sub_sdir, pdf_only = T, readme_content = "")


# each group separately + individual mean



plot_list <- lapply(unique(sdf_mt$cond), function(coi){
  
  
  tmp_df <- subset(df_mt, cond == coi)
  tmp_sdf <- subset(sdf_mt, cond == coi)
  
  tmp_plot.pre <- ggplot(tmp_sdf, aes(x =blk))+
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
                               top = sprintf("Group Mean & SE Time from Initiation"),
                               left = "Mean Time [s]",
                               bottom = "Block") # convert the list of plots



save_plots(tgt_plot = plot_list_save, fname = sprintf("%s_each_cond",local_fname_tag), 
           mdir = sub_mdir, sdir = sub_sdir,
           pdf_only = T, readme_content = "")
