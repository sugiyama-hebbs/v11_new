yrange_a <- c(-20,20)
nr <-  1  # number of rows
nc <-  1 # number of columns

plot_hand_blk <- function(boi){
  
  tmp_df <- subset(df_data_train, blk == boi)
  
  tmp_plot.pre <- ggplot(tmp_df, aes(x = blk_tri)) +
    geom_line(aes(y=rot), color="orange") +
    geom_line(aes(y=herr), color="black") +
    geom_point(aes(y=herr)) +
    scale_shape_manual(values=c(1,16), drop = F)
  
  tmp_plot <- format_gg(tmp_plot.pre, xlabel = "Trial", ylabel = "Hand Error (Tgt - Hand) [deg]", 
                     ptitle = sprintf("Block %d",boi), pos.leg = "bl",
                     xlimit=(range(tmp_df$blk_tri)+c(-1,1)), ylimit = yrange_a, 
                     expand_coord = F)
  
}


plot_list <- plyr::alply(main_blk, 1, plot_hand_blk)
plot_list_save = marrangeGrob(plot_list, nrow = nr, ncol = nc, layout_matrix = matrix(1:(nr*nc),nr,nc,T),
                              top = sprintf("Hand Profile in Intervention (M Trial) %s", ind_tag),
                              left = "",
                              bottom = "") # convert the list of plots

save_plots(tgt_plot = plot_list_save, fname = sprintf("hand_block_train"), pdf_only = T, readme_content = "")


## Average across Trains
sdf_data_train <- df_data_train %>% 
  dplyr::filter(blk %in% train_blk) %>% 
  group_by(blk_tri,rot) %>% 
  summarise(m = mean(herr, na.rm = T), sd = sd(herr, na.rm =T)) %>% 
  ungroup()


plot1.pre <- ggplot(sdf_data_train, aes(x = blk_tri)) +
  geom_line(aes(y=rot), color="orange") +
  # geom_ribbon(aes(ymin=m-sd, ymax=m+sd), fill="black", color = NA, alpha = .3) +
  geom_line(aes(y=m), color="black") +
  geom_point(aes(y=m)) 


plot1 <- format_gg(plot1.pre, xlabel = "Trial", ylabel = "Mean Hand Error (Tgt - Hand) [deg]", 
                      ptitle = sprintf("Mean Hand in Train Blocks (BL not Included) %s", ind_tag), pos.leg = "bl",
                      xlimit=(range(sdf_data_train$blk_tri)+c(-1,1)), ylimit = yrange_a, 
                      expand_coord = F)

save_plots(tgt_plot = plot1, fname = sprintf("hand_block_train_mean"), pdf_only = T, readme_content = "")
# for (i in 1:length(plot_list))
#   save_plots(tgt_plot = plot_list[[i]], fname = sprintf("hand_block_%d",i), pdf_only = T, readme_content = "", png_only = T)

