yrange_a <- c(-20,20)
nr <-  1  # number of rows
nc <-  1 # number of columns

plot_hand_blk <- function(boi){
  
  tmp_df <- subset(df_data, blk == boi)
  
  tmp_plot.pre <- ggplot(tmp_df, aes(x = blk_tri)) +
    geom_line(aes(y=rot), color="orange") +
    geom_line(aes(y=herr), color="black") +
    geom_point(aes(y=herr, shape = m_tri)) +
    scale_shape_manual(values=c(1,16), drop = F)
  
  tmp_plot <- format_gg(tmp_plot.pre, xlabel = "Trial", ylabel = "Hand Error (Tgt - Hand) [deg]", 
                     ptitle = sprintf("Block %d",boi), pos.leg = "bl",
                     xlimit=(range(tmp_df$blk_tri)+c(-1,1)), ylimit = yrange_a, 
                     expand_coord = F)
  
}


plot_list <- plyr::alply(unique(df_data$blk), 1, plot_hand_blk)
plot_list_save = marrangeGrob(plot_list, nrow = nr, ncol = nc, layout_matrix = matrix(1:(nr*nc),nr,nc,T),
                              top = sprintf("Hand Profile %s", ind_tag),
                              left = "",
                              bottom = "") # convert the list of plots



save_plots(tgt_plot = plot_list_save, fname = sprintf("hand_block"), pdf_only = T, readme_content = "")


## Coloring by target


plot_hand_blk_color <- function(boi){
  
  tmp_df <- subset(df_data, blk == boi)
  
  tmp_plot.pre <- ggplot(tmp_df, aes(x = blk_tri)) +
    geom_line(aes(y=rot), color="orange") +
    geom_line(aes(y=herr), color="black") +
    geom_point(aes(y=herr, shape = m_tri, color = tgt_f)) +
    scale_shape_manual(values=c(1,16), drop = F)
  
  tmp_plot <- format_gg(tmp_plot.pre, xlabel = "Trial", ylabel = "Direction [deg]", 
                        ptitle = sprintf("Block %d",boi), pos.leg = "bl", leg.dir = "horizontal",
                        xlimit=(range(tmp_df$blk_tri)+c(-1,1)), ylimit = yrange_a, 
                        expand_coord = F)
  
}


plot_list2 <- plyr::alply(unique(df_data$blk), 1, plot_hand_blk_color)
plot_list2_save = marrangeGrob(plot_list2, nrow = nr, ncol = nc, layout_matrix = matrix(1:(nr*nc),nr,nc,T),
                              top = sprintf("Hand Profile %s", ind_tag),
                              left = "",
                              bottom = "") # convert the list of plots

save_plots(tgt_plot = plot_list2_save, fname = sprintf("hand_block_tgt"), pdf_only = T, readme_content = "")

# for (i in 1:length(plot_list))
#   save_plots(tgt_plot = plot_list[[i]], fname = sprintf("hand_block_%d",i), pdf_only = T, readme_content = "", png_only = T)

