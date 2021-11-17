nr <-  1  # number of rows
nc <-  1 # number of columns

local_fname_tag <- "block"

plot_hand_blk <- function(boi){
  
  tmp_df <- subset(df_data_train, blk == boi & tsize != 0)
  tmp_df_rot <- subset(df_data_train, blk == boi)
  
  tmp_plot.pre <- ggplot(tmp_df, aes(x = tri, color = cond, group = cond)) +
    geom_line(data = tmp_df_rot, aes(y=rot), color="orange", alpha = .3) +
    geom_line(aes(y=m), alpha = .8) +
    geom_point(aes(y=m), alpha = .8) 
  
  tmp_plot <- format_gg(tmp_plot.pre, xlabel = "Trial", ylabel = "Hand Error (Tgt - Hand) [deg]", 
                     ptitle = sprintf("Blk %d (%s)",boi, probe_label[boi]), pos.leg = "bl", leg.dir = "horizontal",  
                     pcol = pcol_4grp, col_name = "Condition",
                     xlimit=(range(tmp_df$tri)+c(-1,1)), ylimit = yrange_a, 
                     expand_coord = F)
  
}


plot_list <- plyr::alply(unique(df_data_train$blk), 1, plot_hand_blk)
plot_list_save = marrangeGrob(plot_list, nrow = nr, ncol = nc, layout_matrix = matrix(1:(nr*nc),nr,nc,T),
                              top = sprintf("Group Mean & SE Probe Hand Profile"),
                              left = "",
                              bottom = "") # convert the list of plots



save_plots(tgt_plot = plot_list_save, fname = sprintf("%s",local_fname_tag), 
           mdir = sub_mdir, sdir = sub_sdir,
           pdf_only = T, readme_content = "")

## color by task demand


plot_hand_blk_td <- function(boi){
  
  tmp_df <- subset(df_data_train_td, blk == boi & tsize != 0)
  tmp_df_rot <- subset(df_data_train_td, blk == boi)

  tmp_plot.pre <- ggplot(tmp_df, aes(x = tri, color = td, group = td)) +
    geom_line(data= tmp_df_rot, aes(y=rot), color="orange", alpha = .3) +
    geom_ribbon(aes(ymin=m-se,ymax=m+se, fill = td), color = NA, alpha = .3) +
    geom_line(aes(y=m), alpha = .8) +
    geom_point(aes(y=m), alpha = .8) +
    guides(fill = F)
  
  tmp_plot <- format_gg(tmp_plot.pre, xlabel = "Trial", ylabel = "Hand Error (Tgt - Hand) [deg]", 
                        ptitle = sprintf("Blk %d (%s)",boi, probe_label[boi]), pos.leg = "bl", leg.dir = "horizontal",  
                        pcol = pcol_td, fcol = pcol_td, col_name = "Condition",
                        xlimit=(range(tmp_df$tri)+c(-1,1)), ylimit = yrange_a, 
                        expand_coord = F)
  
}


plot_list2 <- plyr::alply(unique(df_data_train$blk), 1, plot_hand_blk_td)
plot_list2_save = marrangeGrob(plot_list2, nrow = nr, ncol = nc, layout_matrix = matrix(1:(nr*nc),nr,nc,T),
                              top = sprintf("Group Mean & SE Probe Hand Profile"),
                              left = "",
                              bottom = "") # convert the list of plots



save_plots(tgt_plot = plot_list2_save, fname = sprintf("%s_td",local_fname_tag), 
           mdir = sub_mdir, sdir = sub_sdir,
           pdf_only = T, readme_content = "")


