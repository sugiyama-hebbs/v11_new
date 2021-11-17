yrange_a <- c(-20,20)
nr <-  1  # number of rows
nc <-  1 # number of columns

sub_mdir <- sprintf("%s/%s",main_dir,sub_dir) # sub-main directory name
sub_sdir <- "hand_block" # sub-sub directory name

source("function/create_sub_dir.R")
create_sub_dir("figure",main_dir)
create_sub_dir(sprintf("figure/%s",main_dir),sub_dir)

plot_hand_blk <- function(boi){
  
  tmp_df <- subset(df_data, blk == boi  & tsize > 0)
  tmp_rot <- subset(df_data, blk == boi)
  
  tmp_plot.pre <- ggplot(tmp_df, aes(x = tri, color = cond, group = cond)) +
    geom_line(data = tmp_rot, aes(y=rot), color="orange", alpha = .3) +
    geom_line(aes(y=m), alpha = .8) +
    geom_point(aes(y=m), alpha = .8) 
  
  tmp_plot <- format_gg(tmp_plot.pre, xlabel = "Trial", ylabel = "Hand Error (Tgt - Hand) [deg]", 
                     ptitle = sprintf("Block %d",boi), pos.leg = "bl", leg.dir = "horizontal",  
                     pcol = pcol_4grp, col_name = "Condition",
                     xlimit=(range(tmp_df$tri)+c(-1,1)), ylimit = yrange_a, 
                     expand_coord = F)
  
}


plot_list <- plyr::alply(unique(df_data$blk), 1, plot_hand_blk)
plot_list_save = marrangeGrob(plot_list, nrow = nr, ncol = nc, layout_matrix = matrix(1:(nr*nc),nr,nc,T),
                              top = sprintf("Group Mean & SE Hand Profile"),
                              left = "",
                              bottom = "") # convert the list of plots



save_plots(tgt_plot = plot_list_save, fname = sprintf("hand_block"), 
           mdir = sub_mdir, sdir = sub_sdir,
           pdf_only = T, readme_content = "")

## color by task demand


plot_hand_blk_td <- function(boi){
  
  tmp_df <- subset(df_data_td, blk == boi & tsize > 0)
  tmp_rot <- subset(df_data, blk == boi)
  
  tmp_plot.pre <- ggplot(tmp_df, aes(x = tri, color = td, group = td)) +
    geom_line(data = tmp_rot, aes(y=rot), color="orange", alpha = .3) +
    geom_line(aes(y=m), alpha = .8) +
    geom_point(aes(y=m), alpha = .8)
  
  tmp_plot <- format_gg(tmp_plot.pre, xlabel = "Trial", ylabel = "Hand Error (Tgt - Hand) [deg]", 
                        ptitle = sprintf("Block %d",boi), pos.leg = "bl", leg.dir = "horizontal",  
                        pcol = pcol_td, col_name = "Condition",
                        xlimit=(range(tmp_df$tri)+c(-1,1)), ylimit = yrange_a, 
                        expand_coord = F)
  
}


plot_list2 <- plyr::alply(unique(df_data$blk), 1, plot_hand_blk_td)
plot_list2_save = marrangeGrob(plot_list2, nrow = nr, ncol = nc, layout_matrix = matrix(1:(nr*nc),nr,nc,T),
                              top = sprintf("Group Mean & SE Hand Profile"),
                              left = "",
                              bottom = "") # convert the list of plots



save_plots(tgt_plot = plot_list2_save, fname = sprintf("hand_block_td"), 
           mdir = sub_mdir, sdir = sub_sdir,
           pdf_only = T, readme_content = "")


# for (i in 1:length(plot_list))
#   save_plots(tgt_plot = plot_list[[i]], fname = sprintf("hand_block_%d",i), pdf_only = T, readme_content = "", png_only = T)

