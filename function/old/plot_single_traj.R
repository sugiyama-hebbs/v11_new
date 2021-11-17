## This function plots a movement trajectory in a single trial.
## 
#### Input ####
## data_align: alinged data containing x, y, blk, and blk_tri
## blk: block number
## tri: (block) trial number 
## Author: Taisei Sugiyama

plot_single_traj <- function(data_align, data_tgt, data_para, boi, tri){
  
  library(dplyr)
  library(ggplot2)
  data_plot = data_align %>%
    dplyr::filter(blk == boi & blk_tri == tri) %>%
    select(blk,blk_tri,x_mm_adj,y_mm_adj,speed_rbt,speed_sgolay,
           x_mm_adj_vis, y_mm_adj_vis) 
  
  trad = data_para$trad[blk]*1000 # Target radius (also convert to mm)
  
  tgt_plot = data_tgt %>%
    dplyr::filter(blk == boi & blk_tri == tri) %>%
    mutate(tgt_x = trad*cosd(.$tgt)) %>% # Target x-coord
    mutate(tgt_y = trad*sind(.$tgt)) # Target y-coord
  
  cols = c("Hand"="#0000ff","Cur"="#ff0000")
  
  traj.plot = ggplot()+
    geom_path(data = data_plot, aes(x=x_mm_adj, y=y_mm_adj , colour="Hand")) + # hand path
    geom_path(data = data_plot, aes(x=x_mm_adj_vis, y=y_mm_adj_vis , colour="Cur")) + # cursor path
    
    xlab("x (mm)") +
    ylab("y (mm)") +
    xlim(-100,100) +
    ylim(-20,180) + 
    theme(aspect.ratio = 1) +
    scale_color_manual(name="Path",values = cols)
  
  tmp = dplyr::filter(data_tgt, blk == boi, blk_tri == tri)
  
  if (tmp$tsize != 0){
    traj.plot = traj.plot + geom_tile(data = tgt_plot, aes(x=tgt_x, y=tgt_y, height = 5, width = 5)) # target
  }
  
  traj.plot # Now show it
  
  
}