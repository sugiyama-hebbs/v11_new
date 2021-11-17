# plot trial-by-trial kinematic data

##### Data processing #####
data_plot <- kin_raw %>%
  dplyr::select(total_tri,blk,blk_tri,state, tstep, x, y, vx, vy, sx, sy, svx, svy) %>% 
  left_join(dplyr::select(tgt_raw,blk, blk_tri,tgt, rot), by = c("blk","blk_tri")) %>%
  mutate(tgt = factor(tgt)) %>% 
  mutate(curx = (x*cosd(rot) - y*sind(rot)), cury = (x*sind(rot) + y*cosd(rot)))

# ##### Plotting #####
## Preparation
theme_update(plot.title = element_text(hjust = .5)) # Set default alignment of plot title to be centered
source("function/save_plots.R")
source("function/format_gg.R")
source("function/gg_def_col.R")
source("script/process_ind/miscellaneous/state_list.R")

main_dir = "basic_kin"
sub_dir = "plot_pos"
desc_note <-  c("This plots trial-by-trial trajectory for all the trials in a single block")



# pix_to_mm <-  .2451 * 2 # hard coding.
# tsize_m <- 10*pix_to_mm/1000 # 10 px to mm to m
tsize_m <- 0.01 # hard coding. 1cm

for (boi in bois_plot_kin){
  
  data_plot_sub <- subset(data_plot, blk == boi)
  
  param_raw_tri <- subset(param_raw, blk == boi)
  trad <- param_raw_tri$trad
  
  tois <- subset(point_raw, blk == boi)$blk_tri # trials of interest. Plot all trials 
  
  plot_list <- lapply(tois, function(toi){
    
    
    tgt_raw_tri <- subset(tgt_raw, blk == boi & blk_tri == toi)
    
    tgt_pos <- data.frame(x = trad*cosd(tgt_raw_tri$tgt), y =  trad*sind(tgt_raw_tri$tgt))
    
    tmp_plot.pre <- ggplot(subset(data_plot_sub, blk == boi & blk_tri == toi)) +
      geom_circle(data = tgt_pos, aes(x0=x, y0=y, r = tsize_m), size = .5) +
      geom_path(size = .5, aes(x = curx, y = cury, color = "red")) +
      geom_path(size = .5, aes(x=x, y=y), color = "gray", linetype = "31") + 
      guides(shape = F, color = F) +
      theme(plot.margin=unit(c(1,1,1,1)*0,"pt"))
    
    tmp_plot <- format_gg(tmp_plot.pre, xlabel = "", ylabel = "", 
                          fsize_axis_text = 8,
                          xlimit = c(-.15,.15), ylimit =c(-.15,.15), xticks = NA, yticks = NA,  show.leg = F)
  })
  
  nr = 4 # number of rows
  nc = 4 # number of columns
  
  
  plot_list_save = marrangeGrob(plot_list, nrow = nr, ncol = nc, layout_matrix = matrix(1:(nr*nc),nr,nc,T),
                                top = sprintf("Trajectory [Hand: gray, Cursor: red, Target: black]"),
                                left = "y [m]", bottom = "x [m]") # convert the list of plots
  
  fname_list = sprintf("%s_%s_B%d",sub_dir,tgt_dir, boi)
  save_plots(fname = fname_list, tgt_plot = plot_list_save, pdf_only = T)
  
  ## superimposed
  
  tgt_poss <- data.frame(tgt = unique(tgt_raw$tgt)) %>% 
    mutate(x = trad*cosd(tgt), y =  trad*sind(tgt)) %>% 
    mutate(tgt = factor(tgt))
  
  plot1.pre <- ggplot(data_plot_sub) +
    geom_path(aes(x=x, y=y, group = blk_tri, color = tgt)) +
    geom_circle(data = tgt_poss, aes(x0=x, y0=y, r = tsize_m, group = NA, color = tgt), size = .5)
  
  plot1 <- format_gg(plot1.pre, xlabel = "x [m]", ylabel = "y [m]", 
                     fsize_axis_text = 12,
                     # xlimit = c(-.15,.15), ylimit =c(-.15,.15), xticks = NA, yticks = NA,  show.leg = F)
                     # xlimit = c(-1,1), ylimit =c(-1,1), xticks = NA, yticks = NA,  show.leg = F)
                     ylimit = c(-.01,.15), xlimit =c(-.08,.08), xticks = NA, yticks = NA,  show.leg = F)
  
  save_plots(fname = sprintf("%s_%s_B%d_s.impose",sub_dir,tgt_dir,boi), tgt_plot = plot1, pdf_only = T)
  
  
}
