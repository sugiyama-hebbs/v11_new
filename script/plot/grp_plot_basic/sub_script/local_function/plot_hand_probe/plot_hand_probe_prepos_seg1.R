yrange_a <- c(-10,10)
local_fname_tag <- "seg1"

source(sprintf("script/plot/%s/sub_script/local_function/count_num_each_cond.R",main_dir)) 

prb_tri_seg1 <- 6:15 # hard coding. This works only when prb_pre_null_inc = 10, so adjust this if you change it 

df_data_probe_ind_seg1 <- df_data_probe_ind %>% 
  dplyr::filter(prb_tri %in% prb_tri_seg1, blk %in% c(main_blk[1],main_blk[length(main_blk)]))

df_data_probe_seg1 <- df_data_probe_ind_seg1 %>% 
  dplyr::rename(voi = herr, tri = prb_tri) %>% 
  summarise_by_trial_cond()

df_data_probe_seg1_td <- df_data_probe_ind_seg1 %>% 
  dplyr::rename(voi = herr, tri = prb_tri) %>% 
  summarise_by_trial_td()


plot_list <- lapply(as.character(unique(df_data_probe_seg1$cond)), function(coi){

  if (coi == "LR"){
    pcol_id <- 1
  } else if (coi == "LP"){
    pcol_id <- 2
  }else if (coi == "NR"){
    pcol_id <- 3
  }else if (coi == "NP"){
    pcol_id <- 4
  }
  
  pcol_prepos <- c("gray",pcol_4grp[pcol_id])
  
  tmp_df <- subset(df_data_probe_seg1, cond == coi) %>% 
    mutate(prepos = factor(ifelse(blk == pre_blk,"Pre","Pos"), levels=c("Pre","Pos")))
  
  tmp_plot.pre <- ggplot(tmp_df, aes(x = tri, color = prepos, group = prepos)) +
    geom_line(aes(y=rot), color="orange", alpha = .3) +
    geom_ribbon(aes(ymin=m-se,ymax=m+se, fill = prepos), alpha = .3, color = NA) +
    geom_line(aes(y=m), alpha = .8) +
    geom_point(aes(y=m), alpha = .8) 
  
  
  tmp_plot <- format_gg(tmp_plot.pre, xlabel = " ", ylabel = "", 
                        ptitle = sprintf(coi), show.leg = F,
                        pcol = pcol_prepos, fcol = pcol_prepos, col_name = "Condition",
                        xlimit=(range(tmp_df$tri)+c(-1,1)), ylimit = yrange_a, 
                        expand_coord = F)
  
})


nr <- 2
nc <- 2

plot_list_save = marrangeGrob(plot_list, nrow = nr, ncol = nc, layout_matrix = matrix(1:(nr*nc),nr,nc,T),
                              top = sprintf("Group Pre vs Pos Mean & SE Probe Seg1 Hand"),
                              left = "Hand Error (Tgt - Hand) [deg] (Pre=Gray, Pos=Colored)",
                              bottom = "Trial") # convert the list of plots



save_plots(tgt_plot = plot_list_save, fname = sprintf("%s",local_fname_tag), 
           mdir = sub_mdir, sdir = sub_sdir,
           pdf_only = T, readme_content = "")

## by task demand
plot_list_td <- lapply(as.character(unique(df_data_probe_seg1_td$td)), function(tdoi){
  
  if (tdoi == "Lrn"){
    pcol_id <- 1
  } else if (tdoi == "NLrn"){
    pcol_id <- 2
  }
  
  pcol_prepos <- c("gray",pcol_td[pcol_id])
  
  tmp_df <- subset(df_data_probe_seg1_td, td == tdoi) %>% 
    mutate(prepos = factor(ifelse(blk == pre_blk,"Pre","Pos"), levels=c("Pre","Pos")))
  
  tmp_plot.pre <- ggplot(tmp_df, aes(x = tri, color = prepos, group = prepos)) +
    geom_line(aes(y=rot), color="orange", alpha = .3) +
    geom_ribbon(aes(ymin=m-se,ymax=m+se, fill = prepos), alpha = .3, color = NA) +
    geom_line(aes(y=m), alpha = .8) +
    geom_point(aes(y=m), alpha = .8) 
  
  
  tmp_plot <- format_gg(tmp_plot.pre, xlabel = " ", ylabel = "", 
                        ptitle = tdoi, show.leg = F,
                        pcol = pcol_prepos, fcol = pcol_prepos, col_name = "Condition",
                        xlimit=(range(tmp_df$tri)+c(-1,1)), ylimit = yrange_a, 
                        expand_coord = F)
  
})

## by task demand, lrn vs nlrn
plot_list_td2 <- lapply(c(pre_blk,pos_blk), function(boi){
  

  tmp_df <- subset(df_data_probe_seg1_td, blk == boi) 
  
  tmp_plot.pre <- ggplot(tmp_df, aes(x = tri, color = td, group = td)) +
    geom_line(aes(y=rot), color="orange", alpha = .3) +
    geom_ribbon(aes(ymin=m-se,ymax=m+se, fill = td), alpha = .3, color = NA) +
    geom_line(aes(y=m), alpha = .8) +
    geom_point(aes(y=m), alpha = .8) 
  
  if (boi == pre_blk){
    title_tag <- "Pre"
  } else {
    title_tag <- "Pos"   
  }
  
  tmp_plot <- format_gg(tmp_plot.pre, xlabel = " ", ylabel = "", 
                        ptitle = title_tag, show.leg = F,
                        pcol = pcol_td, fcol = pcol_td, col_name = "Condition",
                        xlimit=(range(tmp_df$tri)+c(-1,1)), ylimit = yrange_a, 
                        expand_coord = F)
  
})


nr <- 2
nc <- 2

plot_list_td_save = marrangeGrob(c(plot_list_td,plot_list_td2), nrow = nr, ncol = nc, layout_matrix = matrix(1:(nr*nc),nr,nc,T),
                                 top = sprintf("Group Pre vs Pos Mean & SE Probe Seg1 Hand"),
                                 left = "Hand Error (Tgt - Hand) [deg]",
                                 bottom = "Trial") # convert the list of plots



save_plots(tgt_plot = plot_list_td_save, fname = sprintf("%s_td",local_fname_tag), 
           mdir = sub_mdir, sdir = sub_sdir,
           pdf_only = T, readme_content = "")

