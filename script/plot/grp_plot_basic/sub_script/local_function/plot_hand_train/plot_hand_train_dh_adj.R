yrange_a <- c(-2,5)
local_fname_tag <- "dh_adj"

source(sprintf("script/plot/%s/sub_script/local_function/%s/func_summarise_train_dh_adj.R",main_dir,sub_sdir))

df_data_train_dh_ind_raw <- df_data_ind %>% 
  mutate(pre_herr = lag(herr,1), pos_herr = lead(herr,1)) %>% 
  mutate(pre_herr = pre_herr*sign(rot),pos_herr = pos_herr*sign(rot)) %>% 
  mutate(dh = (pos_herr - pre_herr)) %>% 
  dplyr::filter(block_phase == phase_train & tsize == 0) %>% # extract S trial
  dplyr::select(sub_id,cond,blk,blk_tri,herr,tgt,rot,td,valence,pre_herr,pos_herr,dh) %>% 
  mutate(td = factor(td, levels=c("Lrn","NLrn")))


### Pre-S and Pos-S
## 4groups
df_data_train_pre_s_ind <- df_data_train_dh_ind_raw %>% 
  dplyr::rename(voi = pre_herr) %>% 
  summarise_train_dh_preorpos_cond_ind() %>% 
  mutate(trial = "Pre-S")

df_data_train_pos_s_ind <- df_data_train_dh_ind_raw %>% 
  dplyr::rename(voi = pos_herr) %>% 
  summarise_train_dh_preorpos_cond_ind() %>% 
  mutate(trial = "Pos-S")

df_data_train_prepos_s_ind <- rbind(df_data_train_pre_s_ind,df_data_train_pos_s_ind) %>% 
  mutate(trial = factor(trial, levels = c("Pre-S","Pos-S")))

df_data_train_prepos_s <- df_data_train_prepos_s_ind %>% 
  dplyr::rename(voi = ind_m) %>% 
  summarise_train_dh_prepos_cond()


## Lrn vs NLrn
df_data_train_pre_s_ind_td <- df_data_train_dh_ind_raw %>% 
  dplyr::rename(voi = pre_herr) %>% 
  summarise_train_dh_preorpos_td_ind() %>% 
  mutate(trial = "Pre-S")

df_data_train_pos_s_ind_td <- df_data_train_dh_ind_raw %>% 
  dplyr::rename(voi = pos_herr) %>% 
  summarise_train_dh_preorpos_td_ind() %>% 
  mutate(trial = "Pos-S")

df_data_train_prepos_s_ind_td <- rbind(df_data_train_pre_s_ind,df_data_train_pos_s_ind) %>% 
  mutate(trial = factor(trial, levels = c("Pre-S","Pos-S")))

df_data_train_prepos_s_td <- df_data_train_prepos_s_ind_td %>% 
  dplyr::rename(voi = ind_m) %>% 
  summarise_train_dh_prepos_td()






### Delta hand
## 4groups
df_data_train_dh_ind <- df_data_train_dh_ind_raw %>% 
  dplyr::rename(voi = dh) %>% 
  summarise_train_dh_cond_ind()

df_data_train_dh <- df_data_train_dh_ind %>% 
  dplyr::rename(voi = ind_m) %>% 
  summarise_train_dh_cond() 

## Lrn vs NLrn

df_data_train_dh_ind_td <- df_data_train_dh_ind_raw %>% 
  dplyr::rename(voi = dh) %>% 
  summarise_train_dh_td_ind()

df_data_train_dh_td <- df_data_train_dh_ind_td %>% 
  dplyr::rename(voi = ind_m) %>% 
  summarise_train_dh_td() 


#### Plot ####
### Prepos
## 4groups
plot_hand_blk <- function(boi){
  
  tmp_df <- subset(df_data_train_prepos_s, blk == boi)
  
  tmp_plot.pre <- ggplot(tmp_df, aes(x = trial, color = cond, group = cond)) +
    geom_hline(yintercept = 0, color="gray") +
    geom_line(aes(y=m), alpha = .8) +
    geom_point(aes(y=m), alpha = .8) +
    guides(linetype = F)
  
  tmp_plot <- format_gg(tmp_plot.pre, xlabel = "", ylabel = "", 
                        ptitle = sprintf("Blk %d (%s)",boi, probe_label[boi]), show.leg = F,
                        pcol = pcol_4grp, col_name = "Condition", ylimit = yrange_a)
  
}


nr <-  2  # number of rows
nc <-  3 # number of columns

plot_list <- plyr::alply(unique(df_data_train$blk), 1, plot_hand_blk)
plot_list_save = marrangeGrob(plot_list, nrow = nr, ncol = nc, layout_matrix = matrix(1:(nr*nc),nr,nc,T),
                              top = sprintf("Group Mean & SE Probe Hand in Pre/Pos-S"),
                              left = "Hand Error (Tgt - Hand) [deg] (Solid=+, Dotted=-)",
                              bottom = "Trial") # convert the list of plots

save_plots(tgt_plot = plot_list_save, fname = sprintf("%s_prepos_s",local_fname_tag), 
           mdir = sub_mdir, sdir = sub_sdir,
           pdf_only = T, readme_content = "")

## Lrn vs NLrn
plot_hand_blk_td <- function(boi){
  
  tmp_df <- subset(df_data_train_prepos_s_td, blk == boi)
  
  tmp_plot.pre <- ggplot(tmp_df, aes(x = trial, color = td, group = td)) +
    geom_hline(yintercept = 0, color="gray") +
    geom_line(aes(y=m), alpha = .8) +
    geom_point(aes(y=m), alpha = .8) +
    geom_errorbar(aes(ymin=m-se, ymax=m+se), alpha = .8, width=.3)
  
  
  tmp_plot <- format_gg(tmp_plot.pre, xlabel = "", ylabel = "", 
                        ptitle = sprintf("Blk %d (%s)",boi, probe_label[boi]), show.leg = F,
                        pcol = pcol_td, col_name = "Condition", ylimit = yrange_a)
  
}


nr <-  2  # number of rows
nc <-  3 # number of columns

plot_list <- plyr::alply(unique(df_data_train$blk), 1, plot_hand_blk_td)
plot_list_save = marrangeGrob(plot_list, nrow = nr, ncol = nc, layout_matrix = matrix(1:(nr*nc),nr,nc,T),
                              top = sprintf("Group Mean & SE Probe Hand in Pre/Pos-S"),
                              left = "Hand Error (Tgt - Hand) [deg] (Solid=+, Dotted=-)",
                              bottom = "Trial") # convert the list of plots

save_plots(tgt_plot = plot_list_save, fname = sprintf("%s_prepos_s_td",local_fname_tag), 
           mdir = sub_mdir, sdir = sub_sdir,
           pdf_only = T, readme_content = "")





### delta-hand
## 4groups
plot1.pre <- ggplot(df_data_train_dh, aes(x=blk, color = cond, group = cond)) +
  geom_hline(yintercept = 0, color = "gray") +
  geom_line(aes(y=m))+
  geom_point(aes(y=m))


plot1 <- format_gg(plot1.pre, xlabel = "Block", ylabel = "Delta Hand [deg] (Solid:+, Dotted=-)", 
                      ptitle = sprintf("Group Mean & SE Delta Hand"), show.leg = F,
                      pcol = pcol_4grp, col_name = "Condition", ylimit = yrange_a)

## Lrn vs NLrn
plot2.pre <- ggplot(df_data_train_dh_td, aes(x=blk, color = td, group = td)) +
  geom_hline(yintercept = 0, color = "gray") +
  geom_line(aes(y=m))+
  geom_point(aes(y=m))+
  geom_errorbar(aes(ymin=m-se, ymax=m+se), width = .3) 

plot2 <- format_gg(plot2.pre, xlabel = "Block", ylabel = "Delta Hand [deg] (Solid:+, Dotted=-)", 
                   ptitle = sprintf("Group Mean & SE Delta Hand"), show.leg = F,
                   pcol = pcol_td, col_name = "Condition", ylimit = yrange_a)




save_plots(tgt_plot = plot1, fname = sprintf("%s",local_fname_tag), mdir = sub_mdir, sdir = sub_sdir, pdf_only = T, readme_content = "")
save_plots(tgt_plot = plot2, fname = sprintf("%s_td",local_fname_tag), mdir = sub_mdir, sdir = sub_sdir, pdf_only = T, readme_content = "")



