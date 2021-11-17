# hand direction is based on the edge of slit (8cm), so plots look slightly different from ones made in initial processing
source("function/set_exp_param.R")

# sub_id <- sub_inc_main[3]
# sub_id <- 37

slit_inner_edge <- 0.05 # distance of the inner slit edge from the start (m). hard coding 

prb_pre_null_inc <- 10 # how many included are null trials that precedes Probe (rotation)

main_dir = "ind_plot"

tmp_fn <- list.files("data/processed/", pattern=sprintf("S%d",sub_id))
unique_sub_tag <- tmp_fn[length(tmp_fn)] # you can set your own specific subject id/tag in string. Set NA if you don't need this feature
sub_dir <-  sprintf("%s_%dcm",unique_sub_tag,slit_inner_edge*100)

## Preparation
library(dplyr)
library(ggplot2)
library(purrr)
library(nloptr)
library(Biobase) # Be careful about using this package, as it will mask many functions
library(gridExtra)
source("function/format_gg.R")
source("function/gg_def_col.R")
source("function/save_plots.R")

options(dplyr.summarise.inform = FALSE)

tgt_dir <- unique_sub_tag # copy

## Read and organize data

load(sprintf("data/processed/%s/exp_data.Rdata",tgt_dir)) # File path

df_point_load <- output_list$point
df_tgt_load <- output_list$tgt %>% 
  dplyr::rename(show_cur = showcur, block_phase = phase)
df_para_load <- output_list$param



# get hand direction at the inner slit
df_hand_slit <- output_list$kin %>% 
  dplyr::select(blk,blk_tri,sx,sy) %>% 
  mutate(dist = sqrt(sx^2 + sy^2)) %>% 
  dplyr::filter(dist >= slit_inner_edge) %>% 
  group_by(blk,blk_tri) %>% 
  dplyr::filter(row_number() ==1) %>% 
  ungroup() %>% 
  mutate(hand_slit = atan2(sy,sx)*180/pi) %>% 
  dplyr::select(blk,blk_tri,hand_slit)

df_point <- dplyr::select(df_point_load, blk, blk_tri, rc_xpass, rc_ypass, vc_xpass, vc_ypass, gain) %>%
  left_join(dplyr::select(df_para_load, blk, cx, cy, xoffset,yoffset), by="blk") %>% 
  mutate(align_rc_xpass = (rc_xpass-(cx-xoffset)/1000), align_rc_ypass = (rc_ypass-(cy-yoffset)/1000)) %>% 
  mutate(hand = atan2(align_rc_ypass,align_rc_xpass)*180/pi) %>% 
  mutate(align_vc_xpass = (vc_xpass-cx), align_vc_ypass = (vc_ypass-cy)) %>% 
  mutate(hand_v = atan2(align_vc_ypass,align_vc_xpass)*180/pi) %>% 
  dplyr::select(blk,blk_tri,hand,gain,hand_v)

df_tgt <- dplyr::select(df_tgt_load, blk, blk_tri,tgt,show_cur,tsize,rot,block_phase, train_type, difficulty)



### Further organizing data for plotting
df_data <- left_join(df_point,df_tgt, by=c("blk","blk_tri")) %>% 
  left_join(df_hand_slit, by=c("blk","blk_tri"))%>% 
  mutate(herr = tgt - hand_slit, 
         terr = tgt - (hand_slit + rot), 
         tgt_f = factor(tgt)) %>%
  mutate(m_tri = factor(ifelse(show_cur == 0, "y", "n"), levels=c("y","n")), cur = factor(ifelse(show_cur == 0, "no_cur","cur")))

df_data_train <- df_data %>% 
  dplyr::filter(block_phase == phase_train) %>% 
  mutate(rot = ifelse(m_tri == "y", 0, rot), herr = ifelse(tsize == 0, NA, herr), 
         terr = ifelse(tsize == 0, NA, terr),
         total_cycle = (row_number()-1)%/% tpc + 1) %>% # some modifications for further processing/plotting
  group_by(blk) %>% 
  mutate(block_cycle = (row_number()-1)%/% tpc + 1, train_tri = row_number()) %>% 
  ungroup()

df_data_probe_pre_bl <- df_data %>% 
  dplyr::filter(lead(block_phase, prb_pre_null_inc) == phase_prb_prebl | block_phase == phase_prb_prebl) %>% 
  mutate(prb_tri = row_number(), run = 1)

df_data_probe <- df_data %>% 
  dplyr::filter(lead(block_phase, prb_pre_null_inc) == phase_prb | block_phase == phase_prb) %>% 
  group_by(blk) %>% 
  mutate(prb_tri = row_number()) %>% 
  ungroup() %>% 
  mutate(run = blk) %>% 
  rbind(df_data_probe_pre_bl,.)

#### Plot
source(sprintf("script/plot/%s/sub_script/get_grp_tag.R",main_dir))
ind_tag <- sprintf("%s [%s]",unique_sub_tag, cond_tag)

source(sprintf("script/plot/%s/sub_script/plot_hand_block.R",main_dir)) # hand profile in each block
source(sprintf("script/plot/%s/sub_script/plot_tgt_bias.R",main_dir)) # target location-dependent bias
source(sprintf("script/plot/%s/sub_script/plot_hand_block_train.R",main_dir)) # hand profile in each block
source(sprintf("script/plot/%s/sub_script/plot_hand_block_probe.R",main_dir)) # hand profile in each block
source(sprintf("script/plot/%s/sub_script/plot_score.R",main_dir)) # hand profile in each block













# 
# source(sprintf("script/plot/%s/sub_script/plot_hand_block_train.R",main_dir)) # set evaluation function
# 
# # score
# source(sprintf("script/plot/%s/sub_script/plot_score.R",main_dir))
# 
# ### Fit
# 
# ## Probe ##
# df_data_probe <- subset(df_data, block_phase == 6) %>%
#   dplyr::filter(row_number() > preprb_init_tri_remove)
# 
# 
# num_tri_probe <- length(df_data_probe$hand)/num_run
# 
# df_fit_probe_pre <- df_data_probe %>%
#   mutate(h = -herr) %>%
#   mutate(run = ((row_number()-1)%/%(num_tri_probe)+1))
# 
# df_fit_probe <- df_fit_probe_pre %>%
#   group_by(run) %>%
#   dplyr::filter(row_number() > init_tri_remove, row_number() <= (num_tri_probe -last_tri_remove)) %>% # remove initial null
#   ungroup() %>%
#   dplyr::select(rot,h, run, show_cur)
# 
# df_fit_probe_pos_clamp <- df_fit_probe_pre %>%
#   group_by(run) %>%
#   dplyr::filter(row_number() >= num_init_tri, show_cur == 3) %>%
#   ungroup() %>%
#   dplyr::select(rot,h, run, show_cur)
# 
# df_fit_probe_rot_clamp <- df_fit_probe_pre %>%
#   group_by(run) %>%
#   dplyr::filter(row_number() > init_tri_remove) %>% # remove initial null
#   ungroup() %>%
#   dplyr::select(rot,h, run, show_cur)
# 
# df_input_raw <- df_fit_probe
# output_data <- df_fit_probe
# 
# dir.create(file.path(sprintf("script/plot/%s",main_dir), "data"), showWarnings = FALSE)
# save(output_data, file=sprintf("script/plot/%s/data/%s.R",main_dir,unique_sub_tag))
# 
# # Make lists
# df_input_list <- df_fit_probe %>%
#   with(split(.,list(run))) %>% # Convert to list
#   purrr::discard(function(x) nrow(x) ==0)# Remove list elements with zero rows
# 
# df_input_pos_clamp_list <- df_fit_probe_pos_clamp %>%
#   with(split(.,list(run))) %>% # Convert to list
#   purrr::discard(function(x) nrow(x) ==0)# Remove list elements with zero rows
# 
# df_input_raw_pos_clamp <- df_fit_probe_pos_clamp
# 
# df_input_rot_clamp_list <- df_fit_probe_rot_clamp %>%
#   with(split(.,list(run))) %>% # Convert to list
#   purrr::discard(function(x) nrow(x) ==0)# Remove list elements with zero rows
# 
# df_input_raw_rot_clamp <- df_fit_probe_rot_clamp
# 
# 
# ## Plotting
# ylimit_probe_hand <- c(-5,15)
# 
# source(sprintf("script/plot/%s/sub_script/plot_hand_probe.R",main_dir))
# source(sprintf("script/plot/%s/sub_script/plot_hand_probe_pos_clamp.R",main_dir))
# source(sprintf("script/plot/%s/sub_script/plot_hand_probe_avg.R",main_dir))
# 
# ## Now fit and plot
# source(sprintf("script/plot/%s/sub_script/run_ssm_spe.R",main_dir))
# source(sprintf("script/plot/%s/sub_script/run_ssm_spe_one_alpha.R",main_dir)) # shared alpha across runs
# 
# 
# 
# source(sprintf("script/plot/%s/sub_script/run_ssm_spe_clamp.R",main_dir)) # clamp portion to measure alpha
# source(sprintf("script/plot/%s/sub_script/run_ssm_spe_alpha_from_clamp.R",main_dir)) # clamp portion to measure alpha
# 
# source(sprintf("script/plot/%s/sub_script/run_ssm_spe_rot_clamp.R",main_dir)) # clamp portion to measure alpha. estimation rotation portion to estimate x0 for clamp
# source(sprintf("script/plot/%s/sub_script/run_ssm_spe_rot_clamp_sep_alpha.R",main_dir)) # assign a separate alpha for pre-washout
# #
# output_data_estpara <- rbind(estpara_independent_alpha,estpara_one_alpha,estpara_alpha_from_clamp, estpara_rot_clamp, estpara_rot_clamp_sep_alpha) %>%
#   dplyr::select(-beta_wo)
# 
# save(output_data_estpara, file=sprintf("script/plot/%s/data/%s_estpara.R",main_dir,unique_sub_tag))
# 
# # plot estimated parameters
# source(sprintf("script/plot/%s/sub_script/plot_estpara.R",main_dir)) # clamp portion to measure alpha
# #
# # ## Train ##
# df_data_train <- subset(df_data, block_phase == 4)
# 
# df_fit_train <- df_data_train %>%
#   mutate(h = -herr) %>%
#   mutate(run = 1) %>%
#   dplyr::select(rot,h, run, show_cur)
# #
# # ## Now fit and plot
# source(sprintf("script/plot/%s/sub_script/plot_decay_9.175.R",main_dir))
# source(sprintf("script/plot/%s/sub_script/plot_dh_9.175.R",main_dir))
# save.image(sprintf("script/plot/%s/data/%s_image.RData",main_dir,unique_sub_tag))

