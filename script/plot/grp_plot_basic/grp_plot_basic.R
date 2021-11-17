# hand direction is based on the edge of slit (8cm), so plots look slightly different from ones made in initial processing
source("function/set_exp_param.R")

# sub_id <- sub_inc_main[3]
# sub_id <- 37
slit_inner_edge <- 0.1 # distance of the inner slit edge from the start (m). hard coding
prb_pre_null_inc <- 10 # how many included are null trials that precedes Probe (rotation)


load_organized_data <- T # if you re-run this, you can save by loading data
save_df_data_ind <- F # whether you save organized data
main_dir <- "grp_plot_basic"
sub_dir <- sprintf("grp_%dcm",slit_inner_edge*100)







# tmp_fn <- list.files("data/processed/", pattern=sprintf("S%d",sub_id))
# unique_sub_tag <- tmp_fn[length(tmp_fn)] # you can set your own specific subject id/tag in string. Set NA if you don't need this feature
# sub_dir <-  sprintf("%s_%dcm",unique_sub_tag,slit_inner_edge*100)

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

# tgt_dir <- unique_sub_tag # copy

## Read and organize data
if (load_organized_data){
  fpath <- sprintf("script/plot/%s/data",main_dir)
  load(sprintf("%s/%s/df_data_ind.RData",fpath,sub_dir))
} else {
  source(sprintf("script/plot/%s/sub_script/local_function/organize_ind_data.R",main_dir)) 
  df_data_ind <- lapply(sub_inc_main, organize_ind_data) %>% 
    reduce(rbind) %>% 
    mutate(cond = factor(cond, levels=c("LR","LP","NR","NP"))) %>% 
    mutate(td = ifelse(cond %in% c("LR","LP"), "Lrn", "NLrn"), valence = ifelse(cond %in% c("LR","NR"),"Rwd","Pun"))
  if (save_df_data_ind){
    fpath <- sprintf("script/plot/%s/data",main_dir)
    dir.create(file.path(fpath, sprintf("%s",sub_dir)), showWarnings = FALSE)
    save(df_data_ind, file=sprintf("%s/%s/df_data_ind.RData",fpath,sub_dir))
  }
  
}

source(sprintf("script/plot/%s/sub_script/local_function/count_num_each_cond.R",main_dir)) 

# individual probe 
source(sprintf("script/plot/%s/sub_script/local_function/func_summarise_by_trial.R",main_dir)) 
source(sprintf("script/plot/%s/sub_script/local_function/create_df_part.R",main_dir)) 

# df_data_wo <- df_data_wo_ind %>% 
#   group_by(blk,wo_tri, cond, show_arc) %>% 
#   summarise(m = mean(herr, na.rm = T), sd = sd(herr, na.rm = T), se = sd(herr, na.rm = T)/sqrt(length(herr)), n = length(herr)) %>% 
#   ungroup()

# get group tag (sub_id and condition)
grp_tag <- df_data_ind %>% 
  group_by(sub_id) %>% 
  slice_head() %>% 
  ungroup() %>% 
  dplyr::select(sub_id, cond)


## Now plot

source(sprintf("script/plot/%s/sub_script/plot_hand_probe.R",main_dir))
source(sprintf("script/plot/%s/sub_script/plot_hand_train.R",main_dir))
source(sprintf("script/plot/%s/sub_script/plot_hand_bias.R",main_dir))
source(sprintf("script/plot/%s/sub_script/plot_mt.R",main_dir))




# 
# 
# ### Further organizing data for plotting
# df_data <- left_join(df_point,df_tgt, by=c("blk","blk_tri")) %>% 
#   left_join(df_hand_slit, by=c("blk","blk_tri"))%>% 
#   mutate(herr = tgt - hand_slit, 
#          terr = tgt - (hand_slit + rot), 
#          tgt_f = factor(tgt)) %>%
#   mutate(m_tri = factor(ifelse(show_cur == 0, "y", "n"), levels=c("y","n")), cur = factor(ifelse(show_cur == 0, "no_cur","cur")))
# 
# df_data_train <- df_data %>% 
#   dplyr::filter(block_phase == phase_train) %>% 
#   mutate(rot = ifelse(m_tri == "y", 0, rot), herr = ifelse(tsize == 0, NA, herr), 
#          terr = ifelse(tsize == 0, NA, terr),
#          total_cycle = (row_number()-1)%/% tpc + 1) %>% # some modifications for further processing/plotting
#   group_by(blk) %>% 
#   mutate(block_cycle = (row_number()-1)%/% tpc + 1, train_tri = row_number()) %>% 
#   ungroup()
# 
# df_data_probe_pre_bl <- df_data %>% 
#   dplyr::filter(lead(block_phase, prb_pre_null_inc) == phase_prb_prebl | block_phase == phase_prb_prebl) %>% 
#   mutate(prb_tri = row_number(), run = 1)
# 
# df_data_probe <- df_data %>% 
#   dplyr::filter(lead(block_phase, prb_pre_null_inc) == phase_prb | block_phase == phase_prb) %>% 
#   group_by(blk) %>% 
#   mutate(prb_tri = row_number()) %>% 
#   ungroup() %>% 
#   mutate(run = blk) %>% 
#   rbind(df_data_probe_pre_bl,.)
# 
# #### Plot
# source(sprintf("script/plot/%s/sub_script/get_grp_tag.R",main_dir))
# ind_tag <- sprintf("%s [%s]",unique_sub_tag, cond_tag)
# 
# source(sprintf("script/plot/%s/sub_script/plot_hand_block.R",main_dir)) # hand profile in each block
# source(sprintf("script/plot/%s/sub_script/plot_tgt_bias.R",main_dir)) # target location-dependent bias
# source(sprintf("script/plot/%s/sub_script/plot_hand_block_train.R",main_dir)) # hand profile in each block
# source(sprintf("script/plot/%s/sub_script/plot_hand_block_probe.R",main_dir)) # hand profile in each block
# source(sprintf("script/plot/%s/sub_script/plot_score.R",main_dir)) # hand profile in each block













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

