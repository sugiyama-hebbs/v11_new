## This script reads and organizes raw data text file (sequence file, param file, and raw data file) for RMA Project 
## Author: Taisei Sugiyama


rawdata_dir <- "data/rawdata" # the directory where raw data folders (directories) are stored
save_main_dir <- "data"
save_sub_dir <- "processed"


# unique_sub_tag <- "S22" # you can set your own specific subject id/tag in string. Set NA if you don't need this feature

plot_kinematics <- F # whether you want to output plots for basic kinematics. 
bois_plot_kin <- c(2,6) # blocks in which 


plot_points <- F # boolean. whether you want to output plots for basic trial-by-trial point data (e.g., mt, rt, error). 
output_rdata <- T # boolean. whether you want output formatted as rdata
output_csv <- F # boolean.  whether you want output formatted as csv

plot_bias <- F

align_window <- 500 # In how long (ms) do you want kinematic data after it's aligned to movement initiation. This automatically considers downsampling, so set as time, not # of data points. 
align_time_back <- 100 # How long (ms) do you want to include kinematic data before movement initiation. Note that this "pre-movement" period is included in align_window. 


#### Initial preparation #### 
## Load packages
library(dplyr)
library(purrr)
library(signal)
library(iemisc)
library(ggplot2)
library(ggforce)
library(gridExtra)

options(dplyr.summarise.inform = FALSE) # suppress annoying summary message from dplyr

source("script/process_ind/miscellaneous/param_sg_filt.R")
source("script/process_ind/miscellaneous/rawdata_col_list_default.R")
source("script/process_ind/miscellaneous/state_list.R")
source("script/process_ind/sub_script/process_kin.R")

## miscellaneous processing
if (!is.na(unique_sub_tag)){
  tgt_dir <- unique_sub_tag
} else {
  if (add_zero & sub_id < 10){
    tgt_dir <- sprintf("%s0%d",sub_identifier,sub_id)
  } else {
    tgt_dir <- sprintf("%s%d",sub_identifier,sub_id)
  }
}

fname_all <- list.files(path = sprintf("%s/%s",rawdata_dir,tgt_dir), full.names = T) 

## Read parameter file
para_fname <- grep(glob2rx("*B*Para.dat"),fname_all) %>%
  fname_all[.] 

param_raw <- lapply(para_fname, function(one_fname){ 
  
  blk <- as.numeric(str_extract_all(one_fname, "[0-9]+")[[1]])[2] # 2nd value is block number 
  
  tmp_data <- read.table(one_fname, header = T, sep=",") %>% 
    mutate(blk)
  
}) %>% 
  reduce(rbind) %>%
  dplyr::rename(trad = Trad, max_mt = MaxMT, min_mt = MinMT, sxpos = Sxpos, sypos = Sypos,
                tgt_fname = Tgt.Filename, rwd_zone = RwdZone, learn_to_max_outcome = L2Maxpay)


## Target files
fpath_tgt <- sprintf("data/target/") # File path for tgt files

tgt_raw <- apply(param_raw,1,function(df){
  
  tgt_blk <- sprintf("data/target/%s",df["tgt_fname"]) %>% 
    read.table(., header=F, sep="", col.names = tgt_col) %>% 
    mutate(blk = as.numeric(df["blk"]), blk_tri = row_number()) %>% 
    mutate(rot = rot/1000, wait_time = wait_time/1000) # convert unit (remove milli)
  
  # colnames(tgt_blk) <- c(tgt_col,"blk","blk_tri")
  
  return(tgt_blk)
  
}) %>% 
  reduce(rbind) %>% 
  mutate(total_tri = row_number()) 


## Read trial data files 
dat_fname <- grep(glob2rx("*B*T*.dat"),fname_all) %>%
  fname_all[.] 

tmp_list <- lapply(dat_fname, function(one_fname){
  
  this_blk <- as.numeric(str_extract_all(one_fname, "[0-9]+")[[1]])[2] # 2nd value is block number 
  blk_tri <- as.numeric(str_extract_all(one_fname, "[0-9]+")[[1]])[3] + 1 # 3rd value is trial number (starting from 0, so add 1)
  
  tmp_data <- read.table(one_fname, header = F, sep=",", col.names = dat_col) %>% 
    dplyr::filter(state != 0) %>% 
    mutate(this_blk, blk_tri)

  blk_param <- subset(param_raw,blk == this_blk)
  task_center_x <- (blk_param$cx-blk_param$xoffset)/1000
  task_center_y <- (blk_param$cy-blk_param$yoffset)/1000
    

  # separate kinematic data and point data. Also filter and downsample kinematic data
  kin_filt <- tmp_data[,c("x","y","vx","vy","fs_x","fs_y","state")] %>% 
    mutate(x = (x - task_center_x), y = (y - task_center_y)) %>%  # zero-ing with respect to the center of task space. 
    process_kin(.,reduce_hz,reduce_hz_rate) %>%
    mutate(blk = this_blk, blk_tri = blk_tri)
  
  tmp_data_point <- tmp_data[,c(15,16,8:14)] %>% 
    dplyr::filter(row_number() ==1)
  
  # name columns
  colnames(tmp_data_point) <- c("blk","blk_tri",dat_col[8:14])
  
  return_list <- list(df_kin = kin_filt, df_point = tmp_data_point)
  
})

kin_raw <- sapply(tmp_list,"[","df_kin") %>% 
  reduce(rbind) %>% 
  arrange(blk,blk_tri) %>% 
  left_join(dplyr::select(tgt_raw, blk, blk_tri, total_tri), by=c("blk","blk_tri"))

point_raw <- sapply(tmp_list,"[","df_point") %>% 
  reduce(rbind) %>% 
  arrange(blk,blk_tri) %>% 
  mutate(total_tri = row_number())


kin_align <- dplyr::filter(kin_raw, lead(state, (align_time_back/reduce_hz_rate))>= state_moving) %>% 
  group_by(blk,blk_tri) %>% 
  dplyr::filter(row_number() <= (align_window/reduce_hz_rate)) %>% 
  mutate(tstep_align = tstep - tstep[1]+1) %>% # add time step with respect to movement initiation
  ungroup()


#### Plotting ####
if (plot_kinematics){
  source("script/process_ind/sub_script/plot_pos.R")
  source("script/process_ind/sub_script/plot_vel.R")
}



#### Saving ####
save_dir <-  sprintf("%s/%s",save_main_dir,save_sub_dir)

# Create folders if not exist
dir.create(file.path(save_main_dir, save_sub_dir), showWarnings = FALSE)
dir.create(file.path(save_dir, sprintf("%s",tgt_dir)), showWarnings = FALSE)

fpath = sprintf("%s/%s",save_dir,tgt_dir)

# put everything in list and save as Rdata

if (output_rdata){
  output_list <- list(point = point_raw, tgt = tgt_raw, kin = kin_raw, param = param_raw)
  save(output_list, file=sprintf("%s/exp_data.RData",fpath))
}

# save as csv
if (output_csv){
  write.csv(point_raw, file=sprintf("%s/point_data.csv",fpath), row.names = FALSE)
  write.csv(tgt_raw, file=sprintf("%s/tgt_data.csv",fpath), row.names = FALSE)
  write.csv(kin_raw, file=sprintf("%s/kin_data.csv",fpath), row.names = FALSE)
  write.csv(kin_align, file=sprintf("%s/kin_data_align.csv",fpath), row.names = FALSE)
  write.csv(param_raw, file=sprintf("%s/param_data.csv",fpath), row.names = FALSE)
}




