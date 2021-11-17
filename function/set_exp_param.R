# Set basic experiment parameters
# Make sure that all the values are set properly when you copy and paste the anaysis scripts from one paradigm to another
#
# Author: Taisei Sugiyama

# all_subs <- 37:65
# 
# subs_1s1m <- c(37:40, 45:48, 52:55, 61:65)
# subs_5s5m <- c(41:44, 49:51, 56:60)

sub_inc_main <- c(22:40, 42, 44:56, 58:59, 63:64, 66:68)

pre_blk <- 2 # baseline block number
pos_blk <- 6 # last train block number

main_blk <- pre_blk:pos_blk
train_blk <- (pre_blk+1):pos_blk


## block phase number
phase_train <- 4
phase_pre_prb <- 5
phase_prb <- 6


phase_pre_prb_prebl <- 1 # this includes all initial null in BL, so process with care
phase_prb_prebl <- 2

## train cycle 
tpc <- 5 # number of trials per cycle
cpb <- 28 # number of cycles per block

## labeling
probe_label <- c("Pre-BL","NoRwd (BL)","Train1","Train2","Train3","Train4","Strategy")


## coloring
source("function/gg_def_col.R")

pcol_4grp <- c("#ce3660","#e7211f","#9ecd6d","#2a4097")
pcol_td <- gg_def_col(2)


