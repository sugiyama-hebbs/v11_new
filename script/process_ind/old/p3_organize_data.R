## This script organizes data.
## Make sure to run the p2_clean_data before you run this (or have the environment ready by loading it) 
## Author: Taisei Sugiyama

## Load packages
library(dplyr)
library(iemisc)

## Organize hand and error data 
# Get kinematic data for end-point (i.e., Just when the hand is crossing)
data_kin_endpt = data_kin_align %>%
  select(total_tri,blk_tri,blk,state,x_mm_adj,y_mm_adj,dist) %>%
  group_by(total_tri) %>%
  dplyr::filter(dist >= data_para[[1,"trad"]]*1000) %>% # Find endpoint
  slice(1) %>% # Get the first data point
  ungroup() %>%
  mutate(hand = atan2d(y = .$y_mm_adj, x = .$x_mm_adj)) # calculate the angle from xy

# tmp <- subset(data_kin_align, total_tri == 106) %>%
#   select(total_tri,blk_tri,blk, time_pt, state,x_mm_adj,y_mm_adj,dist, moving, max_speed, speed_sgolay)
# 
# tmp2 <- subset(data_kin_filt, total_tri == 106) %>%
#   select(total_tri,blk_tri,blk, time_pt, state,x_mm_adj,y_mm_adj,dx_mm, dy_mm, dist)
# 
# 
# tmp3 <- left_join(data_kin_endpt, data_tgt, by = "total_tri")


# Organize trial data frame
data_tri_clean = data_tri %>%
  mutate(tgt = data_tgt$tgt[total_tri]) %>%
  left_join(data_kin_endpt, by = c("total_tri", "blk_tri", "blk")) %>%
  mutate(cur = hand + data_tgt$shift[total_tri]/1000) %>%
  mutate(error_hand = tgt - hand) %>%
  mutate(error_cur = tgt - cur) %>%
  select(total_tri,blk_tri,blk,tgt,hand,cur,error_hand,error_cur,gain,mt,rt,rc_xpass,rc_ypass)

## Estimate values
# To be edited.




