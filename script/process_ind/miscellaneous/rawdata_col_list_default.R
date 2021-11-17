# set columns of raw data files. 
# You can specify your own columns according to your raw data files, but script may fail if "key" variables are missing or renamed (e.g., x)
#
# Author: Taisei Sugiyama


dat_col <- c("x","y","vx","vy","fs_x","fs_y","state","mt","rt","vc_xpass","vc_ypass","rc_xpass","rc_ypass","gain")
tgt_col <-  c("tgt","wait_time","bval","field","rot","max_score","difficulty","showcur","show_score","tsize","phase","train_type","min_score")
# tgt_col <-  c("tgt","iti","bval","field","shift","maxpay","basepen","showcur","mf","tsize","phase","task_demand","minpay")
