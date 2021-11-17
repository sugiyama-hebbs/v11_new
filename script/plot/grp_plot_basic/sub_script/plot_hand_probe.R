yrange_a <- c(-20,20)
nr <-  1  # number of rows
nc <-  1 # number of columns

sub_mdir <- sprintf("%s/%s",main_dir,sub_dir) # sub-main directory name
sub_sdir <- "plot_hand_probe" # sub-sub directory name

source("function/create_sub_dir.R")
create_sub_dir("figure",main_dir)
create_sub_dir(sprintf("figure/%s",main_dir),sub_dir)



source(sprintf("script/plot/%s/sub_script/local_function/%s/%s_block.R",main_dir,sub_sdir,sub_sdir)) 
source(sprintf("script/plot/%s/sub_script/local_function/%s/%s_prepos_seg1.R",main_dir,sub_sdir,sub_sdir)) 
source(sprintf("script/plot/%s/sub_script/local_function/%s/%s_prepos_seg1_delta_from_avg.R",main_dir,sub_sdir,sub_sdir)) 
