yrange_a <- c(0,.5)
nr <-  1  # number of rows
nc <-  1 # number of columns

sub_mdir <- sprintf("%s/%s",main_dir,sub_dir) # sub-main directory name
sub_sdir <- "plot_mt" # sub-sub directory name

source("function/create_sub_dir.R")
create_sub_dir("figure",main_dir)
create_sub_dir(sprintf("figure/%s",main_dir),sub_dir)



source(sprintf("script/plot/%s/sub_script/local_function/%s/%s_block.R",main_dir,sub_sdir,sub_sdir))


