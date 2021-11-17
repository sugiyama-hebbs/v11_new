tgt_fname_train1 <- df_para_load$tgt_fname[train_blk[1]]

fname_cond_tag <- substr(tgt_fname_train1,11,13)


if(fname_cond_tag == "GTW"){
  cond_tag <- "LR"
} else if(fname_cond_tag == "GTA") {
  cond_tag <- "LP"
} else if(fname_cond_tag == "NTW") {
  cond_tag <- "NR"
} else if(fname_cond_tag == "NTA") {
  cond_tag <- "NP"
} else {
  cond_tag <- NA
} 
