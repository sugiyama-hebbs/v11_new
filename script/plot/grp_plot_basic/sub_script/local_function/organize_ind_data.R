organize_ind_data <- function(sub){
  
  tmp_fn <- list.files("data/processed/", pattern=sprintf("S%d",sub))
  unique_sub_tag <- tmp_fn[length(tmp_fn)] # you can set your own specific subject id/tag in string. Set NA if you don't need this feature
  
  load(sprintf("data/processed/%s/exp_data.Rdata",unique_sub_tag)) # File path
  
  df_point_load <- output_list$point
  df_para_load <- output_list$param
  df_tgt_load <- output_list$tgt %>% 
    dplyr::rename(show_cur = showcur, block_phase = phase)
  
  # get hand direction at the inner slit
  df_hand_slit <- output_list$kin %>% 
    dplyr::select(blk,blk_tri,sx,sy, tstep) %>% 
    mutate(dist = sqrt(sx^2 + sy^2)) %>% 
    dplyr::filter(dist >= slit_inner_edge) %>% 
    group_by(blk, blk_tri) %>% 
    dplyr::filter(row_number() ==1) %>% 
    ungroup() %>% 
    mutate(hand_slit = atan2(sy,sx)*180/pi) %>% 
    dplyr::select(blk,blk_tri,hand_slit, tstep)
  
  
  df_point <- dplyr::select(df_point_load, blk, blk_tri, rc_xpass, rc_ypass, vc_xpass, vc_ypass, gain, mt,rt) %>%
    left_join(dplyr::select(df_para_load, blk, cx, cy, xoffset,yoffset), by="blk") %>% 
    mutate(align_rc_xpass = (rc_xpass-(cx-xoffset)/1000), align_rc_ypass = (rc_ypass-(cy-yoffset)/1000)) %>% 
    mutate(hand = atan2(align_rc_ypass,align_rc_xpass)*180/pi) %>% 
    mutate(align_vc_xpass = (vc_xpass-cx), align_vc_ypass = (vc_ypass-cy)) %>% 
    mutate(hand_v = atan2(align_vc_ypass,align_vc_xpass)*180/pi) %>% 
    dplyr::select(blk,blk_tri,hand,gain,hand_v, mt, rt) %>% 
    dplyr::rename(score = gain)
  
  
  df_tgt <- dplyr::select(df_tgt_load, blk, blk_tri,tgt,show_cur,tsize,rot,block_phase, train_type, difficulty)
  
  
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
  
  df_data <- left_join(df_point,df_tgt, by=c("blk","blk_tri")) %>% 
    left_join(df_hand_slit, by=c("blk","blk_tri"))%>% 
    mutate(herr = tgt - hand_slit, 
           herr_cross = tgt - hand,
           terr = tgt - (hand_slit + rot), 
           tgt_f = factor(tgt)) %>%
    mutate(sub_id = sub, cond = cond_tag, m_tri = factor(ifelse(show_cur == 0, "y", "n"), levels=c("y","n")), cur = factor(ifelse(show_cur == 0, "no_cur","cur"))) %>% 
    dplyr::select(sub_id, cond, blk,blk_tri,herr,herr_cross, tgt, show_cur,tsize,rot,block_phase,train_type,difficulty,m_tri,score, tstep, mt,rt)
  
}