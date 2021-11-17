# This process an individual data
# It takes 10-15 minutes to process a single subject dataset, so keep that in mind when
# running multiple data sets
# Author: Taisei Sugiyama

# source("unload_all_lib.R") # Clear all loaded library first to avoid bugs by masking functions

source("function/get_grp_soi.R")
source("function/set_exp_param.R")
# subs = get_grp_soi(all_subs,"rd")

subs <- 127

# for blk 7
# source("function/additional_blk_cond.R")
# subs <- addblk_cond$sub_id

for (sub in subs){

sid_num = sub

message(sprintf("Running S%d",sub))

# # Do basic processing
source("script/ind_process/p1_load_rawdata.R")
source("script/ind_process/p2_clean_data.R")
source("script/ind_process/p3_organize_data.R")
# source("script/ind_process/p4_fitting_data_lsse.R")
# # source("script/ind_process/p5_fitting_data_em.R")
source("script/ind_process/p6_save_ind_data.R")
# # # #
# # # # ## Calculate bias
# source("script/ind_process/calc_bias.R")
# source("script/ind_process/calc_bias_blk.R")
# source("script/ind_process/calc_bias_lastfam.R")
# source("script/ind_process/calc_bias_null.R")
# source("script/ind_process/calc_bias_allnull.R")
# # # #
# # # # ## Additional fitting
# source("script/ind_process/prb_additional_fit.R")
# source("script/ind_process/prb_additional_fit_nb.R")
# source("script/ind_process/prb_additional_fit_zerowo.R")
# source("script/ind_process/prb_additional_fit_zerowo_lin.R")

# source("script/ind_process/prb_additional_fit_blk7.R") # only for those who did block 7
# # #
# # #
# # # ## Jags Estim (Intervention)
# source("script/ind_process/jags/jags_est_fixed_alpha.R")
# source("script/ind_process/jags/jags_est_free_alpha.R")
# source("script/ind_process/jags_wo/jags_est_wo_alpha.R")
# # 
# source("script/ind_process/jags/jags_est_fixed_alpha_x.R")
# source("script/ind_process/jags/jags_est_fixed_lowalpha_x.R")
# source("script/ind_process/jags/jags_est_fixed_highalpha_x.R") 
# source("script/ind_process/jags/jags_est_fixed_highalpha_x_init.R")
# source("script/ind_process/jags/jags_est_fixed_highalpha_x_init_wide_xpre.R")
# source("script/ind_process/jags/jags_est_fixed_highalpha_x_init_no_e_norm1.R")
# source("script/ind_process/jags/jags_est_fixed_highalpha_x_init_no_e_norm2.R")
# source("script/ind_process/jags/jags_est_fixed_highalpha_x_init_no_e_norm3.R")
# source("script/ind_process/jags/jags_est_fixed_highalpha_x_init_no_e_norm4.R")

# source("script/ind_process/jags/jags_est_fixed_highalpha_x_init_no_e_norm1_fa.R")
# source("script/ind_process/jags/jags_est_fixed_highalpha_x_init_no_e_norm1_nx.R")

# source("script/ind_process/jags/jags_est_fixed_highalpha_x_init_no_e_norm3_nx.R")
# source("script/ind_process/jags/jags_est_fixed_highalpha_x_init_no_e_nx.R")

# source("script/ind_process/jags/jags_est_fixed_highalpha_x_init_no_e_norm1_nx_bp.R") # testing a bad prior to check if we get worse DIC

# source("script/ind_process/jags/jags_est_fixed_highalpha_x_init_no_e2.R")
# source("script/ind_process/jags/jags_est_fixed_highalpha_x_init_no_e2_cont.R")
# source("script/ind_process/jags/jags_hybrid_ha_no_rl.R")

# source("script/ind_process/jags/jags_est_fixed_highalpha_x_init_no_e_cont.R")
# source("script/ind_process/jags/jags_hybrid_ha_no_e_no_rl.R")

# source("script/ind_process/jags/jags_est_fixed_highalpha_x_init_bias.R")

# source("script/ind_process/jags/jags_est_free_alpha_x_init.R")

# source("script/ind_process/jags/jags_est_fixed_lowalpha_x_init.R")
# source("script/ind_process/jags/jags_est_fixed_zeroalpha_x_init.R")

# new model that esrtimates xpre from ypre
# source("script/ind_process/jags/jags_est_fixed_lowalpha_x_init_y.R")
# source("script/ind_process/jags/jags_est_fixed_zeroalpha_x_init_y.R")
# source("script/ind_process/jags/jags_est_fixed_highalpha_x_init_y.R")
# source("script/ind_process/jags/jags_est_free_alpha_x_init_y.R")

# new model with lenient estimation of xpre
# source("script/ind_process/jags/jags_est_fixed_lowalpha_x_init_p.R")
# source("script/ind_process/jags/jags_est_fixed_zeroalpha_x_init_p.R")
# source("script/ind_process/jags/jags_est_fixed_highalpha_x_init_p.R")
# source("script/ind_process/jags/jags_est_free_alpha_x_init_p.R")

# source("script/ind_process/jags/jags_est_fixed_highalpha_x_init_p.R")
# source("script/ind_process/jags/jags_est_fixed_lowalpha_x_init_p.R")
# source("script/ind_process/jags/jags_est_fixed_zeroalpha_x_init_p.R")
# source("script/ind_process/jags/jags_est_free_alpha_x_init_p.R")

# source("script/ind_process/jags/jags_est_fixed_highalpha_x_init_u.R")
# source("script/ind_process/jags/jags_est_fixed_lowalpha_x_init_u.R")
# source("script/ind_process/jags/jags_est_fixed_zeroalpha_x_init_u.R")
# source("script/ind_process/jags/jags_est_free_alpha_x_init_u.R")


# source("script/ind_process/jags/jags_est_fix_beta_x_init.R")
# source("script/ind_process/jags/jags_hybrid.R")
# source("script/ind_process/jags/jags_hybrid_ha.R")
# source("script/ind_process/jags/jags_hybrid_ha_zeroeta.R")
# source("script/ind_process/jags/jags_hybrid2.R")
# source("script/ind_process/jags/jags_hybrid2_zeroeta.R")
# source("script/ind_process/jags/jags_hybrid3.R")



# 
# # #
# # # ## Jags Estim (Probe)
# source("script/ind_process/jags_probe/jags_est_free_alpha_probe.R")
# source("script/ind_process/jags_probe/jags_est_free_alpha_probe_seg.R")
# source("script/ind_process/jags_probe/jags_est_free_alpha_e_probe.R")
# source("script/ind_process/jags_probe/jags_est_free_alpha_e_probe_seg.R")
# source("script/ind_process/jags_probe/jags_est_free_alpha_e2_probe.R")
# source("script/ind_process/jags_probe/jags_est_free_alpha_e2_probe_seg.R")
# source("script/ind_process/jags_probe/jags_est_fix_alpha_e2_probe_seg.R")
# source("script/ind_process/jags_probe/jags_est_fix_lowalpha_e2_probe_seg.R")
# source("script/ind_process/jags_probe/jags_est_fix_highalpha_e2_probe_seg.R")
# source("script/ind_process/jags_probe/jags_est_fix_alpha_e_probe_seg.R")
# source("script/ind_process/jags_probe/jags_est_fix_lowalpha_e_probe_seg.R")
# source("script/ind_process/jags_probe/jags_est_fix_highalpha_e_probe_seg.R")
# source("script/ind_process/jags_probe/jags_est_fix_highalpha_tnk_seg.R")
# source("script/ind_process/jags_probe/jags_est_fix_highalpha_e2_probe_seg_nowo.R")

# source("script/ind_process/jags_probe/jags_est_fix_highalpha_e2_probe_seg_init_no_e.R")
# source("script/ind_process/jags_probe/jags_est_fix_highalpha_e2_probe_seg_init_no_e_norm.R")
# source("script/ind_process/jags_probe/jags_est_fix_highalpha_e2_probe_seg_init_no_e_norm_nx.R")
# source("script/ind_process/jags_probe/jags_est_fix_highalpha_e2_probe_seg_init_no_e_norm_fa.R")

# source("script/ind_process/jags_probe/jags_est_ha_e_init_probe.R")
# source("script/ind_process/jags_probe/jags_est_ha_e_init_probe_2seg.R")
# source("script/ind_process/jags_probe/jags_est_ha_e_init_probe_sand.R")
# source("script/ind_process/jags_probe/jags_est_ha_e_init_probe_sand2.R")
# source("script/ind_process/jags_probe/jags_est_ha_e_init_probe_sand_lin.R")
# source("script/ind_process/jags_probe/jags_est_ha_e_init_probe_sand_lin2.R") # same as ~lin. but narrower prior for beta
# source("script/ind_process/jags_probe/jags_est_ha_e_init_probe_sand_obs_e.R")
# source("script/ind_process/jags_probe/jags_est_ha_e_init_probe_sand_y.R")
# source("script/ind_process/jags_probe/jags_est_ha_e_init_probe_sand_p.R")
# source("script/ind_process/jags_probe/jags_est_ha_e_init_probe_sand_u.R")
# source("script/ind_process/jags_probe/jags_est_ha_e_init_probe_sand_f.R")

# source("script/ind_process/jags_probe/jags_est_ha_e_init_probe_sand_blk7.R")

# source("script/ind_process/jags_probe/jags_est_ha_e_init_probe_sand_obs_e_y.R")

}
