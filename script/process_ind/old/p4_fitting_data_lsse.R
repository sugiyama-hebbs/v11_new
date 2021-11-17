## This script conducts LSSE fitting.
## This does fitting on both whole and part (1st half, 2nd half, and initial segment) of Probe data set
## Make sure to run the p3_clean_data before you run this (or have the environment ready by loading it) 
## Author: Taisei Sugiyama

## Define key parameter values
## Make sure all the parameters are properly set in the source script
source("script/ind_process/lsse_fit_param.R")

## Load packages
library(dplyr)
library(purrr)
library(nloptr)
library(Biobase) # Be careful about using this package, as it will mask many functions

## Load functions
source("function/calc.sse.ssm.R")
source("function/organize_data_for_fit.R")

## Organize data
boi = init_boi:last_boi
data_prb = organize_data_for_fit(sub,boi,set_type = "all", read_data = F)

## Fitting
# Whole
fit_res_whole = data_prb %>%
  dplyr::filter(half == "first" | half == "second") %>% # Filter
  with(split(.,list(prepos,blk))) %>% # Convert to list
  purrr::discard(function(x) nrow(x) ==0)  %>% # Remove list elements with zero rows
  map( ~ nloptr(x0 = init_param, eval_f = calc.sse.ssm, lb = lb, ub = ub,
               opts=list("algorithm" = "NLOPT_LN_BOBYQA","xtol_rel" = 1.0e-3), data =.)) # Fitting

estpara_whole_raw = subListExtract(L=fit_res_whole, name = "solution") %>% # Get estimated parameters
  reduce(rbind) %>% # Combine the list
  as.data.frame() # Convert to data frame

# Rename row and column
rownames(estpara_whole_raw) = names(fit_res_whole) 
colnames(estpara_whole_raw) = c("alpha","beta","x0")

# Add tags
estpara_whole = estpara_whole_raw %>%
  mutate(split = row.names(.)) %>% # Preserve the splitting condition, as mutate will change the row name to numbers
  mutate(prepos = ifelse(grepl("pre", split),"pre","pos")) %>% # Get Pre vs Pos
  mutate(blk = as.numeric(gsub(".*([0-9]+).*$", "\\1", split))) %>% # Get block number
  mutate(part = "whole")

# Half
fit_res_half = data_prb %>%
  dplyr::filter(half == "first" | half == "second") %>% # Filter 
  with(split(.,list(prepos,blk,half))) %>% # Convert to list
  purrr::discard(function(x) nrow(x) ==0)  %>% # Remove list elements with zero rows
  map(
    ~ nloptr(x0 = init_param, eval_f = calc.sse.ssm, lb = lb, ub = ub,
             opts=list("algorithm" = "NLOPT_LN_BOBYQA","xtol_rel" = 1.0e-5),data = .)) # Fitting

estpara_half_raw = subListExtract(L=fit_res_half, name = "solution") %>% # Get estimated parameters
  reduce(rbind) %>% # Combine the list
  as.data.frame() # Convert to data frame  

# Rename row and column
rownames(estpara_half_raw) = names(fit_res_half)
colnames(estpara_half_raw) = c("alpha","beta","x0")

estpara_half = estpara_half_raw %>%
  mutate(split = row.names(.)) %>% # Preserve the splitting condition, as mutate will change the row name to numbers
  mutate(prepos = ifelse(grepl("pre", split),"pre","pos")) %>% # Get Pre vs Pos
  mutate(blk = as.numeric(gsub(".*([0-9]+).*$", "\\1", split))) %>% # Get block number
  mutate(part = ifelse(grepl("first", split),"first","second")) # Get part data

# Initial 
fit_res_init = data_prb %>%
  dplyr::filter(initial == 1) %>% # Filter
  with(split(.,list(prepos,blk))) %>% # Convert to list
  purrr::discard(function(x) nrow(x) ==0)  %>% # Remove list elements with zero rows
  map(
    ~ nloptr(x0 = init_param, eval_f = calc.sse.ssm, lb = lb, ub = ub,
             opts=list("algorithm" = "NLOPT_LN_BOBYQA","xtol_rel" = 1.0e-5),data = .)) # Fitting

estpara_init_raw = subListExtract(L=fit_res_init, name = "solution") %>% # Get estimated parameters
  reduce(rbind) %>% # Combine the list 
  as.data.frame() # Convert to data frame

# Rename
rownames(estpara_init_raw) = names(fit_res_init)
colnames(estpara_init_raw) = c("alpha","beta","x0")

# Add tags
estpara_init = estpara_init_raw %>%
  mutate(split = row.names(.)) %>% # Preserve the splitting condition, as mutate will change the row name to numbers
  mutate(prepos = ifelse(grepl("pre", split),"pre","pos")) %>% # Get Pre vs Pos
  mutate(blk = as.numeric(gsub(".*([0-9]+).*$", "\\1", split))) %>% # Get block number
  mutate(part = "init")


## All
fit_res_all = data_prb %>%
  with(split(.,list(prepos,blk))) %>% # Convert to list
  purrr::discard(function(x) nrow(x) ==0)  %>% # Remove list elements with zero rows
  map( ~ nloptr(x0 = init_param, eval_f = calc.sse.ssm, lb = lb, ub = ub,
                opts=list("algorithm" = "NLOPT_LN_BOBYQA","xtol_rel" = 1.0e-3), data =.)) # Fitting

estpara_all_raw = subListExtract(L=fit_res_all, name = "solution") %>% # Get estimated parameters
  reduce(rbind) %>% # Combine the list
  as.data.frame() # Convert to data frame

# Rename row and column
rownames(estpara_all_raw) = names(fit_res_all) 
colnames(estpara_all_raw) = c("alpha","beta","x0")

# Add tags
estpara_all = estpara_all_raw %>%
  mutate(split = row.names(.)) %>% # Preserve the splitting condition, as mutate will change the row name to numbers
  mutate(prepos = ifelse(grepl("pre", split),"pre","pos")) %>% # Get Pre vs Pos
  mutate(blk = as.numeric(gsub(".*([0-9]+).*$", "\\1", split))) %>% # Get block number
  mutate(part = "all")


# Combine all together
estpara_lsse = do.call(rbind, list(estpara_whole,estpara_half,estpara_init, estpara_all)) %>%
  select(-"split")


