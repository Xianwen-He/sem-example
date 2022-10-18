## a dataset generated from covars and means
colom_dat <- read.table('./Ch6/Colom_from_cov_mean.txt', sep=',')
colnames(colom_dat) <- c('z1', 'z2', 'z3', 'z5', 'z6', 'z4',
                         'z8', 'z7', 'z9', 'z11', 'z10', 'z12')

## check whether the dataset consistent with the covars and means
# cov
#cov.dat <- cov(colom_dat)
#cov.diff <- abs(cov.dat - colom.cov)  # colom.cov from the other file
#which(cov.diff > 1e-5)
# mean
#mean.dat <- colMeans(colom_dat)
#mean.diff <- abs(mean.dat - colom.mean)


## potential MIIVs
scale_var_select_lst <- list('z4'=paste0('z', c(7:12)),
                             'z7'=paste0('z', c(4:6, 10:12)),
                             'z10'=paste0('z', c(4:9)))
obs_var_select_lst <- list('z2' = list('z1', 'z3', paste0('z', c(4:12))),
                           'z3' = list('z1', 'z2', paste0('z', c(4:12))),
                           'z5' = list('z4', 'z6', paste0('z', c(1:3, 7:12))),
                           'z6' = list('z4', 'z5', paste0('z', c(1:3, 7:12))),
                           'z8' = list('z7', 'z9', paste0('z', c(1:6, 10:12))),
                           'z9' = list('z7', 'z8', paste0('z', c(1:6, 10:12))),
                           'z11'= list('z10', 'z12', paste0('z', c(1:9))),
                           'z12'= list('z10', 'z11', paste0('z', c(1:9))))

## tools
get_R2 <- function(response, explanatory, dataset){
  exp_str <- paste(explanatory, collapse='+')
  fml <- paste0(response, '~', exp_str)
  #print(fml)
  #fml <- formula(fml)
  
  lm_mod <- lm(formula=fml, data=dataset)
  lm_mod_summ <- summary(lm_mod)
  R2 <- lm_mod_summ$r.squared
  
  print(paste(fml, R2, sep=':'))
  
  return(R2)
}
best_combn_R2 <- function(response, choices, num, fixed, dataset){
  combn_mat <- combn(length(choices), num)
  best_r2 <- 0
  best_exp <- NULL
  
  for (i in 1:ncol(combn_mat)){
    curr_combn <- combn_mat[, i]
    curr_exp <- c(fixed, choices[curr_combn])
    curr_r2 <- get_R2(response, curr_exp, dataset)
    
    if (curr_r2 > best_r2){
      best_r2 <- curr_r2
      best_exp <- curr_exp
    }
  }
  
  return(list('explanatory'=best_exp, 'R2'=best_r2))
}

## scaling variables
scale_res_lst <- lapply(names(scale_var_select_lst), function(var_name){
  print(paste0('=========>',var_name, '<========'))
  
  choices <- scale_var_select_lst[[var_name]]
  best_combn_R2('z1', choices, 2, NULL, colom_dat)
})
names(scale_res_lst) <- names(scale_var_select_lst)
## other variables
obs_res_lst <- lapply(names(obs_var_select_lst), function(var_name){
  print(paste0('=========>',var_name, '<========'))
  
  response <- (obs_var_select_lst[[var_name]])[[1]]
  fixed <- (obs_var_select_lst[[var_name]])[[2]]
  choices <- (obs_var_select_lst[[var_name]])[[3]]
  best_combn_R2(response, choices, 1, fixed, colom_dat)
})
names(obs_res_lst) <- names(obs_var_select_lst)

## results
colom.miiv <- '
z4 ~ z9 + z10
z7 ~ z6 + z10
z10 ~ z6 + z9
z2 ~ z3 + z6
z3 ~ z2 + z6
z5 ~ z6 + z1
z6 ~ z5 + z9
z8 ~ z9 + z12
z9 ~ z8 + z12
z11 ~ z12 + z3
z12 ~ z11 + z9
'
