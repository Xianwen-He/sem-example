library("blavaan")

# readin the data
dat.bollen <- MIIVsem::bollen1989a
# rename the variables.
colnames(dat.bollen) <- c("Z4", "Z5", "Z6", 
                          "Z7", "Z8", "Z9", 
                          "Z10", "Z11", 
                          "Z1", "Z2", "Z3" )

# specify the model
mod.indem <- '
  L1 =~ Z1 + Z2 + Z3 
  L2 =~ Z4 + Z5 + Z6  + Z7  
  L3 =~ Z8 + Z9 + Z10 + Z11    

  L2  ~ L1  
  L3  ~ L1 + L2
 
  L1 ~ 1
  Z1 ~ 0
  L2 ~ 1
  Z4 ~ 0
  L3 ~ 1
  Z8 ~ 0
  
  Z4 ~~ Z8
  Z5 ~~ Z7 + Z9
  Z6 ~~ Z10
  Z7 ~~ Z11
  Z9 ~~ Z11 '

# two chains with 50000 iterations for each
set.seed(1234)
Bfit <- bsem(mod.indem, data=dat.bollen, bcontrol = list(cores = 2L),
             n.chains = 2, burnin = 50000, sample = 5000)
save(Bfit, file = 'Bfit.RData')  # save the checkpoint

# null model
null.model <- c(paste0("Z", 1:11, " ~~ Z", 1:11), paste0("Z", 1:11, " ~ 1"))
set.seed(4321)
fit0 <- bsem(null.model, data = dat.bollen, bcontrol = list(cores = 2L),
             n.chains = 2, burnin = 50000, sample = 5000)
save(fit0, file = 'fit0.RData')  # save the checkpoint

# summarize
summary(Bfit)
summary(fit0)
