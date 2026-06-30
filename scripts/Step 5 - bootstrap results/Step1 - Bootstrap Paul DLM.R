#### Script for Bootstrapping the Paul DLM results

# Bootstrap DLM
# SRC 2020-11-23

rm(list = ls())
graphics.off()

library('moments')
library(parallel)

options(mc.cores = parallel::detectCores())

source('./scripts/Step 5 - bootstrap results/Step00 - ODLMAR_for_Bootstrap_2020-11-23.R')

# SOURCE:  BigDataFiles+Analyses/Splines_for_Langevin_April2023/Step1_DLM_Mendota_Pool_2019-2021__2023-06-15.R
# DATA WERE TRIMMED TO DAYS 152-258, THEN 
#     POOLED ACROSS 3 YEARS AND Z-SCORED WITH THE POOLED MEAN AND POOLED S.D.
# Tstep is time step pooled for 2019, 2020, 2021
# X.dlm is pooled z-scored series, X.rawmean and X.rawsd are mean & sd for z score
# nl is DLM lags, delta is DLM delta, X.dlm is input to DLM, Tstep is time,
# local equilibria are X.eq and SD.eq, with ratio z.eq
# level (intercept) measures are level or stdlevel (for level/sd)
# Yyhat, B.ests, B.sd, errvar are DLM outputs
# Tday is time counter for daily means
# dmat is daily data: year, idoy, mean X.dlm, Z.eq, level, stdlevel
# save(useBGA,Tstep,X.dlm,level,levelsd,stdlevel,Yyhat,B.ests,B.sd,errvar,nl,delta,file=Fname)

load(file='./results/DLMresult_HYLB_Paul_ALL_Chl_Predicted to Manual Scale 098 NOISY ARIMA.Rdata')
Fname = c('./results/bootstrapped results/DLM_boot_Paul 1000.Rdata')

# Structure of Yyhat
#  Yyhat = matrix containing: time step, Y obs., yhat (one-step prediction), updated prediction variance
#     Dimension is (nobs-nl)x4 where nobs is number of observations and nl is number of lags

epsilon = Yyhat[,3] - Yyhat[,2]
Yoriginal = Yyhat[,2] # save the original Y with another name
Yhat.nominal = Yyhat[,3] # save Yhat from nominal model

print('Descriptive stats of errors',quote=F)
print(summary(epsilon))
print('N, S.D., skewness, kurtosis of epsilon',quote=F)
print(c(length(epsilon),sd(epsilon),skewness(epsilon),kurtosis(epsilon)),quote=F)

#windows(width=8,height=5)
par(mfrow=c(1,2),mar=c(4, 4.3, 4, 2) + 0.1, cex.axis=1.6,cex.lab=1.6)
eps.acf=acf(epsilon,lag.max=10)
eps.pacf=pacf(epsilon,lag.max=10)

print('',quote=F)
print(eps.acf)

Nboot = 1000  # number of bootstrap cycles

Tstep = Tstep[3:length(Tstep)]  
Ndlm = length(Tstep)
Bootlevel = matrix(0,nr=Ndlm,nc=(1+Nboot))
Bootlevel[,1] = Tstep
Bootsdlevel = Bootlevel

# DLM run details
nobs = length(X.dlm)
title = c('Bootstrap')

# BOOTSTRAP
tstart = Sys.time()

# 
# Bsd.list = vector("list", Nboot)
# Bests.list = vector("list", Nboot)
# Ypseudo.list = vector("list", Nboot)

k = 0

for(i in 1:Nboot) {

  print(i)
  
  
  repeat {  # keep retrying until no Inf appears
    
  set.seed(400+k) # makes it reproducible, but with a different random draw on each run


  eps.rand = sample(epsilon,size=length(epsilon),replace=T) # randomize eps
  Ypsuedo = Yhat.nominal + eps.rand
  print(c('boot cycle ',i),quote=F)
  ODL.out = ODLMAR(nl,delta,Ypsuedo,Tstep,title)
  # Output matrices are stored sideways, like MARSS
  #Yyhat = ODL.out[[1]]
  #EigenVals = ODL.out[[2]]  # only if eigenvalues computed
  B.ests = ODL.out[[2]]    # 3 if there are eigenvalues
  B.sd = ODL.out[[3]]      # 4 if eigenvalues
  #errvar = ODL.out[[5]] # updated error variance; 5 if eigenvalues
  level = B.ests[1,]
  stdlevel = B.ests[1,]/B.sd[1,]
  Bootlevel[,(i+1)] = level
  Bootsdlevel[,(i+1)] = stdlevel



 


  # redo the run if any values diverge
  if(any(is.infinite(stdlevel))){
    print(paste("RE-RUN", i))
    k = k + 1
    next
  }

  
  # Bsd.list[[i]] = B.sd
  # Bests.list[[i]] = B.ests
  # Ypseudo.list[[i]] = Ypsuedo
  
  break
  
  }
  # still increases k to get a new set of random numbers
  k = k+1

}




tstop = Sys.time()
runtime = tstop-tstart
print(c('Bootstrap run time = ',runtime),quote=F)
# 



which(is.infinite(Bootsdlevel[, 23]), arr.ind=TRUE)  # +1 because first column is Tstep



# Plot results
#windows(width=12,height=6)
# plot(Bootsdlevel[,1],Bootsdlevel[,2],type='l',lwd=1,col='blue',
#      xlab='timestep',ylab='stdlevel')
# for(iplot in 2:Nboot) {
#   points(Bootsdlevel[,1],Bootsdlevel[,(iplot+1)],type='l',
#          lwd=1,col='blue')
# }



# Plot all bootstrap standardized levels
# 
# par(mfrow=c(1,1),
#     mar=c(4,4.3,2,2)+0.1,
#     cex.axis=1.4,
#     cex.lab=1.4)
# 
# plot(Bootsdlevel[,1],
#      Bootsdlevel[,2],
#      type='l',
#      lwd=1,
#      col=rgb(0,0,1,0.2),
#      xlab='Time',
#      ylab='Standardized Level',
#      main='Bootstrap Standardized Level')
# 
# for(i in 3:(Nboot+1)){
#   lines(Bootsdlevel[,1],
#         Bootsdlevel[,i],
#         col=rgb(0,0,1,0.2),
#         lwd=1)
# }

save(Nboot,Bootlevel,Bootsdlevel, file=Fname)


# used to create v2 with more inormation for troubleshooting
#save(Nboot,Bootlevel,Bootsdlevel, Bsd.list, Bests.list, Ypseudo.list, file=Fname)

# 
# 
# par(mfrow=c(1,1),
#     mar=c(4,4.3,2,2)+0.1,
#     cex.axis=1.4,
#     cex.lab=1.4)
# 
# plot(Tstep[1:153512],
#      stdlevel,
#      type='l',
#      lwd=3,
#      col='red',
#      xlab='Time',
#      ylab='Standardized Level',
#      main='Original DLM Standardized Level')
