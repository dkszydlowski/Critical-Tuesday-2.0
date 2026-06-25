##### run Paul's DLM on all years of data ######
# uses the sonde data that was corrected to a manual scale in the "sonde vs manual.R" script


# DLM - may help stationarity of variate
# SRC 2023-09-11
# need to adjust digits displayed to create the Tscore for 5-minute data
options(digits = 9)
library(tidyverse)

rm(list = ls())
graphics.off()

#source('DriftDiffJumpFunction.r')
source('./scripts/Step1 - standardize with DLM/0 - ODLMAR_NoBoot_2018-10-20.R')

#library(svMisc)  # used only if eigenvalues are bootstrapped

#library(parallel)
#options(mc.cores = parallel::detectCores())

# Load data: 
# T15 is the Peter subset of the Excel sheet
# Rdd is the daily data matrix 
# Columns are dayint, mean temp HYLB, delta pH HYLB, delta DoSat HYLB,
#   mean log10 chl HYLB, mean log10 Chl HYLB
# "delta" is daily max - daily min as index of production
#save(T15,Rdd,file=Fname)
#
# CHOOSE HYLB OR YSI used for dailies Rdd
#Fname = c('Peter2019_HYLB_Chl+Chl.Rdata') 
# Fname = c('Peter2019_YSI_Chl+Chl.Rdata') 
#
# load(file='Peter2019_YSI_Chl+Chl.Rdata')
# Fname = c('DLMresult_YSI_Peter19.Rdata')
# print(T15[1,])
# title = c('Peter Lake 2019')

Fname = c('./results/DLMresult_HYLB_Paul_ALL_Chl_Predicted to Manual Scale 098 NOISY ARIMA.Rdata')

title = c('Paul Lake 2013-2015, 2024+2025')

#L.all = read.csv("./data/formatted data/HF data/Predicted Paul HYLB 2013-2015 2024 2025 log-trans.csv") %>% 
 # mutate(Lake = "L")


# read in the data and sort by datetime
# all of the datasets are 5-minute
L.all = read.csv("./data/formatted data/HF data/Predicted Paul HYLB on Manual Scale log-trans NOISY ARIMA.csv") %>% 
  mutate(Lake = "L") %>% 
  arrange(datetime)


# chl_summary <- L.all %>%
#   filter(!is.na(Chl_HYLB)) %>%      # keep rows with chl data
#   group_by(Year) %>%
#   summarize(
#     n_chl   = n(),
#     min_DoY = min(DoY, na.rm = TRUE),
#     max_DoY = max(DoY, na.rm = TRUE)
#   ) %>%
#   arrange(Year) %>% 
#   mutate(DoY = max_DoY - min_DoY)

### Create a Tscore that combines year and DoY
mindoy = min(L.all$DoY, na.rm = TRUE)
maxdoy = max(L.all$DoY, na.rm = TRUE)

L.all = L.all %>% 
 mutate(Tscore = Year + ( (DoY - mindoy)/(maxdoy-mindoy+1)))


# plot Chl estimates
#windows(width=10,height=10)
#par(mfrow=c(2,1),mar=c(4, 4.2, 1, 2) + 0.1,cex.axis=1.6,cex.lab=1.6)
plot(L.all$Tscore,L.all$Chl_HYLB_cal,type='l',col='blue')

# X.dlm will be the sequence over years z-scored using a common mean and s.d.

useChl0 = subset(L.all,select=c(DoY, lsonde_cal, Chl_HYLB_cal, Tscore)) 
# using lsonde_cal, which is the log-transformed sonde data calibrated to manual

print(c('dim of selected data = ',dim(useChl0)),quote=F)
useChl = na.omit(useChl0)
print(c('dim after na.omit = ',dim(useChl)),quote=F)
#
Tstep = useChl$Tscore
X0 = useChl$lsonde_cal
X.dlm = (X0 - mean(X0))/sd(X0)  # Z-score to all years
#X.dlm = X0
print(c('range X.dlm = ',range(X.dlm,na.rm=T)),quote=F)

# Start DLM
#windows(width=12,height=6)
plot(Tstep,X.dlm,type='l',col='forestgreen',xlab='DoY index',ylab='X.dlm',
     main='Chl for DLM')
grid()

# Set up DLM
nobs = length(X.dlm)
nl = 1 # number of lags
print('**************',quote=F)
print(' ',quote=F)
print('**************',quote=F)
delta = 0.98 # 0<delta<1; see advice in functions

# Run DLM
ODL.out = ODLMAR(nl,delta,X.dlm,Tstep,title)

# Output matrices are stored sideways, like MARSS
Yyhat = ODL.out[[1]]
EigenVals = ODL.out[[2]]
B.ests = ODL.out[[3]]  
B.sd = ODL.out[[4]]
errvar = ODL.out[[5]] # updated error variance

# Post process DLM -----------------------------------------------

# Calculate moving equilibrium
X.eq = B.ests[1,]/(1 - B.ests[2,])
# Calculate its variance
#SDterm1 = X.eq*X.eq
#SDterm2 = (B.sd[1,]*B.sd[1,] + errvar)/(B.ests[1,]*B.ests[1,])
#SDterm3 = (B.sd[2,]*B.sd[2,])/((1 - B.ests[2,])*(1 - B.ests[2,]))
#SD.eq = sqrt(SDterm1*(SDterm2 + SDterm3))
# From blue notebook p 70 2023-04-20
deno = (1 - B.ests[2,])
Vterm1 = (B.sd[1,]*B.sd[1,] + errvar)/(deno*deno)
Vterm2 = ((B.ests[1,]*B.sd[2,])/(deno*deno))*((B.ests[1,]*B.sd[2,])/(deno*deno))
SD.eq = sqrt(Vterm1+Vterm2)

# Z score
Z.eq = X.eq/SD.eq

# Time steps start at 2
Nstep = length(Tstep)

# Plot components of steady-state estimate
#windows(width=6,height=12)
par(mfrow=c(3,1),mar=c(4, 4.2, 3, 2) + 0.1,cex.axis=1.6,cex.lab=1.6)
plot(Tstep[2:Nstep],X.eq,type='l',col='blue',ylim=c(-10,10),
     ylab='Steady State',xlab='DoY',
     main='Local Steady-State estimate, sd, and ratio')
grid()
plot(Tstep[2:Nstep],SD.eq,type='l',col='red',ylim=c(0,10),
     ylab='S.D.',xlab='DoY')
grid()
plot(Tstep[2:Nstep],Z.eq,type='l',col='purple',
     ylab='Z score',xlab='DoY')
grid()


# plot(Tstep, X.dlm,
#      type = 'l',
#      col = 'forestgreen',
#      xlab = 'DoY index',
#      ylab = 'X.dlm',
#      main = 'Chl for DLM')
# 
# # Add model estimates as points
# points(Tstep[2:Nstep], Yyhat[,3],
#        pch = 16,
#        col = 'red')

grid()

### save these things for plotting
save(Tstep, X.dlm, Nstep, Yyhat, file = './results/DLMresult_HYLB_Paul_ALL_Chl_Predicted to Manual Scale 098 NOISY PLOTTING ARIMA.Rdata')

# Calculate level estimates
level = B.ests[1,]
levelsd = B.sd[1,]
stdlevel = B.ests[1,]/B.sd[1,]

# Plot components of level estimate
#windows(width=12,height=9)
par(mfrow=c(3,1),mar=c(4, 4.2, 3, 2) + 0.1,cex.axis=1.6,cex.lab=1.6)
plot(Tstep[2:Nstep],level,type='l',col='deepskyblue',#ylim=c(-10,10),
     ylab='level',xlab='DoY',
     main='Level and Std level estimate')
grid()
plot(Tstep[2:Nstep],levelsd,type='l',col='blue',#ylim=c(-10,10),
     ylab='level s.d',xlab='DoY')
plot(Tstep[2:Nstep],stdlevel,type='l',col='red',#ylim=c(0,10),
     ylab='Std Level',xlab='DoY')
grid()

# Density plots
dens.Xdlm = density(X.dlm,bw='SJ',window="epanechnikov",n=512,na.rm='T')
dens.slev = density(stdlevel,bw='SJ',window="epanechnikov",n=512,na.rm='T')
dens.lev = density(level,bw='SJ',window="epanechnikov",n=512,na.rm='T')

#windows(width=10,height=6)
par(mfrow=c(1,3),mar=c(4, 4.2, 3, 2) + 0.1,cex.axis=1.6,cex.lab=1.6)
plot(dens.Xdlm$x,dens.Xdlm$y,type='l',lwd=2,col='forestgreen',xlab='Z score of log chlorophyll',
     ylab='density')
plot(dens.lev$x,dens.lev$y,type='l',lwd=2,col='forestgreen',xlab='level of log chlorophyll',
     ylab='density')
plot(dens.slev$x,dens.slev$y,type='l',lwd=2,col='forestgreen',xlab='Standardized Level',
     ylab='density')

save(useChl,Tstep,X.dlm,level,levelsd,stdlevel,Yyhat,B.ests,B.sd,errvar,nl,delta,file=Fname)
print(Fname,quote=F)

