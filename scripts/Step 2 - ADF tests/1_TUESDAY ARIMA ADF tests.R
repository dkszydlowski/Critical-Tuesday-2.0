#### script for running diagnostics and formatting prior to DDJ
## this script uses the output from the DLM file

# ADF stationarity test and ARIMA for Markov lag
# SRC 2023-09-05
# adapted by DKS

rm(list = ls())
graphics.off()

library(stats)
library(tseries)
library(zoo)

library(forecast)

library(parallel)
options(mc.cores = parallel::detectCores(), digits = 10)

# Save post-processed DLM data
#
#save(useBGA,Tstep,X.dlm,level,levelsd,stdlevel,file=Fname)  
#load(file="DLMresult_YSI_Peter19.Rdata")

load("./results/DLMresult_HYLB_Tuesday_ALL_Chl_Predicted to Manual Scale 098 NOISY ARIMA.Rdata")

### try to make a separate dataset that we can run through DDJ, properly stacked

# thin standard level and tstep to get rid of autocorrelation
# this step is iterative, depending on AR order of the data from code at the bottom of the script
# because the data are AR(3), we thin to every 3rd point
aropt = 3
idx <- seq(1, length(stdlevel), by = aropt)
stdlevel = stdlevel[idx]
Tstep = Tstep[idx]

# 2013
idx13 <- grep(paste0("^", 2013), Tstep)

Xvar013  <- stdlevel[idx13]
nx13     <- length(Xvar013)
Tstep013 <- Tstep[idx13]

# 2014
idx14 <- grep(paste0("^", 2014), Tstep)

Xvar014  <- stdlevel[idx14]
nx14     <- length(Xvar014)
Tstep014 <- Tstep[idx14]

# 2015
idx15 <- grep(paste0("^", 2015), Tstep)

Xvar015  <- stdlevel[idx15]
nx15     <- length(Xvar015)
Tstep015 <- Tstep[idx15]

# 2024
idx24 <- grep(paste0("^", 2024), Tstep)

Xvar024  <- stdlevel[idx24]
nx24     <- length(Xvar024)
Tstep024 <- Tstep[idx24]


# 2025
idx25 <- grep(paste0("^", 2025), Tstep)

Xvar025  <- stdlevel[idx25]
nx25     <- length(Xvar025)
Tstep025 <- Tstep[idx25]




### trick to avoid jumps between years

make_transitions = function(Xvar, year){
  ns = length(Xvar)
  
  data.frame(
    year = year,
    x0   = Xvar[1:(ns-1)],
    x1   = Xvar[2:ns],
    dx   = Xvar[2:ns] - Xvar[1:(ns-1)]
  )
}

DDJ13 <- make_transitions(Xvar013, 2013)
DDJ14 <- make_transitions(Xvar014, 2014)
DDJ15 <- make_transitions(Xvar015, 2015)
DDJ24 <- make_transitions(Xvar024, 2024)
DDJ25 <- make_transitions(Xvar025, 2025)

DDJ_prepped <- rbind(DDJ13, DDJ14, DDJ15, DDJ24, DDJ25) %>% 
  filter(!is.na(dx))

write.csv(DDJ_prepped, "./data/formatted data/HF data/ARIMA Tuesday stacked data for global DDJ 098 NOISY THINNED.csv", row.names = FALSE) # this is really 098







### check adf and auto.arima

dx <- DDJ_prepped$dx
x0 <- DDJ_prepped$x0
n  <- length(dx)


# arfit_dx  <- auto.arima(dx)
# lagopt_dx <- arimaorder(arfit_dx)
# 
# print("AR structure of dx:")
# print(lagopt_dx)

arfit_x0  <- auto.arima(x0)
lagopt_x0 <- arimaorder(arfit_x0)

print("AR structure of x0:")
print(lagopt_x0)

# check results using adf.test
ADF.result = adf.test(x0)
pvalue = ADF.result$p.value
print(pvalue)

#
# aropt = unname(lagopt_x0[1])  # save optimal AR order
# # if optimal lag is 0 then data are uncorrelated, use original data
# aropt = ifelse(aropt==0,1,aropt)  
# 
# # subsample Xvar0 according to lagopt
# ikeep = seq(1,n,by=aropt)
# Xvar = x0[ikeep]
# Tstep = Tstep0[ikeep]
# nx=length(Xvar)
# 
# # check AR order of thin data
# arfit1 = auto.arima(Xvar)
# lagopt1 = arimaorder(arfit1)
# print('',quote=F)
# print('optimal order of thinned data using autoarima() and arimaorder()',quote=F)
# print(lagopt1)
# 
