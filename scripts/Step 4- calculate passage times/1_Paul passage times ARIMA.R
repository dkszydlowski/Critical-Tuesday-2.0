#### use the DDJ results to get passage times for Paul #####

# Direct count exit times from data each year
# SRC 2023-08-14
# updated 2026-01-10 DKS to apply to 5 years of Paul

# this code is run year-by-year, with the user updating keepyear for 2013, 2014, 2015, 2024, and 2025
# in sequence


rm(list = ls())
graphics.off()

# library(bvpSolve)
# library(cubature)
library(stats)
library(tictoc)
library(tidyverse)

options(mc.cores = parallel::detectCores())

# Load DLM result
#save(useBGA,Tstep,X.dlm,level,levelsd,stdlevel,file=Fname)  
# load(file='DLMresult_YSI_Peter19.Rdata')
load('./results/DLMresult_HYLB_Paul_ALL_Chl_Predicted to Manual Scale 098 NOISY ARIMA.Rdata')

# filter Tstep and stdlevel to year of interest
keepyear = 2015
idx13 <- grep(paste0("^", keepyear), Tstep)

stdlevel  <- stdlevel[idx13]
Tstep <- Tstep[idx13]


# thin the data to match DDJ
aropt=3 # this is from the adf step when we thinned
nx = length(stdlevel)
ikeep = seq(1, nx, by = aropt)

stdlevel = stdlevel[ikeep]
Tstep = Tstep[ikeep]

# put Tstep and stdlevel into a dataset
dat0 = as.data.frame(cbind(Tstep[1:length(stdlevel)],stdlevel))
# GATHER PASSAGE TIMES FOR SELECTED YEAR ====================================================================

# Load DDJ data so we have the threshold
load('./results/DDJ results Paul ARIMA-corrected data.Rdata')

#plot the time series
par(mfrow=c(1,1),mar=c(4,4,2,2)+0.1,cex.lab=1.5,cex.axis=1.5)
plot(dat0[,1],dat0[,2],type='l',lwd=1,col='blue',xlab='DoY',ylab='Chlorophyll, std level')
abline(h=xeq2[2],lty=2,lwd=2,col='red')

x0 = dat0[,2]

basin = ifelse(x0 < xeq2[2], "left", "right")
change = c(TRUE, basin[-1] != basin[-length(basin)]) # compare each value to the value before it
grp = cumsum(change)

ET = aggregate(rep(1, length(basin)), by = list(grp, basin), FUN = length)
names(ET) = c("event", "basin", "steps")
ET$minutes = ET$steps * 15 # 15 minutes, new time step of thinned data

ET_Tues_l = ET$minutes[ET$basin == "left"]
ET_Tues_r = ET$minutes[ET$basin == "right"]

mean(ET_Tues_l)
mean(ET_Tues_r)

ET$year = keepyear

write.csv(ET, paste("./results/passage times/Paul ARIMA-corrected ", keepyear," global 2026-06-18 THINNED.csv", sep = ""), row.names = FALSE)
