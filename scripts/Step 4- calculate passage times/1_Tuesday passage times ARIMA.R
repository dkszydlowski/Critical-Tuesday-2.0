#### use the DDJ results to get passage times for Tuesday #####

# Direct count exit times from data each year
# SRC 2023-08-14
# updated 2026-01-10 DKS to apply to 5 years of Tuesday

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
load('./results/DLMresult_HYLB_Tuesday_ALL_Chl_Predicted to Manual Scale 098 NOISY ARIMA.Rdata')

# filter Tstep and stdlevel to year of interest
keepyear = 2015
idx13 <- grep(paste0("^", keepyear), Tstep)

stdlevel  <- stdlevel[idx13]
Tstep <- Tstep[idx13]

# if(keepyear == 2013){
#   test = data.frame(stdLevel = stdlevel,Tstep =  Tstep) %>%
#     filter(Tstep >= 	
#              2013.11800 )
#   
#   stdlevel = test$stdLevel
#   nx13 = length(stdlevel)
#   Tstep = test$Tstep
# }

# thin the data to match DDJ
aropt=3
nx = length(stdlevel)
ikeep = seq(1, nx, by = aropt)

stdlevel = stdlevel[ikeep]
Tstep = Tstep[ikeep]

#DT = aropt*5/(24*12)  # time step, days, of thinned data for 5-minute time steps


# put Tstep and stdlevel into a dataset
dat0 = as.data.frame(cbind(Tstep[1:length(stdlevel)],stdlevel))

# GATHER PASSAGE TIMES FOR SELECTED YEAR ====================================================================

# Load DDJ data 
#save(avec,D1,totsig,sigma,jumpsig,lamda,bw,x0,x1,dx,DT,Tstep,xeq,EPout,xeq2,file=Fname)
#load(file='DDJ_YSI_Peter19.Rdata')


load('./results/DDJ results Tuesday ARIMA-corrected data.Rdata')

#windows()
par(mfrow=c(1,1),mar=c(4,4,2,2)+0.1,cex.lab=1.5,cex.axis=1.5)
plot(dat0[,1],dat0[,2],type='l',lwd=1,col='blue',xlab='DoY',ylab='Chlorophyll, std level')
abline(h=xeq2[2],lty=2,lwd=2,col='red')

# Find and plot crossing times of observed data
x0 = dat0[,2]
dev = x0 - xeq2[2] # deviation from threshold
sdev = sign(dev)
dsx = c(0,diff(sdev))
Tmax = length(dsx)
Tcount = c(1:Tmax)
tup = Tcount[which(dsx > 0)]  # jumps upward across threshold
tdn = Tcount[which(dsx < 0)]  # jumps downward across threshold

# if the first jump was up:
if(tup[1] < tdn[1]) {
  ET_Tues_r = tdn - tup[1:length(tdn)]  # exit times from right basin
  ET_Tues_l = tup - c(0,tdn)  # exit times from left basin
}
# if the first jump was down
if(tup[1] > tdn[1]) {
  ET_Tues_r = tdn - c(0,tup[1:(length(tdn)-1)])
  ET_Tues_l = tup - tdn[1:length(tup)]
}

# Unit is 5 minutes, or DT days
# Convert to minutes:
ET_Tues_r = ET_Tues_r*5
ET_Tues_l = ET_Tues_l*5

# boxplots
#windows()
par(mfrow=c(1,1),mar=c(4,4,2,2)+0.1,cex.lab=1.5,cex.axis=1.5)
boxplot(log10(ET_Tues_l),log10(ET_Tues_r),
        at=c(1,2),
        names=c('left','right'),
        col=c('lightskyblue','lightgreen'),
        border='black',horizontal=T,notch=F,
        xlab='log10(ET, minutes)',main='20_Tues_')



# plot non-log
par(mfrow=c(1,1),mar=c(4,4,2,2)+0.1,cex.lab=1.5,cex.axis=1.5)
boxplot((ET_Tues_l),(ET_Tues_r),
        at=c(1,2),
        names=c('left','right'),
        col=c('lightskyblue','lightgreen'),
        border='black',horizontal=T,notch=F,
        xlab='log10(ET, minutes)',main='20_Tues_')

print('',quote=F)
print('ET summary statistics, left then right',quote=F)
pvec = c(0.1,0.5,0.9)
print(quantile(ET_Tues_l,probs=pvec))
print(c('mean ',mean(ET_Tues_l),', sd ',sd(ET_Tues_l),', N ',length(ET_Tues_l)),quote=F)
print('right',quote=F)
print(quantile(ET_Tues_r,probs=pvec))
print(c('mean ',mean(ET_Tues_r),', sd ',sd(ET_Tues_r),', N ',length(ET_Tues_r)),quote=F)

# since there are few crossings, make a sorted list
print('',quote=F)
print('sorted list of left passage times, minutes',quote=F)
print(sort(ET_Tues_l))
print('sorted list of right passage times, minutes',quote=F)
print(sort(ET_Tues_r))
print('',quote=F)
print('sorted list of left passage times, days',quote=F)
print(sort(ET_Tues_l)/(24*60))
print('sorted list of right passage times, days',quote=F)
print(sort(ET_Tues_r)/(24*60))

# How is time allocated among passage events?  Left--
sET_Tues_l = sort(ET_Tues_l)
sumET_Tues_l = sum(ET_Tues_l)
pET_Tues_l = sET_Tues_l/sumET_Tues_l # proportion of total passage time per event
cpET_Tues_l = cumsum(pET_Tues_l) # cumulative proportions
#
tET_Tues_l = sET_Tues_l*pET_Tues_l  # time spent in each passage event
ctET_Tues_l = cumsum(tET_Tues_l) # cumulative time spent in passage events
ptET_Tues_l = ctET_Tues_l/sumET_Tues_l # proportion of time for each passage

#windows(width=10,height=5)
par(mfrow=c(1,2),mar=c(4,4,2,2)+0.1,cex.lab=1.5,cex.axis=1.5)
#
plot(sET_Tues_l,ptET_Tues_l,log='xy',type='p',pch=19,col='darkblue',
     xlab='Left ET, minutes',
     ylab='Proportion of Time',main='Left Basin')
grid()
#
plot(sET_Tues_l,cpET_Tues_l,log='xy',type='p',pch=19,col='darkblue',
     ylab='Cumulative Proportion',
     xlab='Left ET, minutes',main='Cumulative Proportion of Time')
grid()

# How is time allocated among passage events?  Right--
sET_Tues_r = sort(ET_Tues_r)
sumET_Tues_r = sum(ET_Tues_r)
pET_Tues_r = sET_Tues_r/sumET_Tues_r # proportion of total passage time per event
cpET_Tues_r = cumsum(pET_Tues_r) # cumulative proportions
#
tET_Tues_r = sET_Tues_r*pET_Tues_r  # time spent in each passage event
ctET_Tues_r = cumsum(tET_Tues_r) # cumulative time spent in passage events
ptET_Tues_r = ctET_Tues_r/sumET_Tues_r # proportion of time for each passage

#windows(width=10,height=5)
par(mfrow=c(1,2),mar=c(4,4,2,2)+0.1,cex.lab=1.5,cex.axis=1.5)
#
plot(sET_Tues_r,ptET_Tues_r,log='xy',type='p',pch=19,col='forestgreen',
     xlab='Right ET, minutes',
     ylab='Proportion of Time',main='Right Basin')
grid()
#
plot(sET_Tues_r,cpET_Tues_r,log='xy',type='p',pch=19,col='forestgreen',
     ylab='Cumulative Proportion',
     xlab='Right ET, minutes',main='Cumulative Proportion of Time')
grid()


#### save everything so we can compare among years ####


basin = ifelse(x0 < xeq2[2], "left", "right")
change = c(TRUE, basin[-1] != basin[-length(basin)]) # compare each value to the value before it
grp = cumsum(change)

ET = aggregate(rep(1, length(basin)), by = list(grp, basin), FUN = length)
names(ET) = c("event", "basin", "steps")
ET$minutes = ET$steps * 15 # 15 minutes

ET_Tues_l = ET$minutes[ET$basin == "left"]
ET_Tues_r = ET$minutes[ET$basin == "right"]

mean(ET_Tues_l)
mean(ET_Tues_r)

ET$year = keepyear

write.csv(ET, paste("./results/passage times/Tuesday ARIMA-corrected ", keepyear," global 2026-06-18 THINNED.csv", sep = ""), row.names = FALSE)
