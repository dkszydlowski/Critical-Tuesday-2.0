# DDJ model
# SRC 2023-06-02
# updated by DKS 2026-01-15
# updated to run DDJ on stacked data from the DLM, to get one single fit


rm(list = ls())
graphics.off()

source('./scripts/Step 3- Fit DDJ models/0_DriftDiffJumpFunction.r')
#source('ODLMAR_NoBoot_2018-10-20.R')

source('./scripts/Step 3- Fit DDJ models/0_EPFunction+EQ.R')

library(forecast)
library(cubature)
library(parallel)
library(zoo)
options(mc.cores = parallel::detectCores())

# Save post-processed DLM data
#
#save(useBGA,Tstep,X.dlm,level,levelsd,stdlevel,file=Fname)  


#load(file="./scripts/Langevin/HF Langevin/DLMresult_HYLB_Tuesday_ALL_Chl.Rdata")
Fname = c(paste('./results/DDJ results Tuesday ARIMA-corrected data.Rdata', sep = ""))

# read in dataframe from diagnostic step
DDJ_prepped = read.csv("./data/formatted data/HF data/ARIMA Tuesday stacked data for global DDJ 098 NOISY THINNED.csv")

## new modified
nx = nrow(DDJ_prepped)

# thin the data (ALREADY THINNED PREVIOUSLY)
# aropt = 2
# ikeep = seq(1, nx, by = aropt)

# could subset each of these by ikeep if not already thinned
Xvar = DDJ_prepped$x0
x0 <- DDJ_prepped$x0
dx <- DDJ_prepped$dx

nx <- length(dx)
Tstep0 = c(1:nx)
Tstep = c(1:nx)


plot(Tstep0, Xvar, type="l", col="blue")


# construct inputs to Bandi function 
#Bandi4d <- function(x0,dx,nx,DT,bw,na,avec)
#x0= Xvar[1:(nx-1)]
#x1= Xvar[2:nx]
#dx = x1-x0

aropt = 3

DT = aropt/(24*12)  # time step of 5-minute data
xrange = range(x0,na.rm=T)
xsd = sd(x0, na.rm = T)
bw = 0.09*(xrange[2]-xrange[1]) # tie bandwidth to range of data, or standard deviation
#bw = 0.4*(xsd)
na = 1000  # number of mesh points (nominal 200)
amin = xrange[1] #+ bw  # set first mesh point 1 bw above minimum
amax = xrange[2] #- bw  # set mesh endpoint 1 bw below maximum
avec = seq(from=amin,to=amax,length.out=na) 


#=======================================================================================================================================#
#=======================================================================================================================================#
#=======================================================================================================================================#
# Run Bandi function
# Output: The function returns a list of 6 variables, as follows:
#
# avec: same as the input avec
# mu.x: vector of nonparametric drift estimates for each element of avec
# sigma.x: vector of nonparametric total sigma (not sigma^2) estimates for
#     each element of avec
# sigma.diff: vector of nonparametric diffusion sigma (not sigma^2) estimates for
#     each element of avec
# sigma.z: a scalar; nonparametric jump sigma (not sigma^2)
# lamda.z: vector of nonparametric jump frequency (or jump intensity) lamda
#     estimates for each element of avec

DDJ1 = Bandi4d(x0,dx,(nx-1),DT,bw,na,avec)

# unpack result
D1 = DDJ1[[2]]
totsig = DDJ1[[3]]
sigma = DDJ1[[4]]
jumpsig = DDJ1[[5]]
lamda = DDJ1[[6]]

print(c('jump magnitude (sigma) ',round(jumpsig,5)),quote=F)

#windows(height=10,width=5)
par(mfrow=c(4,1),mar=c(4, 4.3, 1, 2) + 0.1,cex.axis=1.6,cex.lab=1.8)
plot(avec,D1,type='l',lwd=2,col='blue',ylab='Drift')
grid()
abline(h=0,lty=3,col='red')
plot(avec,sigma,type='l',lwd=2,col='blue',ylab='diffusion sigma')
grid()
plot(avec,lamda,type='l',lwd=2,col='blue')
grid()

print('deterministic equilibria of D1',quote=F)
# Find equilibria
sdrift = sign(D1)
dsdrift = c(0,-diff(sdrift))
xeq = avec[which(!dsdrift == 0)]
ixeq = which(!dsdrift == 0)  # indices of the equilibria

print('',quote=F)
print('equilibria from D1 on avec',quote=F)
print(xeq,quote=F)
print(ixeq,quote=F)  

# Total D2 from Johannes: sum of diffusion & jump variances
# Johannes also calls this conditional variance
D2 = sigma^2 + lamda*(jumpsig^2)
sig.D2 = sqrt(D2)  # Bandi, Johannes and we do not divide by q when computing moment q  

# add conditional variance to the plot
plot(avec,D2,type='l',lwd=2,col='blue',ylab='Conditional Variance')
grid()

# Equilibria from effective potential based on conditional variance
#
# screen out missing values if present
Xs=avec
sigvec = sig.D2
EPinput = as.data.frame(cbind(Xs,D1,sigvec))
EPin = na.omit(EPinput)
EPout = EPFEQ(EPin$Xs,EPin$D1,EPin$sigvec)
#outlist = list(xvec.ep,EPF,dEPdx,xeq)
xvec.ep1 = EPout[[1]]
epf1 = EPout[[2]]
dEPdx1 = EPout[[3]]
xeq2 = EPout[[4]]

# Print equilibria from EPF
print('Equilibria on avec accounting for noise',quote=F)
print(xeq2,quote=F)
#print('log of equilibria accounting for noise',quote=F)
#print(log(xeq2),quote=F)
#print('',quote=F)
print('',quote=F)

# Plot EPF
#windows(width=8,height=4)
par(mfrow=c(1,2),mar=c(4,4.1,1,2)+0.1,cex.lab=1.6,cex.axis=1.8)
plot(xvec.ep1[2:100],epf1,type='l',lwd=2,col='blue',xlab='X',ylab='Effective Potential')
grid()
# sign of derivative was corrected in EPfunction
plot(xvec.ep1,dEPdx1,type='l',lwd=2,col='blue',xlab='X',ylab='-d(EP)/dx')
abline(h=0,lty=3,lwd=2,col='black')
grid()

x1 = DDJ_prepped$x1

save(avec,D1,sigma,jumpsig,lamda,D2,sig.D2,bw,x0,x1,dx,DT,Tstep,xeq,EPout,
     xeq2,file=Fname)

### make a dataframe of the model
all = data.frame(X = xvec.ep1[2:100], efective.potential = epf1)

all.deriv = data.frame(X = xvec.ep1, deriv.ef.pot = dEPdx1)

write.csv(all, paste("./results/DDJ results Tuesday ARIMA-correced", ".csv", sep = ""), row.names = FALSE)
write.csv(all.deriv, paste("./results/DDJ results deriv Tuesday ARIMA-correced", ".csv", sep = ""), row.names = FALSE)

