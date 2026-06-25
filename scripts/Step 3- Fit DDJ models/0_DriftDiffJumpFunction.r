# Function to compute nonparametric drift, diffusion, jump estimators
#    used in the paper "Early warnings of unknown nonlinear shifts: a nonparametric
#    approach" by S.R. Carpenter and W.A. Brock
# Programmer: Stephen R. Carpenter, June 2011
# R version: 2.11.1, downloaded 2010-05-31
#
# Inputs:
#
#  x0 is the predictor vector, which can be any covariate measured at the same
#     interval as the response variable. In the paper x0 is x-subscript-i*delta.
#  dx is the response variable vector. In the paper it is
#     x-subscript-(i+1)*delta minus x-subscript-i*delta
#  nx is length of the vector dx
#  DT is time step
#  bw is the bandwidth. See Hardle 1990 and Johannes 2004 (cited in the paper)
#     for advice about choice of bandwidth. In our experience, more stable,
#     accurate and precise estimates are obtained by using bandwidths larger
#     than the "optimal" bandwidths computed using 'density' {kernsmooth} and
#     similar algorithms.
#  na is the length of the avec vector
#  avec is a vector of values of the predictor variable for which nonparametric
#      estimates will be computed
#
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

Bandi4d <- function(x0,dx,nx,DT,bw,na,avec)  {
# Set up constants and useful preliminaries
SF <- 1/(bw*sqrt(2*pi))  # scale factor for kernel calculation
dx2 <- dx*dx # second power of dx
dx4 <- dx2*dx2  # fourth power of dx
dx6 <- dx2*dx4  # sixth power of dx
# Compute matrix of kernel values
Kmat <- matrix(0,nrow=na,ncol=nx)
for(i in 1:(nx)) {  # loop over columns (x0 values)
  Kmat[,i] <- SF*exp(-0.5*(x0[i]-avec)*(x0[i]-avec)/(bw*bw))
  }
# Compute M1, M2, M4 and moment ratio for each value of a
M1.a <- rep(0,na)
M2.a <- rep(0,na)
M4.a <- rep(0,na)
M6M4r <- rep(0,na)  # vector to hold column kernel-weighted moment ratio
for(i in 1:na) {  # loop over rows (a values)
  Ksum <- sum(Kmat[i,])  # sum of weights
  M1.a[i] <- (1/DT)*sum(Kmat[i,]*dx)/Ksum
  M2.a[i] <- (1/DT)*sum(Kmat[i,]*dx2)/Ksum
  M4.a[i] <- (1/DT)*sum(Kmat[i,]*dx4)/Ksum
  M6.c <- (1/DT)*sum(Kmat[i,]*dx6)/Ksum
  M6M4r[i] <- M6.c/M4.a[i]
  }
# Compute jump frequency, diffusion and drift
sigma2.Z <- mean(M6M4r)/(5) # average the column moment ratios
sigma.Z <- sqrt(sigma2.Z)
lamda.Z <- M4.a/(3*sigma2.Z*sigma2.Z)
sigma2.a <- M2.a - (lamda.Z*sigma2.Z)
sigma.diff <- ifelse(sigma2.a>0,sqrt(abs(sigma2.a)),0)
sigma2.x <- M2.a     # total variance
sigma.x <- sqrt(sigma2.x)
mu.x <- M1.a
outlist <- list(avec,mu.x,sigma.x,sigma.diff,sigma.Z,lamda.Z)
return(outlist)
} # end Bandi function
