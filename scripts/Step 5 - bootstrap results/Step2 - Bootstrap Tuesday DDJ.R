# Bootstrap DDJ after DLM
# SRC 2023-08-13

rm(list = ls())
graphics.off()

source('./scripts/Langevin/HF Langevin/Langevin analysis/DriftDiffJumpFunction.r')

source('./scripts/Langevin/HF Langevin/Langevin analysis/EPFunction+EQ.R')

library(forecast)
library(tidyverse)
library(parallel)
options(mc.cores = parallel::detectCores())

# Load result of DLM bootstrap
#save(Nboot,Bootlevel,Bootsdlevel,file=Fname)
load(file="'./results/bootstrapped results/DLM_boot_Tuesday 1000.Rdata'")
# Name of output file
Fname = c('./results/bootstrapped results/DDJ_boot_Tuesday 1000.Rdata')

# Extract data for selected year
bdat0 = Bootsdlevel

# Thin data by aropt = Markov thinning factor
aropt = 2
nx0 = length(bdat0[,1])
# subsample bdat0 according to aropt
# ikeep = seq(1,nx0,by=aropt)
# bdat = bdat0[ikeep,]

# set up matrices for DDJ results: avec, D1, total D2
na = 1000  # length of avec
amat = matrix(0,nr=na,nc=Nboot)
D1mat = matrix(0,nr=na,nc=Nboot)
D2mat = matrix(0,nr=na,nc=Nboot)
sigmat = matrix(0,nr=na,nc=Nboot)
xeqmat= matrix(0,nr=3,nc=Nboot)
EPxeqmat = matrix(0,nr=3,nc=Nboot)

#Tstep = bdat[,1] # save Tstep of bootstrap DLM
DT = aropt/(24*12)  # time step of 5-minute data

# function for getting rid of jumps between years
make_transitions = function(Xvar, Tstep, year){
  
  idx = grep(paste0("^", year), Tstep)
  
  Xy = Xvar[idx]
  ny = length(Xy)
  
  if(ny < 2) return(NULL)
  
  data.frame(
    x0 = Xy[1:(ny-1)],
    dx = Xy[2:ny] - Xy[1:(ny-1)]
  )
}

cols_with_inf <- apply(bdat0, 2, function(x) any(is.infinite(x)))

which(cols_with_inf)

min_row_inf <- sapply(1:ncol(bdat0), function(j) {
  inf_rows <- which(is.infinite(bdat0[, j]))  # rows with Inf/-Inf in this column
  if(length(inf_rows) == 0) return(NA)        # no Inf in this column
  min(inf_rows)                               # first row with Inf
})

# Show results
names(min_row_inf) <- colnames(bdat0)  # optional: attach column names
min_row_inf


# Count number of infinite values per column
inf_count <- apply(bdat0, 2, function(x) sum(is.infinite(x)))

# Optional: attach column names
names(inf_count) <- colnames(bdat0)

# Show results
inf_count

# NO INFINITE VALUES ANYMORE

# BOOTSTRAP DDJ
tstart = Sys.time()

print(c('starting DDJ bootstrap '),quote=F)
for(ib in 1:Nboot) {  # start bootstrap loop
  # construct inputs to Bandi function 
  #Bandi4d <- function(x0,dx,nx,DT,bw,na,avec)
  
  
  # 
  # Xvar = bdat[,(ib+1)] # bootstrapped stdlevel
  # nx = length(Xvar)
  # x0= Xvar[1:(nx-1)]
  # x1= Xvar[2:nx]
  # dx = x1-x0
  
  Xvar_full = bdat0[,(ib+1)]
  T_full    = bdat0[,1]
  
  years = c(2013, 2014, 2015, 2024, 2025)
  
  trans_list = list()
  
  for (yy in years) {
    
    idx = grep(paste0("^", yy), T_full)
    
    Xy = Xvar_full[idx]
    
    # THIN WITHIN YEAR
    ikeep = seq(1, length(Xy), by = aropt)
    Xy = Xy[ikeep]
    
    if(length(Xy) < 2) next
    
    trans_list[[as.character(yy)]] =
      data.frame(
        x0 = Xy[1:(length(Xy)-1)],
        dx = diff(Xy)
      )
  }
  
  DDJ_stack = do.call(rbind, trans_list)
  
  x0 = DDJ_stack$x0
  dx = DDJ_stack$dx
  nx = length(dx)
  
  
  xrange = range(x0,na.rm=T)
  bw = 0.09*(xrange[2]-xrange[1]) # tie bandwidth to range of data
  amin = xrange[1] #+ bw  # set first mesh point 1 bw above minimum
  amax = xrange[2] #- bw  # set mesh endpoint 1 bw below maximum
  avec = seq(from=amin,to=amax,length.out=na) 
  print(c('starting Bandi fit, boot cycle ',ib),quote=F)
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
  #
  DDJ1 = Bandi4d(x0,dx,(nx-1),DT,bw,na,avec)
  # unpack result
  D1 = DDJ1[[2]]
  sigma = DDJ1[[4]]
  jumpsig = DDJ1[[5]]
  lamda = DDJ1[[6]]
  # Total D2 from Johannes: sum of diffusion & jump variances
  D2 = sigma^2 + lamda*(jumpsig^2)
  sig.D2 = sqrt(D2) # To stick with the original EPFQ, we want to use just D2, not 2*D2
  # check deterministic equilibria
  sdrift = sign(D1)
  dsdrift = c(0,-diff(sdrift))
  xeq = avec[which(!dsdrift == 0)]
  if(length(xeq) != 3) 
    { next } # move to next row if there are not 3 equilibria
  print(c('D1 equilibria ',xeq),quote=F)
  xeqmat[,ib] = xeq
  # check equilibria of effective potential
  EPinput = as.data.frame(cbind(avec,D1,sig.D2))
  # screen out missing values if present
  EPin = na.omit(EPinput)
  EPout = EPFEQ(EPin$avec,EPin$D1,EPin$sig.D2)
  xeqEP = EPout[[4]]
  if(length(xeqEP) != 3) 
  { next } # equilibrium to next row if there are not 3 equilibria
  print(c('EP equilibria: ',xeqEP),quote=F)
  EPxeqmat[,ib] = xeqEP
  # Now store results in output matrices
  amat[,ib] = avec
  D1mat[,ib] = D1
  D2mat[,ib] = D2
  sigmat[,ib] = sig.D2
}

tstop = Sys.time()
runtime = tstop-tstart
print(c('Bootstrap run time = ',runtime),quote=F)

# save results
# bdat0 is the bootstrapped DLM output, bdat is Markov-thinned bdat0
save(bdat0,aropt,Nboot,amat,D1mat,D2mat,sigmat,xeqmat,EPxeqmat,file=Fname)
print(c('result saved in ',Fname),quote=F)


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

# D1 is drift


plot.DDJ = data.frame(avec =avec, D1 = D1, D2 = D2, sig.D2 = sig.D2)

drift_diff = plot.DDJ %>% 
  select(avec, D1, D2) %>% 
  pivot_longer(cols = c("D1", "D2"), names_to = "estimate") %>% 
  mutate(estimate = replace(estimate, estimate == "D1", "Drift")) %>% 
  mutate(estimate = replace(estimate, estimate == "D2", "Diffusion (as s.d.)"))
  


ggplot(drift_diff, aes(x = avec, y = value, color = estimate, linetype = estimate))+
  geom_line(size = 1.2)+
  geom_hline(yintercept = 0, linetype = "dotted")+
  theme_classic()+
  labs(x = "Chlorophyll",  y = "Drift or diffusion")+
  scale_color_manual(values = c("Drift" = "blue", "Diffusion (as s.d.)" = "red"))+
  scale_linetype_manual(values = c("Drift" = "solid", "Diffusion (as s.d.)" = "dashed"))+
  theme(legend.title = element_blank())





















# Deterministic equilibria
valid_det = colSums(xeqmat != 0) == 3
xeq_valid = xeqmat[, valid_det]

# EP equilibria
valid_ep = colSums(EPxeqmat != 0) == 3
EPxeq_valid = EPxeqmat[, valid_ep]

cat("Valid deterministic runs:", sum(valid_det), "\n")
cat("Valid EP runs:", sum(valid_ep), "\n")





det_mean = apply(xeq_valid, 1, mean)
det_median = apply(xeq_valid, 1, median)
det_sd   = apply(xeq_valid, 1, sd)

det_summary = data.frame(
  Equilibrium = c("Lower", "Middle", "Upper"),
  Mean = det_mean,
  SD = det_sd,
  Median = det_median
)

print(det_summary)


EPxeqmat

mid_det = xeqmat[2, ]
mid_det = mid_det[!is.na(mid_det)]

plot(density(mid_det),
     lwd = 2,
     col = "blue",
     main = "Density of Middle Deterministic Equilibrium",
     xlab = "Equilibrium value")
grid()



mid_ep = EPxeqmat[2, ]
mid_ep = mid_ep[mid_ep != 0]





mid_det = xeqmat[2, ]
mid_det = mid_det[!is.na(mid_det)]

mid_ep = EPxeqmat[2, ]
mid_ep = mid_ep[!is.na(mid_ep)]

plot(density(mid_det),
     lwd = 2,
     col = "blue",
     main = "Density of Middle Equilibrium",
     xlab = "Equilibrium value")

lines(density(mid_ep), lwd = 2, col = "red")

legend("topright",
       legend = c("Deterministic", "Effective Potential"),
       col = c("blue","red"),
       lwd = 2)

grid()

