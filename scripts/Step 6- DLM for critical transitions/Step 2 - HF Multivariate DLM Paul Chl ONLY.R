#Paul Lake DLM for identifing critical transitions

#### Fit to the HF data #####
# hope is that this matches more closely to the passage times #

library(tidyverse)
library(ggpubr)


# ## select which year we want to run through the DLM ##
# year = 2025 # can either be "All Years" or the year of interest

### loop through all years and save the results ###
years = c(2013, 2014, 2015, 2024, 2025)

deltas = c(0.90, 0.91, 0.92, 0.93, 0.94, 0.95, 0.96, 0.97, 0.98, 0.99)

Fname = "./results/Paul CT DLM.csv"


L.all = read.csv("./data/formatted data/HF data/Predicted Paul HYLB on Manual Scale log-trans NOISY ARIMA.csv") %>% 
  mutate(Lake = "L") %>% 
  arrange(datetime)

for(k in 1:length(years)){
  year = years[k]
  
  for(j in 1:length(deltas)){
    delta.input = deltas[j]
    
    
    
    
    #======================================
    # Cleaning up the data set to run through the DLM
    
    if(year != "All years"){
      sonde = L.all %>% 
        mutate(datetime = ymd_hms(datetime)) %>% 
        filter(Year == year) %>% 
        rename(doy_frac = DoY) %>% #rename to capture what's actually in the column
        mutate(doy = trunc(doy_frac)) %>% #create a DOY variable
        mutate(
          chl = Chl_HYLB) %>% 
        filter(format(datetime, "%H") %in% c("06", "07", "08", "09")) %>% 
        select(doy_frac, doy, chl)
      
      
      
daily_mean = sonde %>% 
  group_by(doy) %>%
  summarise(
    chl_mean = mean(chl, na.rm = TRUE))%>%
  ungroup()
      
    }
    
    
    
    
    #Calculate daily average values
    #NOTE: can also trim to specific time periods during the day using doy_frac
    
    
    # Plot everything up so we know what we're working with
    chl_plot = 
      ggplot(daily_mean, aes(x = doy, y = chl_mean)) + 
      geom_point(color = '#117733') + geom_path(color = '#117733') +
      theme_bw() + 
      xlab("") + ylab("") + ggtitle("Chlorophyll, Log Transformed")
    
    ggarrange(chl_plot,
              nrow = 1, ncol = 1, align = 'v')
    
    #============================================
    # Attempt multivariate DLM by Pole method, for Paleo data
    # SRC 2020-01-20 and Modified for Squeal Experiments
    # (c) Stephen R. Carpenter
    
    # ONLINE DYNAMIC LINEAR MODEL (DLM) ESTIMATION
    DLM <- function(delta, n.gamma, d.gamma, mvec, Cpar, Yvec, Fmat) {
      
      # Online algorithm for Dynamic linear regression 
      # Copyright 2016 by Stephen R. Carpenter
      
      # Description and definitions:
      
      # Observation equation is
      # Y_t = F_t'*theta_t + eta_t where
      # Y_t is the prediction
      # F_t is a vector of predictors at the beginning of the time step
      # theta_t is the parameter vector
      # eta_t is an individual observation error
      
      # System equation is:
      # theta_t = theta_t-1 + omega_t
      # where theta is defined above and omega_t is an individual process error
      
      # Inputs to the function are:
      # delta, the discount factor
      # n.gamma, the initial number of observations (usually 1)
      # d.gamma, the initial shape parameter for prediction errors
      #  (prior estimate of prediction variance = d.gamma / n.gamma)
      # mvec, the initial guess of regression coefficients
      # Cpar, the initial guess of the covariance matrix of regression coefficients
      # Yvec, the vector of the observed response variate
      # Fmat, the matrix of predictors
      
      # Outputs are:
      # predix, the one-step-ahead predictions of the response variate
      # varpredix, the prediction variance at start of time step before error is measured
      # pars, the updated parameter estimates using the most recent prediction error
      # parvar, the variances of the parameters
      # Svec, the update (after error is measured within a time step) of varpredix
      
      # Updating follows the equations on p. 176-179 of Carpenter 2003,
      # Regime Shifts in Lake Ecosystems: Pattern and Variation
      
      # Determine constants
      npar <- length(mvec)
      Nobs <- length(Yvec)
      S0 <- d.gamma/n.gamma
      
      # Set up vectors to hold results
      predix <- rep(0,Nobs)
      varpredix <- rep(0,Nobs)
      Svec = rep(0,Nobs)
      pars <- matrix(0, nrow = Nobs, ncol = npar)
      parvar = matrix(0, nrow = Nobs, ncol = npar)
      
      for(i in 1:Nobs)  {  #Start DLM loop
        # Generate predictions
        Fvec <- Fmat[i,] # vector of predictors
        predix[i] <- sum(Fvec*mvec)
        # Compute error and update estimates
        error <- Yvec[i]-predix[i]
        Rmat <- Cpar/delta
        varpredix[i] <- (t(Fvec) %*% Rmat %*% Fvec) + S0
        n.gamma <- (delta*n.gamma)+1
        d.gamma <- (delta*d.gamma)+(S0*error*error/varpredix[i])
        S1 <- d.gamma/n.gamma
        Svec[i] = S1  # save updated variance
        Avec <- (Rmat %*% Fvec)/varpredix[i]
        mvec <- mvec + (Avec*error)
        pars[i,] <- mvec
        Cpar <- (S1/S0)*(Rmat - (Avec %*% t(Avec))*varpredix[i])
        # Disallow negative variances on the diagonal
        for(idiag in 1:npar) {
          Cpar[idiag,idiag] <- max(0,Cpar[idiag,idiag])
        }
        parvar[i,] = diag(Cpar)
        S0 <- S1 # roll over S
      } # End DLM loop
      
      DLM.out <- list(predix, varpredix, pars, parvar, Svec)
      return(DLM.out)
    } # END DLM FUNCTION
    
    # Main program -----------------------------------------------------------------
    
    # Load the dat
    # Make a matrix with the time series as columns
    #Xmat.0 = dat0[,4:6]  # keep log10 phycocyanin, Chl, delta DOsat, delta pH
    dat0 = daily_mean
    #dat0 = dat0 %>% mutate(chl_mean = log10(chl_mean))
    Xmat.0 = subset(dat0, select = c(chl_mean))
    Xmat.1 = as.matrix(Xmat.0)
    dimX = dim(Xmat.1)
    nX = dimX[1] # number of observations
    nV = dimX[2] # number of variates
    # make a time variable
    tvec = dat0$doy
    doy_dlm <- tvec[1:(length(tvec)-1)]
    
    
    # optional transformations
    ######################
    # log transform (optional)
    #Xmat.1 <- log(2+Xmat.1)  
    ######################
    #center (optional)
    unit = rep(1,nX) #nX = number of observations
    cmean = colMeans(Xmat.1)
    Xmat.2 = Xmat.1 - (unit%*%t(cmean))
    ######################
    # make z scores
    csd = apply(Xmat.1, 2, sd, na.rm=T)
    invcsd = 1/csd
    Xmat.3 = Xmat.1*(unit%*%t(invcsd)) 
    #####################
    
    ### What matrix will be analyzed?
    #Xmat = Xmat.3
    Xmat = as.matrix(cbind(unit, Xmat.1)) # Xmat.1 = untransformed, include intercept
    
    # set number of parameters (remember to add 1 if there is an intercept)
    npar = nV+1 #(nV is the number of variates before "unit" was added to the matrix)
    
    # Quick parameter estimate using LS for the inputs to the DLM program
    print('Multivariate least squares regression to check the data',quote=F)
    Xmat1 = Xmat[2:nX,]
    Xmat0 = Xmat[1:(nX-1),]
    Xinv = solve(t(Xmat0)%*%Xmat0)
    bmat = Xinv%*%t(Xmat0)%*%Xmat1
    print(bmat)
    
    beig = eigen(bmat,only.values = TRUE)
    print(beig$values)
    
    print('prediction error variances',quote=F)
    yhat = Xmat0%*%bmat
    err = Xmat1 - yhat
    verr = apply(err,2,var)
    print(verr)
    print('parameter covariance matrices',quote=F)
    print('each of the regressions has the same t(x)%*%x but unique error variance',quote=F)
    parcov = array(data=0,dim=c(npar,npar,npar))
    for(i in 1:npar) {
      parcov[i,,] = Xinv*verr[i]
      #print(parcov[i,,])
    }
    
    # Set up for multivariate DLM --------------------------------------------------------------
    
    # Additional inputs
    delta = rep(delta.input, npar) # vector of 1 value per parameter, e.g. c(0.9,0.9,0.9)
    n.gamma = rep(1, npar)
    d.gamma = verr
    
    # Output arrays to hold results
    par.dlm = array(data = 0, dim = c((nX-1), npar, npar))
    
    # Run the npar DLMs and save results
    yhatmat = matrix(0, nr = (nX-1), nc = npar)
    for(idlm in 1:npar) {
      MAR.est = DLM(delta[idlm],
                    n.gamma[idlm],
                    d.gamma[idlm],
                    bmat[,idlm],
                    parcov[idlm,,],
                    Xmat1[,idlm],
                    Xmat0)
      par.dlm[,,idlm] = MAR.est[[3]]
      # Plot predix and obs
      yhat = MAR.est[[1]]
      yhatmat[,idlm] = yhat
      if(idlm > 1) {
        print(c('Corr of prediction and observation for column ',idlm),quote=F)
        print(cor(yhat,Xmat1[,idlm]),quote=F)
      }
      # windows()
      # par(mfrow=c(1,1),mar=c(4, 4.3, 2, 2) + 0.1, cex.axis=1.6,cex.lab=1.6)
      # plot(tvec[2:nX,],yhat,type='l',lwd=3,col='deepskyblue',xlab='DOY',ylab='Y and Yhat',
      # main=bquote('Predix & Obs for column'~ .(idlm)) )
      # points(tvec[2:nX,],Xmat1[,idlm],type='p',pch=19,col='darkblue')
    }
    
    
    #==========
    chl_pred = yhatmat[,2]
    chl_obs = Xmat1[,2]
    chl_fit = data.frame(cbind(chl_pred, chl_obs)) %>%
      mutate(doy = doy_dlm) # this doy seq is assuming the first day is dropped
    
    chl_fit_plot = 
      ggplot(chl_fit, aes(x = doy, y = chl_pred)) + 
      geom_line(color = "#479A63", size = 2, alpha = 0.5) +
      geom_point(data = chl_fit, aes(x = doy, y = chl_obs), color = '#117733', size = 2) + 
      theme_bw() + xlab("") + ylab("") + ggtitle("Chlorophyll predicted & observed") +
      annotate(geom = 'text', x = 140, y = 0.5, 
               label = round(cor(chl_fit$chl_pred, chl_fit$chl_obs), 3))
    
    ggarrange(chl_fit_plot)
    
    
    
    # Construct the b matrices at each time and compute eigenvalues
    eigvals = rep(0,(nX-1)) # vector to hold maximum eigenvalues
    weights1 = matrix(0, nr = (nX-1), nc = (npar-1)) # subtract 1 column due to removing intercept
    for(it in 1:(nX-1)) {
      b.it = par.dlm[it,,]
      b.noint = b.it[2:npar,2:npar] # remove intercept
      lam = eigen(b.noint,only.values=F)
      lam.max = max(Mod(lam$values))
      imax = which.max(Mod(lam$values))
      eigvals[it] = sign(Re(lam$values[imax]))*Re(lam.max)
      weights1[it,] = lam$vectors[,1]
    }
    
    
    eigenvalues = data.frame(eigvals) %>%
      mutate(doy = daily_mean$doy[1:length(daily_mean$doy)-1]) %>% 
      mutate(Year = year) %>% 
      mutate(delta = delta.input) %>% 
      mutate(model.cor = cor(yhat,Xmat1[,idlm]))
    
    ggplot(eigenvalues, aes(x = doy, y = eigvals)) + 
      geom_point(color = '#1F78B4', size = 2) +
      geom_path(color = '#1F78B4', size = 1.5, alpha = 0.5) + 
      theme_bw() +
      ggtitle(paste("Paul Lake ", year, " Multivariate DLM", sep = "")) +
      geom_hline(yintercept = 1) + # when eigvals cross 1 from below = evidence of a critical transition
      geom_vline(xintercept = 162, linetype = "dashed") + #date that nutrient additions began
      xlab("") + ylab("Eigenvalues")
    
    
    
    if(k == 1 & j == 1){
      all.results = eigenvalues
    }
    if(k > 1 | j > 1){
      all.results = rbind(all.results, eigenvalues)
    }
    
    
  }
  
  
  # 
  # ggplot(all.results, aes(x = doy, y = eigvals, color = as.factor(Year)))+
  #   geom_line(size = 1)+
  #   #geom_point()+
  #   geom_hline(yintercept = 1)+
  #   facet_wrap(~Year, nrow = 5, ncol = 1)+
  #   theme_bw()
  
  
  
  
}


write.csv(all.results, Fname)


ggplot(all.results %>% filter(delta == 0.94), aes(x = as.numeric(doy), y = eigvals, color = as.factor(Year)))+
  geom_line(size = 1)+
  #geom_point()+
  geom_hline(yintercept = 1)+
  facet_wrap(~Year, nrow = 5, ncol = 1)+
  theme_bw()

ggplot(all.results, aes(x = delta, y = model.cor, color = as.factor(Year)))+
  geom_line(size = 1)+
  #geom_point()+
  geom_hline(yintercept = 1)+
  facet_wrap(~Year, nrow = 5, ncol = 1)+
  theme_bw()

#write.csv(all.results, "./scripts/Multivariate DLM/eigenvalues 2026-05-21 CHL ONLY.csv", row.names = FALSE)


ggplot(all.results, aes(x = delta, y = model.cor, color = as.factor(Year)))+
  geom_point()+
  geom_line()
