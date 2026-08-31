## function to calculate thermocline using SRC's code

# requires a dataframe with lake, date, year, day of year, and water temp columns called
# wtr_#.# where #.# is the depth the temperature was measured

# the lake and year arguments select the lake and year we are interested in 

# the columns must be in order from lowest depth (0 m) to highest depth

# this calculates three types of thermocline -- the shallowest depth where the
# temperature drops 2 degrees in one meter, the deepest depth where the temp
# drops 2 degrees in one meter, and the depth with the deepest temperature slope.

# Steve tried this with cubic fitting but it created ghost thermoclines, instead
# it identifies when the slope (the first derivative) crosses the horizontal -2 line
# based on a linear fit between points.

RootSpline1.DKS = function (x, y, y0, verbose = TRUE) {
  if (is.unsorted(x)) {
    ind <- order(x)
    x <- x[ind]; y <- y[ind]
  }
  z <- y - y0
  ## which piecewise linear segment crosses zero?
  k <- which(z[-1] * z[-length(z)] <= 0)
  ## analytical root finding
  xr <- x[k] - z[k] * (x[k + 1] - x[k]) / (z[k + 1] - z[k])
  ## make a plot?
  if (verbose) {
    #windows()
    # par(mar = c(4, 4, 2, 1))
    # plot(x, y, "l"); abline(h = y0, lty = 2)
    # points(xr, rep.int(y0, length(xr)))
    # 
    # test = data.frame(x, y)
    # 
    #   ggplot(test, aes(x = x, y = y))+
    #   geom_line()+
    #   geom_hline(yintercept = y0)
  
  #print(str(test))
  }
  ## return roots
  return(unname(xr))
}



if (!require(npreg, quietly = TRUE)) install.packages('npreg')
suppressWarnings(library(npreg))

if (!require(stats, quietly = TRUE)) install.packages('stats')
suppressWarnings(library(stats))

if (!require(tidyverse, quietly = TRUE)) install.packages('tidyverse')
suppressWarnings(library(tidyverse))

# 
# # test data
# #test.data = get(load("G:/My Drive/Projects and Papers/PhD/Aquashade-GHG/data/formatted data/routines temp/2018 2019 2024 routines temp.RData"))
# test.data = prof.temp.wide %>% select(lake, year, doy, date, wtr_0, wtr_1, wtr_2, wtr_3,
#                                      wtr_4, wtr_5, wtr_6, wtr_7, wtr_8)
# 
# #test.data = prof.temp.wide
# 
# test.lake = "T"
# test.year = 2024
# 
# temp.data = test.data
# targ.lake = test.lake
# targ.year = test.year


SRC_thermocline = function(temp.data, targ.lake, targ.year){

  
  if (!require(npreg, quietly = TRUE)) install.packages('npreg')
  suppressWarnings(library(npreg))
  
  if (!require(stats, quietly = TRUE)) install.packages('stats')
  suppressWarnings(library(stats))
  
  if (!require(tidyverse, quietly = TRUE)) install.packages('tidyverse')
  suppressWarnings(library(tidyverse))
  
  
  # format the data so it matches Steve's code (create RoundDoY and DoY columns)
  # filter to just be the lake and year we're interested in, then remove lake and year columns
  temp.data = temp.data %>%
    mutate(DoY = decimal_date(date), RoundDoY = yday(date)) %>% 
    filter(lake == targ.lake, year == targ.year) %>% 
    select(-lake, -year)
  
  # remove any columns that are all NA values
  temp.data = temp.data %>% select(where(~ !all(is.na(.))))
  
  # get the measurement depths
  Zchain = temp.data %>% select(starts_with("wtr_")) %>% 
    names(.)
  
  # make the depths numbers and arrange in order
  Zchain = substr(Zchain, 5, nchar(Zchain))
  Zchain = as.numeric(Zchain)
  
  # sort so the depths are in order
  Zchain = sort(Zchain)
  
  # select day range
  doy0 = min(temp.data$RoundDoY) # 1st day of season
  doyN = max(temp.data$RoundDoY) # end of season
  doyvec = sort(unique(temp.data$RoundDoY))
  ndoy = length(doyvec)
  
  # Select time window within each day
  # want from 12 to 4
  #tchain.data = tchain.data %>% filter(hour >= 12 & hour <= 16)
  
  # vectors for results
  ZT1a = rep(0,ndoy)
  ZT1b = rep(0,ndoy)
  ZT2 = rep(0,ndoy)
  id = 1
  
  error.days = c()
  
  
  id = 1
  for(id in 1:ndoy)  {   # LOOP OVER id = DOY counter ****************************
    
    # which doy is the current of the loop iteration
    today = doyvec[id]
    
    Tc = temp.data %>% filter(RoundDoY == today)
    
    # use profiles only, not other columns
    Tprof = Tc %>% select(starts_with("wtr_"))
    ncol.Tprof = ncol(Tprof)
    Tprof = colMeans(Tprof)
    
    
    # linear first derivative, use first point for surface
    d1zchain = (Zchain[1:(ncol.Tprof-1)] + Zchain[2:(ncol.Tprof)])/2  # get the midpoints
    dT = diff(Tprof)/diff(Zchain) # this is updated from Steve's version so it works with any depth intervals
                                  # what I changed was dividing by the change in depth to get true dT/dZ
    
    # Thermocline by -2 degrees/m rule
    #splinefun(x = d1zchain, y = dT, method = "fmm") # could try something like this if we want cubic roots
    
    #options(device = "RStudioGD")
    
    
    twotest = RootSpline1.DKS(x=d1zchain,y=dT,y0=-2.0,verbose=TRUE) 
    if(length(twotest) == 1) {
      ZT1a[id] = twotest
      ZT1b[id] = twotest
    }
    if(length(twotest) > 1) {
      ZT1a[id] = twotest[1]
      ZT1b[id] = twotest[length(twotest)]
    }
    
    print(length(twotest))
    print(twotest)
     test= data.frame(dT = dT, d1zchain = d1zchain)

    # print(ggplot(test, aes(x = d1zchain, y = dT))+
    #    geom_point()+
    #    geom_line()+
    #    geom_hline(yintercept = -2))
    
    # linear second derivative, most negative at steepest slope
    d2T = diff(dT)
    d2zchain = (d1zchain[1:(ncol.Tprof-2)] + d1zchain[2:(ncol.Tprof-1)])/2
    
    
    imin = which.min(dT)   # most negative slope
   # ZT2  = d1zchain[imin]
    
    print(d2T)
    print(d2zchain)
    
    # find index of minima
    #imin = which.min(d2T)
    
    
    tryCatch({
      # This is the line that might produce an error
     # ZT2[id] <- d2zchain[imin]
      ZT2[id] <- d1zchain[imin]
    }, error = function(e) {
      # Print a message indicating which id caused the error
      message(paste("Error with id", id, ": ", e$message))
      
      error.days <<- c(error.days, id)
      
    })
    
  }  # end loop over DOY *************************************************************
  
  
  # make a dataframe of the values from Steve
  thermos = data.frame(doy = doyvec, ZT1a = ZT1a, ZT1b = ZT1b, ZT2 = ZT2)
  
  thermos = thermos %>% pivot_longer(cols = c("ZT1a", "ZT1b", "ZT2"), names_to = "thermocline", values_to = "depth")
  
  # convert error days to doy
  error.days = error.days + doy0-1
  
  thermos = thermos %>% filter(!doy %in% error.days)
  
  # replace all values where the thermocline was not calculated with NA
  
  # zt1a = "shallow -2 line"
  # zt1b = "deep or only -2 line"
  # zt2 = "steepest slope"
  
  
  print( ggplot(thermos, aes(x = doy, y = depth, color = thermocline))+
           geom_point()+
           geom_line()+
           theme_classic()+
           scale_y_reverse()+
           #xlim(136, max(thermos$doy))+
           ylim(7, 0)+
           labs(title = paste0(targ.lake, " ", targ.year, " thermocline ADJUSTMENT 0.5 m"), x = "Day of 2018", y = "depth (m)")+
           scale_colour_manual(labels = c("ZT1a" = "ZT1a shallow -2 line", "ZT1b" = "ZT1b deep or only -2 line", "ZT2" = "ZT2 steepest slope"),
                               values = c("steelblue1", "darkblue", "red")) +   
           theme(legend.position="right"))
  
  
  thermos = thermos %>% mutate(lake = targ.lake, year = targ.year)  
  
  return(thermos)
  
  
  
  
}


# SRC_thermocline(test.data, test.lake, test.year)
