# bootstrap Direct Counts of Passage times From bootstraps of DLM and DDJ
# It can be run anytime after Step 11, the DDJ bootstrap
# SRC 2026-02-07

rm(list = ls())
graphics.off()

library(stats)
library(tictoc)
library(ggrepel)
library(ggpmisc)
library(parallel)

options(mc.cores = parallel::detectCores())

# output file name
Fname = './results/bootstrapped results/Passage_times_boot_Paul 1000.Rdata'

# Load data
# bdat0 is the bootstrapped DLM output, bdat is Markov-thinned bdat0 by aropt
# EPxeqmat is the bootstrapped equilibria of the EPF

load(file='./results/bootstrapped results/DDJ_boot_Paul 1000.Rdata')
# choose time step ***************************************
#DT = aropt/(24*12)  # time step of 1-minute data in days
DT = 5*aropt    # time step of 5 minute data in minutes. If aropt = 2, DT = 10 minutes
# *******************************************************

# *******************************************************
## thin bdat to match previous analyses ##
idx <- seq(1, length(bdat0), by = aropt)
bdat = bdat0[seq(1, nrow(bdat0), by = aropt), ]
# *******************************************************

print('',quote=F)
print(c('Nboot ',Nboot),quote=F)
print(c('dim of time series ',dim(bdat)),quote=F)
NT = dim(bdat)[1]  # number of time steps
print(c('dim of equilibria of EPF ',dim(EPxeqmat)),quote=F)

probvec = c(0.1,0.25,0.5,0.75,0.9)  # quantiles for each cycle
# PTleft = matrix(0,nr=Nboot,nc=length(probvec))  # left passage times, percentiles 
# PTright = matrix(0,nr=Nboot,nc=length(probvec))  # right passage times, percentiles  
# #
# # find passage times in each bootstrap cycle
# for(i in 1:Nboot)  {
#   x0 = bdat[,(1+i)]  # first column of bdat is decimal doy
#   thresh = EPxeqmat[2,i]  # unstable equilibrium
#   #
#   dev = x0 - thresh # deviation from threshold
#   sdev = sign(dev)
#   dsx = c(0,diff(sdev))
#   Tmax = length(dsx)
#   Tcount = c(1:Tmax)
#   tup = Tcount[which(dsx > 0)]  # jumps upward across threshold
#   tdn = Tcount[which(dsx < 0)]  # jumps downward across threshold
#   ETl = tup  # placeholder
#   ETr = tdn  # placeholder
#   # if the first jump was up:
#   if(tup[1] < tdn[1]) {
#     ETr = tdn - tup[1:length(tdn)]  # exit times from right basin
#     ETl = tup - c(0,tdn)  # exit times from left basin
#   }
#   # if the first jump was down
#   if(tup[1] > tdn[1]) {
#     ETr = tdn - c(0,tup[1:(length(tdn)-1)])
#     ETl = tup - tdn[1:length(tup)]
#   }
#   # Convert to time unit, DT
#   ETr = ETr*DT
#   ETl = ETl*DT
#   # compute and save quantiles
#   qleft = quantile(ETl,probs=probvec,na.rm=T,names=F)
#   PTleft[i,] = qleft
#   qright = quantile(ETr,probs=probvec,na.rm=T,names=F)
#   PTright[i,] = qright
# }
# 


#### calculate passage times from each using tidyverse ######



# 
# basin = ifelse(x0 < xeq2[2], "left", "right")
# change = c(TRUE, basin[-1] != basin[-length(basin)]) # compare each value to the value before it
# grp = cumsum(change)
# 
# ET = aggregate(rep(1, length(basin)), by = list(grp, basin), FUN = length)
# names(ET) = c("event", "basin", "steps")
# ET$minutes = ET$steps * 10 # 10 minutes
# 
# ET_Tues_l = ET$minutes[ET$basin == "left"]
# ET_Tues_r = ET$minutes[ET$basin == "right"]
# 
# mean(ET_Tues_l)
# mean(ET_Tues_r)
# 
# ET$year = keepyear
# 





# initialize output matrices
PTleft  = matrix(0, nrow=Nboot, ncol=length(probvec))
PTright = matrix(0, nrow=Nboot, ncol=length(probvec))


# loop over bootstrap realizations
for(i in 1:Nboot) {
  
  ## break up by year
  for(j in c(2013, 2014, 2015, 2024, 2025)){
  
  bdat.cur  = bdat[floor(bdat[,1]) == j, ]
  
  x0    = bdat.cur[, i+1]        # first column is time, next columns are bootstraps
  thresh = EPxeqmat[2, i]    # unstable equilibrium
  
  # classify each step into a basin
  basin = ifelse(x0 < thresh, "left", "right")
  
  # run-length encoding to find continuous stretches in each basin
  r = rle(basin)
  ET = r$lengths * DT         # duration of each stretch in time units
  basins = r$values            # basin corresponding to each stretch
  
  # separate left/right
  ET_left  = ET[basins == "left"]
  ET_right = ET[basins == "right"]
  
 # print(ET_left)
  
  # compute quantiles
  PTleft[i, ]  = quantile(ET_left,  probs=probvec, na.rm=TRUE, names=FALSE)
  PTright[i, ] = quantile(ET_right, probs=probvec, na.rm=TRUE, names=FALSE)
  
  cur.pass.boot = data.frame(year = j, Nboot = i, mean.left = mean(ET_left, na.rm = TRUE), mean.right = mean(ET_right, na.rm = TRUE))
  
  # save the results to a dataframe
  if(i == 1 & j == 2013){
    pass.boot = cur.pass.boot
  }
  if( i > 1 | j!= 2013){
    pass.boot = rbind(pass.boot, cur.pass.boot)
  }
  
  }
  
  
}



ggplot(pass.boot, aes(x =as.factor(year), y = mean.right/60))+
  geom_boxplot()+
  scale_y_log10()


### save the dataframe
save(pass.boot, file = Fname)

# read in mean kNC

data = read.csv("./data/formatted data/simulation model inputs 2013-2015 2024 2025 v4.csv")

data %>% 
  group_by(Year) %>% 
  summarize(mean(grav.m2))

data.mean = data %>% 
  mutate(kNC = kPAR - 0.0177*Manual_Chl) %>% 
  filter(Lake == "L" & kNC > 0) %>% 
  group_by(Year) %>% 
  summarize(mean.kNC = mean(kNC, na.rm = TRUE),
            median.kNC = median(kNC, na.rm = TRUE),
            total.nuts = max(cumulative.load, na.rm = TRUE),
            mean.kPAR = mean(kPAR, na.rm = TRUE))  %>% 
  rename(year = Year)


pass.boot = pass.boot %>% 
  left_join(data.mean, by = "year")

mean.pass.boot = pass.boot %>% 
  group_by(year) %>% 
  summarize(mean.left = mean(mean.left, na.rm = TRUE), mean.right = mean(mean.right, na.rm = TRUE)) %>% 
  left_join(data.mean, by = "year")

ggplot(mean.pass.boot, aes(x = mean.kNC, y = log10(mean.right)))+
  geom_point()

ggplot(mean.pass.boot, aes(x = mean.kNC, y = (mean.left/(24*60))))+
  scale_y_log10()+
  geom_point(size = 3.8) +
  geom_text_repel(aes(label = year), color = "black", size = 3,
                  show.legend = FALSE, max.overlaps = Inf,
                  box.padding = 0.6, point.padding = 0.5,
                  force = 2, min.segment.length = 0, segment.color = NA)+
  labs(x = "mean non-chl light attenuation (kNC)",
       y = "mean passage time (days)") +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed")+
  stat_poly_eq(aes(label = paste(..rr.label..)),
               formula = y ~ x, parse = TRUE,
               label.x = "left", label.y = "top")
  
ggplot(mean.pass.boot, aes(x = mean.kNC, y = (mean.right/(24*60))))+
  scale_y_log10()+
  geom_point(size = 3.8) +
  geom_text_repel(aes(label = year), color = "black", size = 3,
                  show.legend = FALSE, max.overlaps = Inf,
                  box.padding = 0.6, point.padding = 0.5,
                  force = 2, min.segment.length = 0, segment.color = NA)+
  labs(x = "mean non-chl light attenuation (kNC)",
       y = "mean passage time (days)") +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed")+
  stat_poly_eq(aes(label = paste(..rr.label..)),
               formula = y ~ x, parse = TRUE,
               label.x = "right", label.y = "top")

print('mean & sd of Passage Time quantiles over all bootstrap cycles', quote=F)
print(c('quantile probabilities:',probvec),quote=F)
print('left, mean and sd for each quantile',quote=F)
print(colMeans(PTleft))
print(apply(PTleft,2,sd))
print('right, mean and sd for each quantile',quote=F)
print(colMeans(PTright))
print(apply(PTright,2,sd))









#### make figures using bootstrapped passage times ####

ggplot(pass.boot, aes(x = mean.left, ))

green_palette <- c("#CBD4AC", "#b4c187", "#80914b", "#5a6b3a", "#496231")
brown_palette <- c("#CFB491", "#9c7744", "#8c5c2b", "#533113", "#361c07")

pt.right.density =  ggplot(
  pass.boot,
  aes(x = (mean.right)/(24*60), y = factor(year), fill = factor(year))
) +
  geom_density_ridges(scale = 1.2, alpha = 0.8, color = "white") +
  scale_fill_manual(values = (green_palette)) +
 # scale_x_log10(breaks = c(0.1, 1, 10)) +
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6))+
  labs(
    x = "passage time (days)",
    y = "Year",
    title = "blooom state"
  ) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    legend.position = "none"
  )



pt.left.density =  ggplot(
  pass.boot,
  aes(x = (mean.left)/(24*60), y = factor(year), fill = factor(year))
) +
  geom_density_ridges(scale = 1.2, alpha = 0.8, color = "white") +
  scale_fill_manual(values = rev(brown_palette)) +
 # scale_x_log10(breaks = c(0.5, 1, 2, 3, 4, 5)) +
  labs(
    x = "passage time (days)",
    y = "Year",
    title = "clearwater state"
  ) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    legend.position = "none"
  )

ggarrange(pt.left.density, pt.right.density)



lb.plot = ggplot(pass.boot, aes(x = as.factor(year), y = (mean.left)/(24*60), fill = factor(year))) +
  geom_boxplot(alpha = 0.8)+
  labs(y = "bootstrapped passage time (days)", x = "Year", title = "clearwater state") +
  theme(legend.position = "none", axis.text = element_text(size = 14), axis.title = element_text(size = 16))+
  scale_fill_manual(values = rev(brown_palette))+
  theme_classic()+
  scale_y_log10(limits = c(0.1, 10))+
  theme(legend.position = "none")+
  theme(
    axis.text.y  = element_text(size = 12),
    axis.text.x = element_text(size = 10),
    axis.title = element_text(size = 12),
    strip.text = element_text(size = 12),
    legend.position = "none"
  ) 




rb.plot = ggplot(pass.boot, aes(x = as.factor(year), y = (mean.right)/(24*60), fill = factor(year))) +
  geom_boxplot(alpha = 0.8)+
  labs(y = "", x = "Year", title = "bloom state") +
  theme(legend.position = "none", axis.text = element_text(size = 14), axis.title = element_text(size = 16))+
  scale_fill_manual(values = (green_palette))+
  theme_classic()+
  scale_y_log10(limits = c(0.1, 10))+
  theme(legend.position = "none")+
  theme(
    axis.text.y  = element_text(size = 12),
    axis.text.x = element_text(size = 10),
    axis.title = element_text(size = 12),
    strip.text = element_text(size = 12),
    legend.position = "none"
  ) 

ggarrange(lb.plot, rb.plot)



#### compare to kNC ####


knc.lb = ggplot(mean.pass.boot, aes(x = mean.kNC, y = (mean.left/(24*60))))+
  scale_y_log10()+
  theme_bw()+
  geom_point(size = 3.8, color = "#533113") +
  geom_text_repel(aes(label = year), color = "black", size = 3,
                  show.legend = FALSE, max.overlaps = Inf,
                  box.padding = 0.6, point.padding = 0.5,
                  force = 2, min.segment.length = 0, segment.color = NA)+
  labs(x = "mean non-chl light attenuation (kNC)",
       y = "mean bootstrapped passage time (days)") +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "#533113")+
  stat_poly_eq(aes(label = paste(..rr.label..)),
               formula = y ~ x, parse = TRUE,
               label.x = "left", label.y = "top")+
  theme(axis.title = element_text(size = 12))

knc.rb = ggplot(mean.pass.boot, aes(x = mean.kNC, y = (mean.right/(24*60))))+
  scale_y_log10()+
  theme_bw()+
  geom_point(size = 3.8, color = "#5a6b3a") +
  geom_text_repel(aes(label = year), color = "black", size = 3,
                  show.legend = FALSE, max.overlaps = Inf,
                  box.padding = 0.6, point.padding = 0.5,
                  force = 2, min.segment.length = 0, segment.color = NA)+
  labs(x = "mean non-chl light attenuation (kNC)",
       y = "") +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "#5a6b3a")+
  stat_poly_eq(aes(label = paste(..rr.label..)),
               formula = y ~ x, parse = TRUE,
               label.x = "right", label.y = "top")+
  theme(axis.title = element_text(size = 12))


ggarrange(lb.plot, rb.plot, knc.lb, knc.rb)





geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +
  geom_point(size = 3.8) +
  geom_text_repel(aes(label = Year), color = "black", size = 3,
                  show.legend = FALSE, max.overlaps = Inf,
                  box.padding = 0.6, point.padding = 0.5,
                  force = 2, min.segment.length = 0, segment.color = NA) +
  facet_wrap(~basin,
             labeller = as_labeller(c(left = "clearwater state",
                                      right = "bloom state"))) +
  scale_color_manual(values = c(left = "#533113",
                                right = "#5a6b3a")) +
  labs(x = "mean non-chl light attenuation (kNC)",
       y = "mean passage time (days)") +
  stat_poly_eq(aes(label = paste(..rr.label..)),
               formula = y ~ x, parse = TRUE,
               label.x = "left", label.y = "top", color = "black") +
  theme_bw() +
  scale_y_log10(limits = c(0.2, 10))+
  theme(legend.position = "none",
        strip.text = element_text(size = 12))+
  theme(
    axis.text  = element_text(size = 12),
    axis.title = element_text(size = 12),
    strip.text = element_text(size = 12),
    legend.position = "none"
  ) 
