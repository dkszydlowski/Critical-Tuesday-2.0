#### updated version to calculate thermocline from the routines data

library(tidyverse)
library(readxl)

## read in the routines data 

# Package ID: knb-lter-ntl.352.4 Cataloging System:https://pasta.edirepository.org.
# Data set title: Cascade Project at North Temperate Lakes LTER Core Data Physical and Chemical Limnology 1984 - 2016.
# Data set creator:  Stephen Carpenter - University of Wisconsin 
# Data set creator:  Jim Kitchell - University of Wisconsin 
# Data set creator:  Jon Cole - Cary Institute of Ecosystem Studies 
# Data set creator:  Mike Pace - University of Virginia 
# Contact:  Stephen Carpenter -  University of Wisconsin  - steve.carpenter@wisc.edu
# Contact:  Mike Pace -  University of Virginia  - pacem@virginia.edu
# Contact:    -  NTL LTER  - ntl.infomgr@gmail.com
# Stylesheet v2.16 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu      

options(HTTPUserAgent="EDI_CodeGen")

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/352/4/3f928dbd3989c95bc7146ee8363d69bd" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt1 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "lakeid",     
                 "lakename",     
                 "year4",     
                 "daynum",     
                 "sampledate",     
                 "depth",     
                 "temperature_C",     
                 "dissolvedOxygen",     
                 "irradianceWater",     
                 "irradianceDeck",     
                 "comments"    ), check.names=TRUE)

unlink(infile1)

routines.edi = dt1

routines.edi = dt1 %>% filter(year4 %in% c(2013:2015))




# read in a function that calculates thermoclines using Steve's methods
source("./scripts/SRC_thermocline_function.R")

# requires a dataframe with lake, date, year, day of year, and water temp columns called
# wtr_#.# where #.# is the depth the temperature was measured
routines.edi = routines.edi %>% 
  rename(lake = lakeid, year = year4, doy = daynum, date = sampledate) %>% 
  select(lake, date, year, doy, depth, temperature_C) %>% 
  filter(depth %in% seq(0, 8, 0.5))

routines.wide = routines.edi %>% 
  pivot_wider(names_prefix = "wtr_", names_from = "depth", values_from = c(temperature_C)) %>% 
  mutate(date = ymd(date))

lakes = c("L", "R", "T")
years = c(2013:2015)

for(i in 1:length(years)){
  
  cur.year = years[i]
  for(j in 1:length(lakes)){
    cur.lake = lakes[j]
    
    thermo = SRC_thermocline(routines.wide, cur.lake, cur.year)
    

    
    if(i ==1 & j == 1){
      all.thermo = thermo
    }
    if(i != 1  | j != 1){
      all.thermo = rbind(all.thermo, thermo)
    }
    
    
  }
  
  

  
}


ggplot(all.thermo, aes(x = doy, y= depth, color = thermocline))+
  geom_point()+
  geom_line()+
  scale_y_reverse()+
  facet_wrap(lake~year)


### do the same thing with the 2024 and 2025 routines

summary.thermo = all.thermo %>% 
  filter(thermocline == "ZT2")  %>% 
  group_by(lake, year, thermocline) %>% 
  summarize(mean.thermo = mean(depth, na.rm = TRUE))




ggplot(all.thermo %>% filter(thermocline == "ZT2"), aes(x = as.factor(year), y = depth))+
  geom_boxplot()+
  facet_wrap(~lake)



















### play with one day of thermocline

example.data = routines.wide %>% filter(year == 2015 & lake == "T" & doy == 203)

Zchain = example.data %>% select(starts_with("wtr_")) %>% 
  names(.)

# make the depths numbers and arrange in order
Zchain = substr(Zchain, 5, nchar(Zchain))
Zchain = as.numeric(Zchain)

# sort so the depths are in order
Zchain = sort(Zchain)

Tprof = example.data %>% select(starts_with("wtr_"))
ncol.Tprof = ncol(Tprof)
Tprof = colMeans(Tprof)

# linear first derivative, use first point for surface
d1zchain = (Zchain[1:(ncol.Tprof-1)] + Zchain[2:(ncol.Tprof)])/2  # get the midpoints
dT = diff(Tprof) # this is updated from Steve's version so it works with any depth intervals

d1zchain[which.min(dT)]

source("./scripts/SRC_thermocline_function.R")
thermo.test = SRC_thermocline(example.data, "T", 2015)


### merge dataframes

# Convert named vectors to numeric with corresponding depths
dT_df <- data.frame(depth = as.numeric(sub("wtr_", "", names(dT))), dT = dT)
Tprof_df <- data.frame(depth = as.numeric(sub("wtr_", "", names(Tprof))), Tprof = Tprof)

combined_df <- full_join(dT_df, Tprof_df, by = "depth") %>%
  arrange(depth)

# Add the chains
combined_df$d1zchain <- c(d1zchain, rep(NA, nrow(combined_df)-length(d1zchain)))
combined_df$Zchain <- Zchain  # same length as Tprof, works

combined_df

ggplot(combined_df, aes(x = dT, y = depth))+
  geom_path()+
  scale_y_reverse()


ggplot(combined_df, aes(x = dT, y = d1zchain))+
  geom_path(size = 1)+
  scale_y_reverse()+
  scale_x_reverse()+
  geom_vline(xintercept = -2, linetype = "dashed")+
  theme_bw()



ggplot(combined_df, aes(x = Tprof, y = d1zchain))+
  geom_path(size = 1)+
  scale_y_reverse()+
  scale_x_reverse()+
  geom_vline(xintercept = -2, linetype = "dashed")+
  theme_bw()



plot_df <- combined_df %>%
  select(d1zchain, dT, Tprof) %>%
  pivot_longer(cols = c(dT, Tprof), names_to = "variable", values_to = "value")


ggplot(plot_df, aes(x = value, y = d1zchain, color = variable)) +
  geom_path(size = 1) +
  scale_y_reverse() +
  scale_x_reverse() +
  geom_vline(xintercept = -2, linetype = "dashed") +
  geom_hline(
    data = thermo.test,
    aes(yintercept = depth, linetype = thermocline),
    color = "black",
    linewidth = 1
  ) +
  geom_hline(yintercept = 2.25)+
  theme_bw() +
  labs(x = "Temperature / ΔT", y = "Depth (m)", color = "") +
  scale_color_manual(values = c("dT" = "blue", "Tprof" = "red"))







#### TRY rLAKEANALYZER ####

library(rLakeAnalyzer)

lakes = c("L", "R", "T")
years = c(2013:2015)

for(i in 1:length(lakes)){
  
  cur.lake = lakes[i]
  
  for(j in 1:length(years)){
    
    cur.year = years[j]
    cur.data = routines.wide %>% filter(lake == cur.lake & year == cur.year) %>% 
      select(date, names(routines.wide)[grepl("wtr", names(routines.wide))])
    
    thermos = ts.thermo.depth(cur.data, na.rm = TRUE, seasonal = TRUE) %>% 
      mutate(source = "routines", lake = cur.lake, year = cur.year)
    
    if(i == 1 & j == 1){
    all.thermo.rlake = thermos
    }
    if(i > 1 | j > 1){
      all.thermo.rlake = rbind(all.thermo.rlake, thermos)
    }
      
  }
}

all.thermo.rlake = all.thermo.rlake %>% 
  mutate(doy = yday(date))

ggplot(all.thermo.rlake, aes(x = doy, y = thermo.depth))+
  geom_point()+
  facet_wrap(lake~year)+
  scale_y_reverse()+
  ylim(6, 0)+
  geom_line()


ggplot(all.thermo.rlake, aes(x = doy, y = thermo.depth))+
  geom_point()+
  facet_wrap(lake~year)+
  scale_y_reverse()+
  ylim(6, 0)+
  geom_line()

thermos = ts.thermo.depth(cur.data, na.rm = TRUE, seasonal = TRUE) %>% 
  mutate(source = "routines", lake = cur.lake, year = cur.year)





#### what were the original values? ####


















###### Just calculate the steepest slope #####

routines.wide = routines.edi %>% 
  pivot_wider(names_prefix = "wtr_", names_from = "depth", values_from = c(temperature_C)) %>% 
  mutate(date = ymd(date))


### add in the 2024 and 2025 data ###
temp25 = read_xlsx("R:/Cascade/Data/2025 Data and Daily Code/2025 Routines/2025 temp DO profiles.xlsx")

temp25 = temp25 %>% 
  rename(lake = Lake, date = Date, year = Year, depth = Depth, doy = DoY) %>% 
  mutate(date = ymd(date)) %>% 
  select(lake, date, year, depth, doy, Temp_C) %>% 
  filter(depth %in% seq(0, 8, 0.5)) %>% 
  mutate(Temp_C = as.numeric(Temp_C)) %>% 
  pivot_wider(names_prefix = "wtr_", names_from = "depth", values_from = c(Temp_C)) %>% 
  filter(doy > 153) # remove times when rope was flawed


temp24 = read_xlsx("R:/Cascade/Data/2024 Routines/2024 temp DO profiles.xlsx")  %>% 
  rename(lake = Lake, date = Date, year = Year, depth = Depth, doy = DoY) %>% 
  mutate(date = ymd(date)) %>% 
  select(lake, date, year, depth, doy, Temp_C) %>% 
  filter(depth %in% seq(0, 8, 0.5)) %>% 
  mutate(Temp_C = as.numeric(Temp_C)) %>% 
  pivot_wider(names_prefix = "wtr_", names_from = "depth", values_from = c(Temp_C))



all.routines = rbind(temp25, temp24, routines.wide)
  
lakes = c("L", "R", "T")
years = c(2013:2015, 2024, 2025)

for(i in 1:length(years)){
  
  cur.year = years[i]
  for(j in 1:length(lakes)){
    cur.lake = lakes[j]
    
    thermo = SRC_thermocline(all.routines, cur.lake, cur.year)
    
    
    
    if(i ==1 & j == 1){
      all.thermo = thermo
    }
    if(i != 1  | j != 1){
      all.thermo = rbind(all.thermo, thermo)
    }
    
    
  }
  
  
  
  
}


ggplot(all.thermo %>% filter(thermocline == "ZT2"), aes(x = doy, y = depth))+
  geom_point()+
  geom_line()+
  facet_grid(lake~year)+
  scale_y_reverse()+
  ylim(6, 0)


ggplot(all.thermo %>% filter(thermocline == "ZT2"), aes(x = as.factor(year), y = depth))+
  geom_boxplot()+
  facet_wrap(~lake)+
  scale_y_reverse()


ggplot(all.thermo %>% filter(thermocline == "ZT2"), aes(x = lake, y = depth))+
  geom_boxplot()+
  scale_y_reverse()


### remove a few outlier points
all.thermo = all.thermo %>% 
  mutate(depth = replace(depth, depth < 1 & thermocline == "ZT2", NA)) %>% 
  mutate(depth = replace(depth, depth < 2 & thermocline == "ZT2" & lake == "L" & year == 2025, NA))
  


ggplot(all.thermo %>% filter(thermocline == "ZT2"), aes(x = as.factor(year), y = depth))+
  geom_boxplot()+
  facet_wrap(~lake)+
  scale_y_reverse()


write.csv(all.thermo, "./data/formatted data/thermoclines 2013-2015 2024 2025 STEEPEST SLOPE ROUTINES.csv", row.names = FALSE)



all.thermo %>% 
  filter(lake == "T") %>% 
  group_by(year) %>% 
  summarize(median.thermo = median(depth, na.rm = TRUE))






#### TRY rLAKEANALYZER ####

library(rLakeAnalyzer)

lakes = c("L", "R", "T")
years = c(2013:2015, 2024, 2025)

for(i in 1:length(lakes)){
  
  cur.lake = lakes[i]
  
  for(j in 1:length(years)){
    
    cur.year = years[j]
    cur.data = all.routines %>% filter(lake == cur.lake & year == cur.year) %>% 
      select(date, names(all.routines)[grepl("wtr", names(all.routines))])
    
    thermos = ts.thermo.depth(cur.data, na.rm = TRUE, seasonal = TRUE) %>% 
      mutate(source = "routines", lake = cur.lake, year = cur.year)
    
    if(i == 1 & j == 1){
      all.thermo.rlake = thermos
    }
    if(i > 1 | j > 1){
      all.thermo.rlake = rbind(all.thermo.rlake, thermos)
    }
    
  }
}

all.thermo.rlake = all.thermo.rlake %>% 
  mutate(doy = yday(date))


ggplot(all.thermo.rlake, aes(x = doy, y = thermo.depth))+
  geom_point()+
  facet_grid(lake~year)+
  scale_y_reverse()+
  ylim(6, 0)+
  geom_line()


ggplot(all.thermo.rlake, aes(x = as.factor(year), y = thermo.depth))+
  geom_boxplot()+
  facet_grid(~lake)+
  scale_y_reverse()

all.thermo.rlake %>% 
  filter(lake == "T") %>% 
  group_by(year) %>% 
  summarize(median.thermo = median(thermo.depth, na.rm = TRUE))

thermos = ts.thermo.depth(cur.data, na.rm = TRUE, seasonal = TRUE) %>% 
  mutate(source = "routines", lake = cur.lake, year = cur.year)


