#### combine all of the needed datafiles for the covariates into one ###
# version 2 fixes units of zooplankton
# version 3 adjusts units of zooplankton to match previous papers
# but zooplankton were subsequently dropped from this dataset in v4


library(tidyverse)
library(zoo)

#### PAR #####

kPAR = read.csv("./data/formatted data/kPAR 2013-2015 2024 2025 ABOVE ROUTINES SRC THERMOCLINE.csv") %>% 
  rename(Year = year, Lake = lake)

#### DOC ####

DOC = read.csv("./data/formatted data/DOC 2013-2015 2024 2025 PML.csv") %>% 
  rename(Lake = lake, Year = year)


#### thermocline ####
thermo = read.csv("./data/formatted data/thermoclines 2013-2015 2024 2025 STEEPEST SLOPE ROUTINES.csv") %>% 
  filter(thermocline == "ZT2") %>% 
  select(-thermocline) %>% 
  rename(Ztherm = depth, DOY = doy, Lake = lake, Year = year)


#### chlorophyll #####
chl = read.csv("./data/formatted data/manual chlorophyll 2013-2015 2024 2025.csv") %>% 
  rename(Year = year, Lake = lake)


#### nutrients ####
nuts = read.csv("./data/formatted data/experimental nut loads.csv") %>% 
  rename(Lake = lake, Year = year, DOY = doy)

all.data = thermo %>% 
  full_join(DOC, by = c("Lake", "Year", "DOY")) %>% 
  full_join(kPAR, by = c("Lake", "Year", "DOY")) %>% 
  full_join(nuts, by = c("Lake", "Year", "DOY")) %>% 
  full_join(chl, by = c("Lake", "Year", "DOY")) %>% 
  arrange(Lake, Year, DOY) %>% 
  group_by(Lake, Year) %>% 
  mutate(kPAR = na.approx(kPAR, rule = 2), Manual_Chl = na.approx(Manual_Chl, rule = 2),
         DOC = na.approx(DOC, rule = 2)) %>% 
  mutate(daily.load = replace(daily.load, is.na(daily.load), 0.3)) %>% 
  ungroup()

all.data = all.data %>% 
  filter(DOY >= 150 & DOY <= 245) %>% 
  group_by(Lake, Year) %>% 
  mutate(cumulative.load = cumsum(daily.load))
  



# write.csv(all.data, "./data/formatted data/simulation model inputs 2013-2015 2024 2025.csv", row.names = FALSE)

## version 2 has new error estimates with depth, and filtered DOY
# write.csv(all.data, "./data/formatted data/simulation model inputs 2013-2015 2024 2025 v2.csv", row.names = FALSE)

# version 3 has the steepest slope and thermocline from routines
  #write.csv(all.data, "./data/formatted data/simulation model inputs 2013-2015 2024 2025 v3.csv", row.names = FALSE)
  
# version 4 updates kPAR to be calculated above thermocline depths
  write.csv(all.data, "./data/formatted data/simulation model inputs 2013-2015 2024 2025 v4.csv", row.names = FALSE)
  
  
  #### plotting to check outputs

  # Tuesday
ggplot(all.data %>% filter(Lake == "T"), aes(x = DOY, y = daily.load, color= as.factor(Year)))+
  geom_point()+
  geom_line()

ggplot(all.data %>% filter(Lake == "T"), aes(x = DOY, y = cumulative.load, color= as.factor(Year)))+
  geom_point()+
  geom_line()

ggplot(all.data %>% filter(Lake == "T"), aes(x = DOY, y = Manual_Chl, color= as.factor(Year)))+
  geom_point()+
  geom_line()+
  xlim(150, 246)


ggplot(all.data %>% filter(Lake == "T"), aes(x = DOY, y = Ztherm, color= as.factor(Year)))+
  geom_point()+
  geom_line()+
  xlim(150, 246)+
  scale_y_reverse()+
  facet_wrap(~Year)


# Paul
ggplot(all.data %>% filter(Lake == "L"), aes(x = DOY, y = daily.load, color= as.factor(Year)))+
  geom_point()+
  geom_line()

ggplot(all.data %>% filter(Lake == "L"), aes(x = DOY, y = cumulative.load, color= as.factor(Year)))+
  geom_point()+
  geom_line()

ggplot(all.data %>% filter(Lake == "L"), aes(x = DOY, y = Manual_Chl, color= as.factor(Year)))+
  geom_point()+
  geom_line()+
  xlim(150, 246)


ggplot(all.data %>% filter(Lake == "L"), aes(x = DOY, y = Ztherm, color= as.factor(Year)))+
  geom_point()+
  geom_line()+
  xlim(150, 246)+
  scale_y_reverse()+
  facet_wrap(~Year)
