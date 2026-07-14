##### calculate kNC and zooplankton biomass for Table 1 ##########

library(tidyverse)


##### kNC #####

# read in the data
data = read.csv("./data/formatted data/simulation model inputs 2013-2015 2024 2025 v4.csv")

# 

knc.mean = data %>% 
  mutate(kNC = kPAR - 0.0177*Manual_Chl) %>% 
  filter((Lake == "T" | Lake == "L") & kNC > 0) %>% 
  group_by(Year, Lake) %>% 
  summarize(mean.kNC = mean(kNC, na.rm = TRUE),
            median.kNC = median(kNC, na.rm = TRUE),
            sd.kNC = sd(kNC, na.rm = TRUE),
            total.nuts = max(cumulative.load, na.rm = TRUE),
            mean.kPAR = mean(kPAR, na.rm = TRUE)) 


#### zooplankton ####
### all the zoop data combined by Dat 

# this is the Cascade zooplankton data on EDI
zoops = read.csv("./data/formatted data/cascade_zooplankton_v07_DTH.csv")

# sum by year and doy
sum.zoops = zoops %>% 
  group_by(year4, lakeid, daynum) %>% 
  summarize(total.biomass = sum(biomass, na.rm = TRUE)) %>% 
  filter(lakeid %in% c("L", "T"))


ggplot(sum.zoops, aes(x = as.factor(year4), y = total.biomass, color = lakeid))+
  geom_boxplot()


mean.biomass = sum.zoops %>% 
  filter(year4 %in% c(2013:2015, 2024, 2025)) %>% 
  group_by(year4, lakeid) %>% 
  summarize(mean.biomass = median(total.biomass, na.rm = TRUE),
            sd.biomass = sd(total.biomass, na.rm = TRUE))


# filter to relevant years and lakes and plot for supplement
sum.zoops.relevant = sum.zoops %>% 
  filter(year4 %in% c(2013:2015, 2024, 2025))




