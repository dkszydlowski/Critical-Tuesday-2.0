## clean the PAR for Steve's model. includes cleaning of 2025 data 
# DK Szydlowski, 2025-09-03

library(tidyverse)

# read in the 2024 and 2025 light meter data
par24 = read.csv("./data/unformatted data/covariates/2024 light profiles.csv")

par25 = read.csv("./data/unformatted data/covariates/2025 light profiles.csv") %>% 
  select(-X, -X.1, -X.2)

par24.25 = rbind(par24, par25) %>% 
  mutate(Lake = replace(Lake, Lake == "Paul", "L"))%>% 
  mutate(Lake = replace(Lake, Lake == "Peter", "R"))%>% 
  mutate(Lake = replace(Lake, Lake == "Tuesday", "T"))


### read in the 2013-2015 PAR data ####

# Package ID: knb-lter-ntl.352.4 Cataloging System:https://pasta.edirepository.org.
# Data set title: Cascade Project at North Temperate Lakes LTER Core Data Physical and Chemical Limnology 1984 - 2016.
# Data set creator:  Stephen Carpenter - University of Wisconsin 
# Data set creator:  Jim Kitchell - University of Wisconsin 
# Data set creator:  Jon Cole - Cary Institute of Ecosystem Studies 
# Data set creator:  Mike Pace - University of Virginia 
# Contact:  Stephen Carpenter -  University of Wisconsin  - steve.carpenter@wisc.edu
# Contact:  Mike Pace -  University of Virginia  - pacem@virginia.edu
# Contact:    -  NTL LTER  - ntl.infomgr@gmail.com
# Stylesheet v2.15 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu      
# Uncomment the following lines to have R clear previous work, or set a working directory
# rm(list=ls())      

# setwd("C:/users/my_name/my_dir")       



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

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt1$lakeid)!="factor") dt1$lakeid<- as.factor(dt1$lakeid)
if (class(dt1$lakename)!="factor") dt1$lakename<- as.factor(dt1$lakename)
if (class(dt1$year4)=="factor") dt1$year4 <-as.numeric(levels(dt1$year4))[as.integer(dt1$year4) ]               
if (class(dt1$year4)=="character") dt1$year4 <-as.numeric(dt1$year4)
if (class(dt1$daynum)=="factor") dt1$daynum <-as.numeric(levels(dt1$daynum))[as.integer(dt1$daynum) ]               
if (class(dt1$daynum)=="character") dt1$daynum <-as.numeric(dt1$daynum)                                   
# attempting to convert dt1$sampledate dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp1sampledate<-as.Date(dt1$sampledate,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(nrow(dt1[dt1$sampledate != "",]) == length(tmp1sampledate[!is.na(tmp1sampledate)])){dt1$sampledate <- tmp1sampledate } else {print("Date conversion failed for dt1$sampledate. Please inspect the data and do the date conversion yourself.")}                                                                    

if (class(dt1$depth)=="factor") dt1$depth <-as.numeric(levels(dt1$depth))[as.integer(dt1$depth) ]               
if (class(dt1$depth)=="character") dt1$depth <-as.numeric(dt1$depth)
if (class(dt1$temperature_C)=="factor") dt1$temperature_C <-as.numeric(levels(dt1$temperature_C))[as.integer(dt1$temperature_C) ]               
if (class(dt1$temperature_C)=="character") dt1$temperature_C <-as.numeric(dt1$temperature_C)
if (class(dt1$dissolvedOxygen)=="factor") dt1$dissolvedOxygen <-as.numeric(levels(dt1$dissolvedOxygen))[as.integer(dt1$dissolvedOxygen) ]               
if (class(dt1$dissolvedOxygen)=="character") dt1$dissolvedOxygen <-as.numeric(dt1$dissolvedOxygen)
if (class(dt1$irradianceWater)=="factor") dt1$irradianceWater <-as.numeric(levels(dt1$irradianceWater))[as.integer(dt1$irradianceWater) ]               
if (class(dt1$irradianceWater)=="character") dt1$irradianceWater <-as.numeric(dt1$irradianceWater)
if (class(dt1$irradianceDeck)=="factor") dt1$irradianceDeck <-as.numeric(levels(dt1$irradianceDeck))[as.integer(dt1$irradianceDeck) ]               
if (class(dt1$irradianceDeck)=="character") dt1$irradianceDeck <-as.numeric(dt1$irradianceDeck)
if (class(dt1$comments)!="factor") dt1$comments<- as.factor(dt1$comments)

# Convert Missing Values to NA for non-dates

dt1$year4 <- ifelse((trimws(as.character(dt1$year4))==trimws("NA")),NA,dt1$year4)               
suppressWarnings(dt1$year4 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$year4))==as.character(as.numeric("NA"))),NA,dt1$year4))
dt1$daynum <- ifelse((trimws(as.character(dt1$daynum))==trimws("NA")),NA,dt1$daynum)               
suppressWarnings(dt1$daynum <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$daynum))==as.character(as.numeric("NA"))),NA,dt1$daynum))
dt1$depth <- ifelse((trimws(as.character(dt1$depth))==trimws("NA")),NA,dt1$depth)               
suppressWarnings(dt1$depth <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$depth))==as.character(as.numeric("NA"))),NA,dt1$depth))
dt1$temperature_C <- ifelse((trimws(as.character(dt1$temperature_C))==trimws("NA")),NA,dt1$temperature_C)               
suppressWarnings(dt1$temperature_C <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$temperature_C))==as.character(as.numeric("NA"))),NA,dt1$temperature_C))
dt1$dissolvedOxygen <- ifelse((trimws(as.character(dt1$dissolvedOxygen))==trimws("NA")),NA,dt1$dissolvedOxygen)               
suppressWarnings(dt1$dissolvedOxygen <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$dissolvedOxygen))==as.character(as.numeric("NA"))),NA,dt1$dissolvedOxygen))
dt1$irradianceWater <- ifelse((trimws(as.character(dt1$irradianceWater))==trimws("NA")),NA,dt1$irradianceWater)               
suppressWarnings(dt1$irradianceWater <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$irradianceWater))==as.character(as.numeric("NA"))),NA,dt1$irradianceWater))
dt1$irradianceDeck <- ifelse((trimws(as.character(dt1$irradianceDeck))==trimws("NA")),NA,dt1$irradianceDeck)               
suppressWarnings(dt1$irradianceDeck <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$irradianceDeck))==as.character(as.numeric("NA"))),NA,dt1$irradianceDeck))


# Here is the structure of the input data frame:
str(dt1)                            
attach(dt1)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(lakeid)
summary(lakename)
summary(year4)
summary(daynum)
summary(sampledate)
summary(depth)
summary(temperature_C)
summary(dissolvedOxygen)
summary(irradianceWater)
summary(irradianceDeck)
summary(comments) 
# Get more details on character variables

summary(as.factor(dt1$lakeid)) 
summary(as.factor(dt1$lakename)) 
summary(as.factor(dt1$comments))
detach(dt1)               


par13.15 = dt1



### format and combine ####
par24.25 = par24.25 %>% 
  rename(lake = Lake, year = Year, depth = Depth, parWater = IrradianceWater_uEinst_m2_s, parDeck = IrradianceDeck_uEinst_m2_s) %>% 
  select(lake, year, DOY, depth, parWater, parDeck)


par13.15 = par13.15 %>% 
  mutate(DOY = yday(sampledate)) %>% 
  rename(lake = lakeid, year = year4, parWater = irradianceWater, parDeck = irradianceDeck)

par13.15 = par13.15 %>% 
  select(lake, year, DOY, depth, parWater, parDeck)

par.all = rbind(par24.25, par13.15) %>% 
  filter(lake %in% c("L", "R", "T")) %>% 
  filter(year %in% c(2013, 2014, 2015, 2024, 2025)) %>% 
  filter(!is.na(parWater))

## calculate kPAR by day and by lake ##
kPAR = par.all %>% group_by(lake, year, DOY) %>% 
  mutate(ratio = parWater/parDeck) %>% 
  mutate(lnRatio = log(ratio)) %>% 
  filter(is.finite(lnRatio)) %>% 
  summarize(kPAR = -1*coefficients(lm(lnRatio~depth))[[2]])

ggplot(kPAR, aes(x = DOY, y = kPAR, color = as.factor(year)))+
  geom_point(size = 2)+
  geom_line(size = 1)+
  facet_wrap(~lake)+
  theme_bw()


write.csv(kPAR, "./data/formatted data/kPAR 2013-2015 2024 2025.csv", row.names = FALSE)


ggplot(kPAR %>% filter(Lake == "R"), aes(x = as.factor(Year), y = kPAR))+
  geom_boxplot()


#### make a version where kPAR is only calculated below the thermocline

# read in the thermocline
thermo = read.csv("./data/formatted data/thermoclines 2013-2015 2024 2025 STEEPEST SLOPE ROUTINES.csv") %>% 
  rename(DOY = doy, thermo.depth = depth) %>% 
  filter(thermocline == "ZT2")

kPAR = par.all %>% group_by(lake, year, DOY) %>% 
  left_join(thermo, by = c("DOY", "lake", "year")) %>% 
  filter(depth < thermo.depth) %>% 
  mutate(ratio = parWater/parDeck) %>% 
  mutate(lnRatio = log(ratio)) %>% 
  filter(is.finite(lnRatio)) %>% 
  summarize(kPAR = -1*coefficients(lm(lnRatio~depth))[[2]]) %>% 
  filter(kPAR > 0)

ggplot(kPAR, aes(x = DOY, y = kPAR, color = as.factor(year)))+
  geom_point(size = 2)+
  geom_line(size = 1)+
  facet_wrap(~lake)+
  theme_bw()


ggplot(kPAR, aes(x = year, y = kPAR, fill = as.factor(year)))+
  geom_boxplot(size = 2)+
  geom_line(size = 1)+
  facet_wrap(~lake)+
  theme_bw()

write.csv(kPAR, "./data/formatted data/kPAR 2013-2015 2024 2025 ABOVE ROUTINES SRC THERMOCLINE.csv", row.names = FALSE)



