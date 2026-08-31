#### clean DOC #####

## clean the DOC for Steve's model. includes cleaning of 2025 data 
# DK Szydlowski, 2025-09-03

library(tidyverse)
library(readr)


#### 2025 DOC data #####

# compile all of the Cascade DOC data for 2025 from instrument outputs
# this was the original process to produce the dataset that is now available on EDI:

# Wilkinson, G., S. Carpenter, and M. Pace. 2026. Cascade Project at North Temperate Lakes LTER Core Data Carbon 1984 - 2025 ver 9.
# Environmental Data Initiative. https://doi.org/10.6073/pasta/ae1c3e5ed5f47836576a5d58a7b9d83c (Accessed 2026-08-31).


c1 <- read_delim("./data/unformatted data/covariates/07_14_2025_CARBON_Cascade_Ponds_Methane.txt",
                 delim = "\t", skip = 4)

c2 <- read_delim("./data/unformatted data/covariates/07_23_2025_CARBON_CASCADE_NGUYEN_QUIROZ_METHANE_PONDS.txt",
                 delim = "\t", skip = 4)

c3 <- read_delim("./data/unformatted data/covariates/05_27_2025_CARBON_LTER_Ponds_Methane_Cascade.txt",
                 delim = "\t", skip = 4)

c4 <- read_delim("./data/unformatted data/covariates/08_27_2025_CARBON_CASCADE.txt",
                 delim = "\t", skip = 4)

c5 <- read_delim("./data/unformatted data/covariates/08_29_2025_CARBON_CASCADE.txt",
                 delim = "\t", skip = 4)

c6 <- read_delim("./data/unformatted data/covariates/06_20_2025_CARBON_Cascade_Methane_Ponds_FishLake.txt",
                 delim = "\t", skip = 4)

c.all = rbind(c1, c2, c3, c4, c5, c6)


# filter for cascade
c.all = c.all %>% 
  filter(str_detect(`Sample Name`, "L25|R25|T25"))


c.test = c.all %>% 
  extract(
    `Sample Name`,
    into = c("lake", "year", "week", "depth"),
    regex = "([A-Z])([0-9]+)WEEK([0-9]+)([A-Z]+)"
  ) %>% 
  rename(ic.mgL = `Result(IC)`, doc.mgL = `Result(NPOC)`) %>% 
  select(lake, year, week, depth, ic.mgL, doc.mgL)


# average the duplicate dates
c.avg = c.test %>% 
  group_by(lake, year, week, depth) %>% 
  summarize(doc.mgL = mean(doc.mgL, na.rm = TRUE), ic.mgL = mean(ic.mgL, na.rm = TRUE))


ggplot(c.avg, aes(x = as.numeric(week), y = doc.mgL, color = lake))+
  geom_point()+
  geom_line()+
  facet_wrap(~depth)

doc25 = c.avg

## add in the dates ###
doc.dates = read.csv("./data/unformatted data/covariates/2025 light profiles.csv") %>% 
  select(Lake, Date) %>% 
  distinct() %>% 
  mutate(Date = mdy(Date))

## add in the week of the sampling date to doc.dates
doc.dates = doc.dates %>% 
  group_by(Lake) %>% 
  mutate(week = rank(Date)) %>% 
  rename(lake = Lake, date = Date)

#write.csv(doc.dates, "R:/Cascade/Data/2025 Data and Daily Code/2025 Routines/sampling dates.csv", row.names = FALSE)

doc.dates = doc.dates %>% 
  mutate(lake = replace(lake, lake == "Paul", "L"))%>% 
  mutate(lake = replace(lake, lake == "Peter", "R"))%>% 
  mutate(lake = replace(lake, lake == "Tuesday", "T")) %>% 
  mutate(week = as.character(week))

doc25 = doc25 %>% 
  mutate(year = 2025) %>% 
  left_join(doc.dates, by = c("lake", "week")) %>% 
  mutate(DOY = yday(date))

doc25 = doc25 %>%
  select(lake, year, DOY, date, week, depth, doc.mgL, ic.mgL) %>% 
  arrange(date)

#write.csv(doc25, "R:/Cascade/Data/2025 Data and Daily Code/2025 Routines/2025 DOC.csv", row.names = FALSE)

#### 2024 doc data #####
doc24 = read_csv("./data/unformatted data/covariates/2024_carbon_DTH.csv") %>%
  rename(depth = Depth, lake = Lake, date = Date) %>%
  mutate(date = ymd(date),
         DOY = yday(date),
         year = year(date),
         depth = factor(depth, levels = c("PML", "Meta", "Hypo"))) %>%
  filter(!is.na(organic_carbon)) %>% 
  rename(DOC = organic_carbon) %>% 
  filter(depth == "PML") %>% 
  select(lake, year, DOY, DOC)

## combine 2024 and 2025 doc data ##
doc25 = doc25 %>% 
  ungroup() %>% 
  rename(DOC = doc.mgL) %>% 
  filter(depth == "PML") %>% 
  select(lake, year, DOY, DOC)


doc.all = rbind(doc24, doc25)

ggplot(doc.all, aes(x = DOY, y = DOC, color = as.factor(year)))+
  geom_point()+
  geom_line()+
  facet_wrap(~lake)



##### 2013-2015 DOC data #####
# Package ID: knb-lter-ntl.350.8 Cataloging System:https://pasta.edirepository.org.
# Data set title: Cascade Project at North Temperate Lakes LTER Core Data Carbon 1984 - 2023.
# Data set creator:  Stephen Carpenter - University of Wisconsin 
# Data set creator:  Jim Kitchell - University of Wisconsin 
# Data set creator:  Jonathan Cole - Cary Institute of Ecosystem Studies 
# Data set creator:  Michael Pace - University of Virginia 
# Contact:  Stephen Carpenter -  University of Wisconsin  - steve.carpenter@wisc.edu
# Contact:  Michael Pace -  University of Virginia  - pacem@virginia.edu
# Contact:    -  NTL LTER  - ntl.infomgr@gmail.com
# Stylesheet v2.15 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu      
# Uncomment the following lines to have R clear previous work, or set a working directory
# rm(list=ls())      

# setwd("C:/users/my_name/my_dir")       



options(HTTPUserAgent="EDI_CodeGen")


inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/350/8/4130f043c28c43abe2c74d194e301e5d" 
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
                 "depth_id",     
                 "tpc",     
                 "tpn",     
                 "DIC_mg",     
                 "DIC_uM",     
                 "air_pco2",     
                 "water_pco2",     
                 "doc",     
                 "absorbance"    ), check.names=TRUE)

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

if (class(dt1$depth)!="factor") dt1$depth<- as.factor(dt1$depth)
if (class(dt1$depth_id)!="factor") dt1$depth_id<- as.factor(dt1$depth_id)
if (class(dt1$tpc)=="factor") dt1$tpc <-as.numeric(levels(dt1$tpc))[as.integer(dt1$tpc) ]               
if (class(dt1$tpc)=="character") dt1$tpc <-as.numeric(dt1$tpc)
if (class(dt1$tpn)=="factor") dt1$tpn <-as.numeric(levels(dt1$tpn))[as.integer(dt1$tpn) ]               
if (class(dt1$tpn)=="character") dt1$tpn <-as.numeric(dt1$tpn)
if (class(dt1$DIC_mg)=="factor") dt1$DIC_mg <-as.numeric(levels(dt1$DIC_mg))[as.integer(dt1$DIC_mg) ]               
if (class(dt1$DIC_mg)=="character") dt1$DIC_mg <-as.numeric(dt1$DIC_mg)
if (class(dt1$DIC_uM)=="factor") dt1$DIC_uM <-as.numeric(levels(dt1$DIC_uM))[as.integer(dt1$DIC_uM) ]               
if (class(dt1$DIC_uM)=="character") dt1$DIC_uM <-as.numeric(dt1$DIC_uM)
if (class(dt1$air_pco2)=="factor") dt1$air_pco2 <-as.numeric(levels(dt1$air_pco2))[as.integer(dt1$air_pco2) ]               
if (class(dt1$air_pco2)=="character") dt1$air_pco2 <-as.numeric(dt1$air_pco2)
if (class(dt1$water_pco2)=="factor") dt1$water_pco2 <-as.numeric(levels(dt1$water_pco2))[as.integer(dt1$water_pco2) ]               
if (class(dt1$water_pco2)=="character") dt1$water_pco2 <-as.numeric(dt1$water_pco2)
if (class(dt1$doc)=="factor") dt1$doc <-as.numeric(levels(dt1$doc))[as.integer(dt1$doc) ]               
if (class(dt1$doc)=="character") dt1$doc <-as.numeric(dt1$doc)
if (class(dt1$absorbance)=="factor") dt1$absorbance <-as.numeric(levels(dt1$absorbance))[as.integer(dt1$absorbance) ]               
if (class(dt1$absorbance)=="character") dt1$absorbance <-as.numeric(dt1$absorbance)

# Convert Missing Values to NA for non-dates

dt1$tpc <- ifelse((trimws(as.character(dt1$tpc))==trimws("NA")),NA,dt1$tpc)               
suppressWarnings(dt1$tpc <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$tpc))==as.character(as.numeric("NA"))),NA,dt1$tpc))
dt1$tpn <- ifelse((trimws(as.character(dt1$tpn))==trimws("NA")),NA,dt1$tpn)               
suppressWarnings(dt1$tpn <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$tpn))==as.character(as.numeric("NA"))),NA,dt1$tpn))
dt1$DIC_mg <- ifelse((trimws(as.character(dt1$DIC_mg))==trimws("NA")),NA,dt1$DIC_mg)               
suppressWarnings(dt1$DIC_mg <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$DIC_mg))==as.character(as.numeric("NA"))),NA,dt1$DIC_mg))
dt1$DIC_uM <- ifelse((trimws(as.character(dt1$DIC_uM))==trimws("NA")),NA,dt1$DIC_uM)               
suppressWarnings(dt1$DIC_uM <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$DIC_uM))==as.character(as.numeric("NA"))),NA,dt1$DIC_uM))
dt1$air_pco2 <- ifelse((trimws(as.character(dt1$air_pco2))==trimws("NA")),NA,dt1$air_pco2)               
suppressWarnings(dt1$air_pco2 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$air_pco2))==as.character(as.numeric("NA"))),NA,dt1$air_pco2))
dt1$water_pco2 <- ifelse((trimws(as.character(dt1$water_pco2))==trimws("NA")),NA,dt1$water_pco2)               
suppressWarnings(dt1$water_pco2 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$water_pco2))==as.character(as.numeric("NA"))),NA,dt1$water_pco2))
dt1$doc <- ifelse((trimws(as.character(dt1$doc))==trimws("NA")),NA,dt1$doc)               
suppressWarnings(dt1$doc <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$doc))==as.character(as.numeric("NA"))),NA,dt1$doc))
dt1$absorbance <- ifelse((trimws(as.character(dt1$absorbance))==trimws("NA")),NA,dt1$absorbance)               
suppressWarnings(dt1$absorbance <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$absorbance))==as.character(as.numeric("NA"))),NA,dt1$absorbance))


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
summary(depth_id)
summary(tpc)
summary(tpn)
summary(DIC_mg)
summary(DIC_uM)
summary(air_pco2)
summary(water_pco2)
summary(doc)
summary(absorbance) 
# Get more details on character variables

summary(as.factor(dt1$lakeid)) 
summary(as.factor(dt1$lakename)) 
summary(as.factor(dt1$depth)) 
summary(as.factor(dt1$depth_id))
detach(dt1)               



doc13.15 = dt1


doc13.15 = doc13.15 %>% 
  rename(year = year4, lake = lakeid, DOC = doc, DOY = daynum) %>% 
  filter(depth == "PML") %>% 
  filter(year %in% c(2013, 2014, 2015)) %>% 
  select(lake, year, DOY, DOC)


doc.all = rbind(doc.all, doc13.15)

## remove suspect points, seem to be really high outliers
# they are higher than any value ever recorded for Hummingbird Lake, for example
doc.all = doc.all %>% filter(DOC < 20)

ggplot(doc.all, aes(x = DOY, y = DOC, color = as.factor(year)))+
  geom_point()+
  geom_line()+
  facet_wrap(~lake)


write.csv(doc.all, "./data/formatted data/DOC 2013-2015 2024 2025 PML.csv", row.names = FALSE)



### investigate if DOC points were switched between hypo and PML for Tuesday
# in 2013 and 2014
doc13.15 = dt1


doc13.15 = doc13.15 %>% 
  rename(year = year4, lake = lakeid, DOC = doc, DOY = daynum) %>% 
  #filter(depth == "PML") %>% 
  filter(year %in% c(2013, 2014, 2015)) %>% 
  select(lake, year, DOY, DOC, depth)

ggplot(doc13.15 %>% filter(lake == "T"), aes(x = DOY, y = DOC, color = as.character(depth)))+
  geom_point()+
  geom_line()+
  facet_wrap(~year)

### don't appear to be swapped, must be outliers

