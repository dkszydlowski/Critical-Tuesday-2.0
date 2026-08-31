## clean the chlorophyll data for Steve's model. includes cleaning of 2025 data 
# DK Szydlowski, 2025-09-03

library(tidyverse)

### gather and format chlorophyll data ####

#### 2024 and 2025 ####

chl24 = read.csv("./data/unformatted data/covariates/2024_chlorophyll.csv") %>% 
  rename(lake = Lake, year = Year) %>% 
  select(year, lake, DOY, Manual_Chl)

  # equations for converting fluoresence to chl a (ug/L)
# chl a = (Fb - Fa) *Q

# where Fb is the fluorescence before acidification
# and Fa is the fluorescence after acidification

# Q is m * (R/(R-1)) * extraction volume / filter volume
# m is the slope of the calibration curve
# R is the acid ratio from the calibration curve

# values from Dat 6/24 calibration curve 
R = 2.0100576
m = 0.0007
Q = m * (R/(R-1))


# values from Dat+Grace 6/25 calibration curve 
#R = 1.9670762
#m = 0.0008

# read in the data
chl25 = read_csv("./data/unformatted data/covariates/2025_daily_chlorophyll.csv") %>%
  rename(lake = Lake, date = Date) %>%
  mutate(date = mdy(date),
         lake = factor(lake, levels = c("Peter", "Paul", "Tuesday"))) %>%
  #Calculate the chlorophyll concentration
  mutate(chl_conc = ((Fb - Methanol_blank) - 
                       (Fa - Methanol_blank)) * Q * 
           (25/Filter_volume)) %>% 
  #Calculate daily averages from the reps
  group_by(lake, date) %>%
  summarize(mean_chl = mean(chl_conc, na.rm = TRUE),
            sd = sd(chl_conc, na.rm = TRUE)) %>%
  ungroup() %>% 
  mutate(DOY = yday(date), Manual_Chl = mean_chl, year = year(date)) %>% 
  select(year, lake, DOY, Manual_Chl) %>% 
  mutate(lake = as.character(lake)) %>% 
  mutate(lake = replace(lake, lake == "Paul", "L"))%>% 
  mutate(lake = replace(lake, lake == "Peter", "R"))%>% 
  mutate(lake = replace(lake, lake == "Tuesday", "T"))

# save the 2025 data
write.csv(chl25, "./data/formatted data/2025 chlorophyll.csv", row.names = FALSE)

##### 2013-2015 #####

# Package ID: knb-lter-ntl.372.3 Cataloging System:https://pasta.edirepository.org.
# Data set title: Cascade project at North Temperate Lakes LTER - Daily Chlorophyll Data for Whole Lake Nutrient Additions 2013-2015.
# Data set creator:  Mike Pace - University of Virginia 
# Data set creator:  Jon Cole - Cary Institute of Ecosystem Studies 
# Data set creator:  Stephen Carpenter - University of Wisconsin 
# Contact:  Mike Pace -  University of Virginia  - pacem@virginia.edu
# Contact:    -  NTL LTER  - ntl.infomgr@gmail.com
# Stylesheet v2.15 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu      
# Uncomment the following lines to have R clear previous work, or set a working directory
# rm(list=ls())      

# setwd("C:/users/my_name/my_dir")       



options(HTTPUserAgent="EDI_CodeGen")


inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/372/3/a50718a421790c9a5fc68082bb7af45e" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt1 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "Year",     
                 "Lake",     
                 "DoY",     
                 "Manual_Chl"    ), check.names=TRUE)

unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt1$Year)=="factor") dt1$Year <-as.numeric(levels(dt1$Year))[as.integer(dt1$Year) ]               
if (class(dt1$Year)=="character") dt1$Year <-as.numeric(dt1$Year)
if (class(dt1$Lake)!="factor") dt1$Lake<- as.factor(dt1$Lake)
if (class(dt1$DoY)=="factor") dt1$DoY <-as.numeric(levels(dt1$DoY))[as.integer(dt1$DoY) ]               
if (class(dt1$DoY)=="character") dt1$DoY <-as.numeric(dt1$DoY)
if (class(dt1$Manual_Chl)=="factor") dt1$Manual_Chl <-as.numeric(levels(dt1$Manual_Chl))[as.integer(dt1$Manual_Chl) ]               
if (class(dt1$Manual_Chl)=="character") dt1$Manual_Chl <-as.numeric(dt1$Manual_Chl)

# Convert Missing Values to NA for non-dates

dt1$Year <- ifelse((trimws(as.character(dt1$Year))==trimws("NA")),NA,dt1$Year)               
suppressWarnings(dt1$Year <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Year))==as.character(as.numeric("NA"))),NA,dt1$Year))
dt1$Lake <- as.factor(ifelse((trimws(as.character(dt1$Lake))==trimws("NA")),NA,as.character(dt1$Lake)))
dt1$DoY <- ifelse((trimws(as.character(dt1$DoY))==trimws("NA")),NA,dt1$DoY)               
suppressWarnings(dt1$DoY <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$DoY))==as.character(as.numeric("NA"))),NA,dt1$DoY))
dt1$Manual_Chl <- ifelse((trimws(as.character(dt1$Manual_Chl))==trimws("NA")),NA,dt1$Manual_Chl)               
suppressWarnings(dt1$Manual_Chl <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Manual_Chl))==as.character(as.numeric("NA"))),NA,dt1$Manual_Chl))


# Here is the structure of the input data frame:
str(dt1)                            
attach(dt1)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(Year)
summary(Lake)
summary(DoY)
summary(Manual_Chl) 
# Get more details on character variables

summary(as.factor(dt1$Lake))
detach(dt1)               

chl13.15 = dt1 %>% 
  rename(year = Year, lake = Lake, DOY = DoY)




##### combine all chl and save #####
chl.all = rbind(chl13.15, chl24, chl25)

ggplot(chl.all, aes(x = DOY, y = Manual_Chl, color = as.factor(year)))+
  geom_point()+
  geom_line()+
  facet_wrap(~lake)


write.csv(chl.all, "./data/formatted data/manual chlorophyll 2013-2015 2024 2025.csv", row.names = FALSE)
