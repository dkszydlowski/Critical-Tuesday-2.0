library(tidyverse)



##### FINAL FORMATTING OF PAUL HYDROLAB ######
#### check if EDI data are MARSSed


# Package ID: knb-lter-ntl.371.3 Cataloging System:https://pasta.edirepository.org.
# Data set title: Cascade project at North Temperate Lakes LTER - High Frequency Data for Whole Lake Nutrient Additions 2013-2015.
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


inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/371/3/db9752988bbf4eb7d5a5491c0b642b94" 
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
                 "BGA_YSI",     
                 "BGA_HYLB",     
                 "BGA_logged_YSI",     
                 "BGA_logged_HYLB",     
                 "DO_YSI",     
                 "DO_HYLB",     
                 "DOsat_calc_YSI",     
                 "DOsat_calc_HYLB",     
                 "PH_YSI",     
                 "PH_HYLB",     
                 "Chl_YSI",     
                 "Chl_HYLB",     
                 "Temp_YSI",     
                 "Temp_HYLB"    ), check.names=TRUE)

unlink(infile1)

                          
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(as.factor(dt1$Lake))


sonde.13.15 = dt1
# Data should already include Year and DoY columns
# We’ll find consecutive NA gaps in BGA_HYLB


l.13.15 = sonde.13.15 %>% filter(Lake == "L") %>% 
  select(Year, Lake, DoY, Chl_HYLB) %>% 
  mutate(Chl_logged_HYLB = log10(Chl_HYLB+1))


# these data have pigments that are log-transformed (log10(x +1 ))
# if we didn't log-transform, the MARSSing did not converge
#chl.t24 = read.csv("G:/My Drive/Projects and Papers/PhD/Critical-Tuesday/data/formatted data/HF data/2024_Tuesday_CHL_MARSS_Danny2024.csv")
#chl.t24 = read.csv("2024_Tuesday_CHL_MARSS_2025_12_17.csv")
#chl.t25 = read.csv("G:/My Drive/Projects and Papers/PhD/Critical-Tuesday/data/formatted data/HF data/2025_Tuesday_CHL_MARSS_Danny2025.csv")
# new version of MARSS with noise at gaps
chl.t25 = read.csv("./2025_Paul_Chl_MARSS_2026_02_12 DKS.csv")
chl.t24 = read.csv("./2024_Paul_Chl_MARSS_2026_02_12 DKS.csv")

ggplot(chl.t25, aes(x = DOY, y = 10^(hylb_m -1)))+
  geom_line()

chl.t24 = chl.t24 %>% rename(Chl_HYLB = hylb_m) %>% 
  mutate(Chl_HYLB = (10^Chl_HYLB)-1) %>% 
  mutate(Chl_logged_HYLB = log10(Chl_HYLB + 1)) %>% 
  mutate(Year = 2024, Lake = "L") %>% 
  select(Year, Lake, DOY, Chl_HYLB, Chl_logged_HYLB)

chl.t25 = chl.t25 %>%  rename(Chl_HYLB = hylb_m) %>% 
  mutate(Chl_HYLB = (10^Chl_HYLB)-1) %>% 
  mutate(Chl_logged_HYLB = log10(Chl_HYLB + 1)) %>% 
  mutate(Year = 2025, Lake = "L") %>% 
  select(Year, Lake, DOY, Chl_HYLB, Chl_logged_HYLB)


### convert decimal doy to time for joining ###
doy_to_datetime <- function(year, dDOY, tz = "UTC") {
  # Separate integer day and fractional day
  day_int <- floor(dDOY)
  day_frac <- dDOY - day_int
  
  # Convert fractional day to seconds
  sec <- day_frac * 86400
  
  # Build datetime: Jan 1 + (DOY - 1 days) + fractional seconds
  origin <- as.POSIXct(paste0(year, "-01-01 00:00:00"), tz = tz)
  datetime <- origin + (day_int - 1) * 86400 + sec
  
  return(datetime)
}


chl.rec = rbind(chl.t24, chl.t25)
any(is.na(chl.rec$Chl_HYLB))
chl.rec$datetime = doy_to_datetime(chl.rec$Year, chl.rec$DOY) 
chl.rec = chl.rec %>% mutate(DOY = round(DOY, 4))


all.rec = chl.rec %>% 
  filter(datetime < as.POSIXct("2024-08-26 00:00:00", tz = "UTC") |
           datetime > as.POSIXct("2025-05-01 00:00:00", tz = "UTC"))

#### double-check DOY formatting
#all.rec = all.rec %>% mutate(datetime2 = doy_to_datetime(all.rec$Year, all.rec$DOY))

# they are off by a few seconds--will back-calculate DOY now that we've joined
get_decimal_day <- function(datetime) {
  hours <- as.numeric(format(datetime, "%H"))
  minutes <- as.numeric(format(datetime, "%M"))
  seconds <- as.numeric(format(datetime, "%S"))
  
  total_seconds <- hours * 3600 + minutes * 60 + seconds
  
  return(total_seconds / 86400)
}

all.rec = all.rec %>% mutate(DoY = get_decimal_day(datetime) + yday(datetime)) %>% 
  select(-DOY) %>% 
  select(Year, Lake, DoY, Chl_HYLB, Chl_logged_HYLB)


# create and save the final dataset
all.hylb = rbind(l.13.15, all.rec) %>% 
  filter(Lake == "L")

write.csv(all.hylb, "./data/formatted data/HF data/Paul HYLB 2013-2015 2024 2025 log-trans NEW MARSS NOISE 2026-02-25.csv", row.names = FALSE)

ggplot(all.hylb, aes(x = DoY, y = Chl_HYLB, color = Year))+
  geom_line()+
  facet_wrap(~Year)+
  theme_bw()

ggplot(all.hylb, aes(x = as.factor(Year), y = log10(Chl_HYLB), fill =as.factor(Year)))+
  geom_boxplot()+
  #facet_wrap(~Year)+
  theme_bw()+
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 16))+
  labs(x = "Year")





