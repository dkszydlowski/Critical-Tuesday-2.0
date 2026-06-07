#### script for gathering high frequency hydrolab data for fitting Langevin models
# for Tuesday Lake, 2013, 2014, 2015, 2024, 2025

library(tidyverse)
library(ggpubr)


#============================================================================================================
#### combine all of the current MARSS 2013-2015 data with the 2024 and 2025 data #####
new.MARSS15 = read.csv("./data/unformatted data/2015_Tuesday_CHL_MARSS_2026_01_12.csv") %>% 
  mutate(Year = 2015, Lake = "T") 
new.MARSS14 = read.csv("./data/unformatted data/2014_Tuesday_CHL_MARSS_2026_01_09.csv") %>% 
  mutate(Year = 2014, Lake = "T")
new.MARSS13 = read.csv("./data/unformatted data/2013_Tuesday_CHL_MARSS_2026_01_09.csv") %>% 
  mutate(Year = 2013, Lake = "T")

all.new.marss = rbind(new.MARSS15, new.MARSS14, new.MARSS13)

#### format the 2013-2015 data to match recent data
t13.15 = all.new.marss %>% rename(Chl_HYLB = hylb_m, DoY = DOY) %>% 
  mutate(Chl_logged_HYLB = log10(Chl_HYLB + 1))%>% 
  select(Year, Lake, DoY, Chl_HYLB, Chl_logged_HYLB)


# new version of MARSS with noise at gaps
chl.t25 = read.csv("./data/unformatted data/2025_Tuesday_CHL_MARSS_2026_01_19 DKS.csv")
chl.t24 = read.csv("./data/unformatted data/2024_Tuesday_CHL_MARSS_2026_01_19 DKS.csv")

ggplot(chl.t25, aes(x = DOY, y = hylb_m))+
  geom_line()

ggplot(chl.t24, aes(x = DOY, y = hylb_m))+
  geom_line()

chl.t24 = chl.t24 %>% rename(Chl_HYLB = hylb_m) %>% 
  mutate(Chl_logged_HYLB = log10(Chl_HYLB + 1)) %>% 
  mutate(Year = 2024, Lake = "T") %>% 
  select(Year, Lake, DOY, Chl_HYLB, Chl_logged_HYLB)

chl.t25 = chl.t25 %>% rename(Chl_HYLB = hylb_m) %>% 
  mutate(Chl_logged_HYLB = log10(Chl_HYLB + 1)) %>% 
  mutate(Year = 2025, Lake = "T") %>% 
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
all.hylb = rbind(t13.15, all.rec) %>% 
  filter(Lake == "T")

write.csv(all.hylb, "./data/formatted data/HF data/Tuesday HYLB 2013-2015 2024 2025 log-trans NEW MARSS NOISE 2026-02-16.csv", row.names = FALSE)

# plot and check
ggplot(all.hylb, aes(x = DoY, y = Chl_HYLB, color = Year))+
  geom_line()+
  facet_wrap(~Year)+
  theme_bw()


ggplot(all.hylb, aes(x = DoY, y = Chl_logged_HYLB, color = Year))+
  geom_line()+
  facet_wrap(~Year)+
  theme_bw()


