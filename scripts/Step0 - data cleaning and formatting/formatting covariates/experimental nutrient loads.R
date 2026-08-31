##### make a dataframe of daily and cumulative P loads for the experiment ######
# Background daily loading of 0.3

library(tidyverse)

# get the lake_years and days from the chlorophyll data

lake = c("T", "R", "L")
doy = seq(140, 250, 1)
year= c(2013, 2014, 2015, 2024, 2025)

nut.load = expand_grid(lake, year, doy)


nut.load = nut.load %>% mutate(cumulative.load = 0, daily.load = 0.3)


# Need nutrient loading rates for Tuesday in 2013, 2014, and 2015
# and Peter in 2013, 2014, 2015, and 2019

# From Wilkinson et al. 2018 (Ecological Monographs):

# 2013: 0.5 mg P*m-2*d-1 increasing weekly by 0.3125 mg P*m-2*d-1 until doy 203,
# after which the weekly increase was 0.625. Nutrients added 154-238

# 2014: R and T had an aerial P loading rate of 3 mg P*m-2*d-1 from doy 153-241

# 2015: R had an aerial P loading rate of 3 mg P*m-2*d-1 from doy 152-180
#       T had an aerial P loading rate of 3 mg P*m-2*d-1 from doy 152-240
#

# on top of all of these, Steve suggested adding a background loading rate of 0.3 to every day
# to match approach from 2001 paper

# 2013: Mike used 84 days of nutrient loading, says stop day was 238...which would 
# have been a weekly increase day. So cut off is really 237

# 2024: Nutrient additions started on June 10th, ended August 20th

# 2025: Nutrient additions started on June 2nd, stopped on July 16th

nut.load <- nut.load %>%
  mutate(daily.load = case_when(
    year == 2013 & lake %in% c("R","T") & doy >= 154 & doy <= 202 ~ 
      0.5 + 0.3 + 0.3125 * ((doy - 154) %/% 7),
    
    year == 2013 & lake %in% c("R","T") & doy >= 203 & doy <= 237 ~ 
      (3.3 + 0.625 * ((doy - 203) %/% 7)),
    
    year == 2014 & lake %in% c("R","T") & doy >= 153 & doy <= 241 ~ 3.3,
    year == 2015 & lake == "R" & doy >= 152 & doy <= 180 ~ 3.3,
    year == 2015 & lake == "T" & doy >= 152 & doy <= 240 ~ 3.3,
    year == 2024 & lake %in% c("R","T") & doy >= 162 & doy <= 233 ~ 3.3,
    year == 2025 & lake %in% c("T") & doy >= 153 & doy <= 197 ~ 6.3,
    
    TRUE ~ daily.load
  ))



ggplot(nut.load, aes(x = doy, y = daily.load, fill = lake))+
  geom_area()+
  facet_wrap(lake~year)

nut.load = nut.load %>% mutate(year = as.numeric(year))

# calculate the cumulative loading

nut.load = nut.load %>% 
  arrange(lake, year) %>%
  group_by(lake, year) %>% 
  mutate(cumulative.load = cumsum(daily.load), 
  daily.load.no.background = daily.load - 0.3,
 cumulative.load.no.background = cumsum(daily.load - 0.3))


ggplot(nut.load, aes(x = doy, y = cumulative.load, fill = lake))+
  geom_area()+
  facet_wrap(lake~year)

nut.load = nut.load %>% 
  select(-daily.load.no.background, -cumulative.load.no.background)


write.csv(nut.load, "./data/formatted data/experimental nut loads.csv", row.names = FALSE)
