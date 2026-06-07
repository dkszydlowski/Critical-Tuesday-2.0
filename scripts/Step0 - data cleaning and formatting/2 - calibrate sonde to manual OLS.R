#### compare manual and sonde chlorophyll #####
# The sonde chlorophyll has different relationships to the trusted manual standard
# in each year. We sought to try to correct the HF sonde chl to the manuals


library(tidyverse)
library(ggpmisc)
library(ggpubr)
library(slider)




###### 3. Predict HF Manual - Tuesday Lake ######
hf = read.csv("./data/formatted data/HF data/Tuesday HYLB 2013-2015 2024 2025 log-trans NEW MARSS NOISE 2026-02-16.csv") %>% 
  filter(DoY < 244) # cut end of season for fitting

# plot the data
ggplot(hf, aes(x = DoY, y = Chl_HYLB))+
  geom_line()+
  facet_wrap(~Year)

# convert DoY back to datetime
hf <- hf %>%
  mutate(
    datetime = make_datetime(Year, 1, 1, tz = "UTC") +
      ddays(DoY - 1)
  )

# take the mean of the sonde pigments in the morning
morning.pigs = hf %>% 
  filter(format(datetime, "%H") %in% c("06", "07", "08", "09")) %>% 
  mutate(roundDoY = round(DoY)) %>% 
  select(Lake, Year, roundDoY, DoY, Chl_HYLB) %>% 
  group_by(Lake, Year, roundDoY) %>% 
  summarize(Chl_HYLB = mean((Chl_HYLB), na.rm = TRUE)) %>% 
  mutate(Lake = "T")


# read in the manual pigments and combine
man.pigs.morning = read.csv("./data/formatted data/simulation model inputs 2013-2015 2024 2025 v4.csv") %>% 
  rename(roundDoY = DOY) %>% 
  select(Lake, Year, roundDoY, Manual_Chl) %>% 
  left_join(morning.pigs, by = c("Lake", "Year", "roundDoY")) %>% 
  filter(Lake == "T") %>% 
  mutate(lman = log10(Manual_Chl), lsonde = log10(Chl_HYLB)) %>% 
  mutate(
    Manual_Chl = case_when(
      Year == 2013 & (roundDoY >= 245 | roundDoY <= 145) ~  NA,
      Year == 2014 & (roundDoY >= 242 | roundDoY <= 145)  ~ NA,
      Year == 2015 & (roundDoY >= 247 | roundDoY <= 144)  ~ NA,
      Year == 2024 & (roundDoY >= 240 | roundDoY <= 147)  ~ NA,
      Year == 2025 & (roundDoY >= 235 | roundDoY <= 145)  ~ NA,
      TRUE ~ Manual_Chl)) 

# plot manuals
ggplot(man.pigs.morning, aes(x = roundDoY, y = Manual_Chl, color = as.factor(Year)))+
  geom_line(size = 0.7)+
  geom_point(size = 1)+
  theme_classic()+
  facet_wrap(~Year)

ggplot(man.pigs.morning, aes(x = lsonde, y = log10(Manual_Chl)))+
  geom_point(size = 1)+
  geom_smooth(method = "lm", se = FALSE)+
  theme_classic()+
  facet_wrap(~Year)+
  stat_poly_eq(
    formula = y ~ x,
    aes(label = ..rr.label..),
    parse = TRUE,
    size = 4,
    label.x = 0.05,  
    label.y = 0.05)+
  labs(x = "log10(Sonde Chlorophyll μg/L)", y = "log10(Manual Chlorophyll μg/L)", title = "Tuesday Lake Manual vs. Sonde Chlorophyll")+
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        strip.text = element_text(size = 12))



ggplot(man.pigs.morning, aes(x = lsonde, y = log10(Manual_Chl), color = as.factor(Year)))+
  #geom_line(size = 0.7)+
  geom_point(size = 1.2, alpha = 0.5)+
  theme_classic()+
  geom_smooth(method = "lm", se = FALSE, size = 1.5)+
  labs(x = "log10(sonde chl)", y = "log10(Manual Chlorophyll)")+
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        legend.text = element_text(size = 16),
        legend.title = element_blank())
#facet_wrap(~Year)



# calculate regressions by year
year_fits = man.pigs.morning %>%
  filter(is.finite(lman), is.finite(lsonde)) %>%
  group_by(Year) %>%
  do({fit <- lm(lman ~ lsonde, data = .)
  data.frame(a_year = coef(fit)[1], b_year = coef(fit)[2])
  })

# add in individual year regression info
hf_cal = hf %>%
  mutate(lsonde = log10(Chl_HYLB)) %>%
  left_join(year_fits, by = "Year") %>%
  mutate(
    lsonde_cal = a_year + b_year*lsonde,
    Chl_HYLB_cal = 10^lsonde_cal
  )


ggplot(hf_cal, aes(x = DoY, y = Chl_HYLB_cal))+
  geom_line()+
  facet_wrap(~Year)

# take morning average of hf_cal
# compare to manuals
morning.pigs.cal = hf_cal %>% 
  filter(format(datetime, "%H") %in% c("06", "07", "08", "09")) %>% 
  mutate(roundDoY = round(DoY)) %>% 
  select(Lake, Year, roundDoY, DoY, Chl_HYLB_cal, lsonde_cal) %>% 
  group_by(Lake, Year, roundDoY) %>% 
  summarize(Chl_HYLB_cal = mean(Chl_HYLB_cal, na.rm = TRUE)) %>% 
  mutate(Lake = "T")

ggplot(morning.pigs.cal, aes(x = roundDoY, y = (Chl_HYLB_cal), color =as.factor(Year)))+
  geom_line(size = 1)+
  theme_bw()+
  facet_wrap(~Year)

man.pigs.morning.cal = read.csv("./data/formatted data/simulation model inputs 2013-2015 2024 2025 v4.csv") %>% 
  rename(roundDoY = DOY) %>% 
  select(Lake, Year, roundDoY, Manual_Chl) %>% 
  left_join(morning.pigs.cal, by = c("Lake", "Year", "roundDoY")) %>% 
  filter(Lake == "T") %>% 
  mutate(lman = log10(Manual_Chl), lsonde = log10(Chl_HYLB_cal)) %>% 
  mutate(
    Manual_Chl = case_when(
      Year == 2013 & (roundDoY >= 245 | roundDoY <= 145) ~ NA,
      Year == 2014 & (roundDoY >= 242 | roundDoY <= 145)  ~ NA,
      Year == 2015 & (roundDoY >= 247 | roundDoY <= 144)  ~ NA,
      Year == 2024 & (roundDoY >= 240 | roundDoY <= 147)  ~ NA,
      Year == 2025 & (roundDoY >= 235 | roundDoY <= 145)  ~ NA,
      TRUE ~ Manual_Chl)) 

ggplot(man.pigs.morning.cal, aes(x = log10(Chl_HYLB_cal), y = log10(Manual_Chl), color = as.factor(Year)))+
  geom_point(size = 1)+
  geom_smooth(method = "lm", se = FALSE)+
  theme_classic()+
  facet_wrap(~Year)+
  stat_poly_eq(
    formula = y ~ x,
    aes(label = ..rr.label..),
    parse = TRUE,
    size = 4,
    label.x = 0.05,  
    label.y = 0.05)

ggplot(man.pigs.morning.cal, aes(x = roundDoY))+
  geom_line(aes(y = Manual_Chl, color = "Manual"), size = 1)+
  geom_line(aes(y = Chl_HYLB_cal, color = "HYLB_CORRECTED"), size = 1)+
  #geom_smooth(method = "lm", se = FALSE)+
  theme_classic()+
  facet_wrap(~Year)+
  scale_color_manual(
    name = NULL,
    values = c(
      "Manual" = "lightblue3",
      "HYLB_CORRECTED" = "red3"
    )
  )+
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        legend.text = element_text(size = 14),
        strip.text = element_text(size = 14))
  # stat_poly_eq(
  #   formula = y ~ x,
  #   aes(label = ..rr.label..),
  #   parse = TRUE,
  #   size = 4,
  #   label.x = 0.05,  
  #   label.y = 0.05)





### what does the uncorrected look like?


ggplot(man.pigs.morning, aes(x = roundDoY))+
  geom_line(aes(y = Manual_Chl, color = "Manual"), size = 1)+
  geom_line(aes(y = Chl_HYLB, color = "HYLB Sonde"), size = 1)+
  #geom_smooth(method = "lm", se = FALSE)+
  theme_classic()+
  facet_wrap(~Year)+
  scale_color_manual(
    name = NULL,
    values = c(
      "Manual" = "lightblue3",
      "HYLB Sonde" = "red3"
    )
  )+
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        legend.text = element_text(size = 14),
        strip.text = element_text(size = 14))


ggplot(man.pigs.morning.cal, aes(x = log10(Manual_Chl), y = log10(Chl_HYLB_cal), color = as.factor(Year)))+
  geom_point(size = 1)+
  geom_smooth(method = "lm", se = FALSE)+
  theme_classic()+
  facet_wrap(~Year)+
  stat_poly_eq(
    formula = y ~ x,
    aes(label = ..rr.label..),
    parse = TRUE,
    size = 4,
    label.x = 0.05,  
    label.y = 0.05)


man.pigs.morning.cal %>%
  mutate(diff = log10(Manual_Chl) - log10(Chl_HYLB_cal)) %>%
  group_by(Year) %>%
  summarize(
    mean_bias = mean(diff, na.rm = TRUE),
    sd_bias   = sd(diff, na.rm = TRUE)
  )

man.pigs.morning %>%
  mutate(diff = log10(Manual_Chl) - log10(Chl_HYLB)) %>%
  group_by(Year) %>%
  summarize(
    mean_bias = mean(diff, na.rm = TRUE),
    sd_bias   = sd(diff, na.rm = TRUE),
    n = sum(is.finite(diff))
  )

write.csv(hf_cal, "./data/formatted data/HF data/Predicted Tuesday HYLB on Manual Scale log-trans NOISY.csv")















###### Predict HF Manual - Paul Lake #########



###### 3. Predict HF Manual ######
hf = read.csv("./data/formatted data/HF data/Paul HYLB 2013-2015 2024 2025 log-trans NEW MARSS NOISE 2026-02-25.csv") %>% 
  filter(DoY < 244)

# plot the data
ggplot(hf, aes(x = DoY, y = log10(Chl_HYLB)))+
  geom_line()+
  facet_wrap(~Year)

# convert DoY back to datetime
hf <- hf %>%
  mutate(
    datetime = make_datetime(Year, 1, 1, tz = "UTC") +
      ddays(DoY - 1)
  )

# take the mean of the sonde pigments in the morning
morning.pigs = hf %>% 
  filter(format(datetime, "%H") %in% c("06", "07", "08", "09")) %>% 
  mutate(roundDoY = round(DoY)) %>% 
  select(Lake, Year, roundDoY, DoY, Chl_HYLB) %>% 
  group_by(Lake, Year, roundDoY) %>% 
  summarize(Chl_HYLB = mean((Chl_HYLB), na.rm = TRUE)) %>% 
  mutate(Lake = "L")


# read in the manual pigments and combine
man.pigs.morning = read.csv("./data/formatted data/simulation model inputs 2013-2015 2024 2025 v4.csv") %>% 
  rename(roundDoY = DOY) %>% 
  select(Lake, Year, roundDoY, Manual_Chl) %>% 
  left_join(morning.pigs, by = c("Lake", "Year", "roundDoY")) %>% 
  filter(Lake == "L") %>% 
  mutate(lman = log10(Manual_Chl), lsonde = log10(Chl_HYLB)) %>% 
  mutate(
    Manual_Chl = case_when(
      Year == 2013 & (roundDoY >= 245 | roundDoY <= 145) ~  NA,
      Year == 2014 & (roundDoY >= 242 | roundDoY <= 145)  ~ NA,
      Year == 2015 & (roundDoY >= 247 | roundDoY <= 144)  ~ NA,
      Year == 2024 & (roundDoY >= 240 | roundDoY <= 147)  ~ NA,
      Year == 2025 & (roundDoY >= 235 | roundDoY <= 145)  ~ NA,
      TRUE ~ Manual_Chl)) 

# plot manuals
ggplot(man.pigs.morning, aes(x = roundDoY, y = Manual_Chl, color = as.factor(Year)))+
  geom_line(size = 0.7)+
  geom_point(size = 1)+
  theme_classic()+
  facet_wrap(~Year)+
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 12))



ggplot(man.pigs.morning, aes(x = as.factor(Year), y = Manual_Chl, fill = as.factor(Year)))+
  geom_boxplot(size = 0.7)+
  geom_point(size = 1)+
  theme_classic()+
  labs(x = "Year")+
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 12))

ggplot(man.pigs.morning %>% filter(lsonde < 0.8 & log10(Manual_Chl) < 0.9 & log10(Manual_Chl) > 0 & roundDoY > 152), aes(x = lsonde, y = log10(Manual_Chl), color = as.factor(Year)))+
  geom_point(size = 1)+
  geom_smooth(method = "lm", se = FALSE)+
  theme_classic()+
  facet_wrap(~Year)+
  stat_poly_eq(
    formula = y ~ x,
    aes(label = ..rr.label..),
    parse = TRUE,
    size = 4,
    label.x = 0.05,  
    label.y = 0.05)+
  labs(x = "log(HYLB CHL)", y = "log(Manual Chlorophyll)")


ggplot(man.pigs.morning %>% filter(lsonde < 0.8 & log10(Manual_Chl) < 0.9 & log10(Manual_Chl) > 0 & roundDoY > 152), aes(x = lsonde, y = log10(Manual_Chl)))+
  geom_point(size = 1)+
  geom_smooth(method = "lm", se = FALSE)+
  theme_classic()+
  facet_wrap(~Year)+
  stat_poly_eq(
    formula = y ~ x,
    aes(label = ..rr.label..),
    parse = TRUE,
    size = 4,
    label.x = 0.05,  
    label.y = 0.05)+
  labs(x = "log10(Sonde Chlorophyll μg/L)", y = "log10(Manual Chlorophyll μg/L)", title = "Paul Lake Manual vs. Sonde Chlorophyll")+
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        strip.text = element_text(size = 12))

ggplot(man.pigs.morning, aes(x = lsonde, y = log10(Manual_Chl), color = as.factor(Year)))+
  #geom_line(size = 0.7)+
  geom_point(size = 1)+
  theme_classic()+
  geom_smooth(method = "lm", se = FALSE)
#facet_wrap(~Year)



# calculate regressions by year
year_fits = man.pigs.morning %>%
  filter(lsonde < 0.8 & log10(Manual_Chl) < 0.9 & log10(Manual_Chl) > 0) %>% # fit only in acceptable range
  filter(is.finite(lman), is.finite(lsonde)) %>%
  group_by(Year) %>%
  do({fit <- lm(lman ~ lsonde, data = .)
  data.frame(a_year = coef(fit)[1], b_year = coef(fit)[2])
  })

# add in individual year regression info
hf_cal = hf %>%
  mutate(lsonde = log10(Chl_HYLB)) %>%
  left_join(year_fits, by = "Year") %>%
  mutate(
    lsonde_cal = a_year + b_year*lsonde,
    Chl_HYLB_cal = 10^lsonde_cal
  )


ggplot(hf_cal, aes(x = DoY, y = Chl_HYLB_cal))+
  geom_line()+
  facet_wrap(~Year)

# take morning average of hf_cal
# compare to manuals
morning.pigs.cal = hf_cal %>% 
  filter(format(datetime, "%H") %in% c("06", "07", "08", "09")) %>% 
  mutate(roundDoY = round(DoY)) %>% 
  select(Lake, Year, roundDoY, DoY, Chl_HYLB_cal, lsonde_cal) %>% 
  group_by(Lake, Year, roundDoY) %>% 
  summarize(Chl_HYLB_cal = mean(Chl_HYLB_cal, na.rm = TRUE)) %>% 
  mutate(Lake = "L") %>% 
  filter(roundDoY >= 152)

ggplot(morning.pigs.cal, aes(x = roundDoY, y = (Chl_HYLB_cal), color =as.factor(Year)))+
  geom_line(size = 1)+
  theme_bw()+
  facet_wrap(~Year)

man.pigs.morning.cal = read.csv("./data/formatted data/simulation model inputs 2013-2015 2024 2025 v4.csv") %>% 
  rename(roundDoY = DOY) %>% 
  select(Lake, Year, roundDoY, Manual_Chl) %>% 
  left_join(morning.pigs.cal, by = c("Lake", "Year", "roundDoY")) %>% 
  filter(Lake == "L") %>% 
  mutate(lman = log10(Manual_Chl), lsonde = log10(Chl_HYLB_cal)) %>% 
  mutate(
    Manual_Chl = case_when(
      Year == 2013 & (roundDoY >= 245 | roundDoY <= 145) ~ NA,
      Year == 2014 & (roundDoY >= 242 | roundDoY <= 145)  ~ NA,
      Year == 2015 & (roundDoY >= 247 | roundDoY <= 144)  ~ NA,
      Year == 2024 & (roundDoY >= 240 | roundDoY <= 147)  ~ NA,
      Year == 2025 & (roundDoY >= 235 | roundDoY <= 150)  ~ NA,
      TRUE ~ Manual_Chl)) 

ggplot(man.pigs.morning.cal, aes(x = log10(Chl_HYLB_cal), y = log10(Manual_Chl), color = as.factor(Year)))+
  geom_point(size = 1)+
  geom_smooth(method = "lm", se = FALSE)+
  theme_classic()+
  facet_wrap(~Year)+
  stat_poly_eq(
    formula = y ~ x,
    aes(label = ..rr.label..),
    parse = TRUE,
    size = 4,
    label.x = 0.05,  
    label.y = 0.05)

ggplot(man.pigs.morning.cal %>% filter(roundDoY >= 152), aes(x = roundDoY))+
  geom_line(aes(y = Manual_Chl, color = "Manual"), size = 1)+
  geom_line(aes(y = Chl_HYLB_cal, color = "HYLB_CORRECTED"), size = 1)+
  #geom_smooth(method = "lm", se = FALSE)+
  theme_classic()+
  facet_wrap(~Year)+
  scale_color_manual(
    name = NULL,
    values = c(
      "Manual" = "lightblue3",
      "HYLB_CORRECTED" = "red3"
    )
  )
# stat_poly_eq(
#   formula = y ~ x,
#   aes(label = ..rr.label..),
#   parse = TRUE,
#   size = 4,
#   label.x = 0.05,  
#   label.y = 0.05)

ggplot(man.pigs.morning.cal, aes(x = log10(Manual_Chl), y = log10(Chl_HYLB_cal), color = as.factor(Year)))+
  geom_point(size = 1)+
  geom_smooth(method = "lm", se = FALSE)+
  theme_classic()+
  #facet_wrap(~Year)+
  stat_poly_eq(
    formula = y ~ x,
    aes(label = ..rr.label..),
    parse = TRUE,
    size = 4,
    label.x = 0.05,  
    label.y = 0.05)


man.pigs.morning.cal %>%
  mutate(diff = log10(Manual_Chl) - log10(Chl_HYLB_cal)) %>%
  group_by(Year) %>%
  summarize(
    mean_bias = mean(diff, na.rm = TRUE),
    sd_bias   = sd(diff, na.rm = TRUE)
  )

man.pigs.morning %>%
  mutate(diff = log10(Manual_Chl) - log10(Chl_HYLB)) %>%
  group_by(Year) %>%
  summarize(
    mean_bias = mean(diff, na.rm = TRUE),
    sd_bias   = sd(diff, na.rm = TRUE),
    n = sum(is.finite(diff))
  )

write.csv(hf_cal, "./data/formatted data/HF data/Predicted Paul HYLB on Manual Scale log-trans.csv")





ggplot(man.pigs.morning.cal, aes(x = as.factor(Year), y = (Chl_HYLB_cal), fill = as.factor(Year)))+
  geom_boxplot()+
  theme_classic()+
  stat_poly_eq(
    formula = y ~ x,
    aes(label = ..rr.label..),
    parse = TRUE,
    size = 4,
    label.x = 0.05,  
    label.y = 0.05)



hf_cal = read.csv("./data/formatted data/HF data/Predicted Paul HYLB on Manual Scale log-trans.csv")

ggplot(hf_cal, aes(x = DoY, y = Chl_HYLB))+
  geom_line()+
  facet_wrap(~Year, nrow = 5, ncol = 1)
