# Danny's conversion 0f sonde chl to manual chl 2026-06-03

## in this script, we corrected the data using ARIMA models to account for autocorrelation.
## in a different script, we corrected the data using ordinary least squares regression
# the minimum values in Tuesday are -1.76, so the log transformation is log10(x + 2)

# Danny's conversion 0f sonde chl to manual chl 2026-06-03

#rm(list = ls()) 
#graphics.off()

library(tidyverse)
library(forecast)
library(parallel)
library(ggpmisc)

options(mc.cores = parallel::detectCores())

# read in the chl data for correction
# Manual_Chl is the manual chlorophyll
# Chl_HYLB is the sonde chlorophyll
# lman is the log10-transformed manual chlorophyll
# lsonde is the log10-transformed sonde chlorophyll


##### create a dataset for doing the correction #####
# called allchl.ct (all chl corrected)
# read in hf data

#========================================================================================================================================#
###### TUESDAY #######

hf = read.csv("./data/formatted data/HF data/Tuesday HYLB 2013-2015 2024 2025 log-trans NEW MARSS NOISE 2026-02-16.csv") %>% 
  filter(DoY < 244)

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

# take the mean of the sonde pigments in the morning (6AM - 8AM)
# this is so we can compare to the morning manual grabs and avoid photoquenching
morning.pigs = hf %>% 
  filter(format(datetime, "%H") %in% c("06", "07", "08", "09")) %>% 
  mutate(roundDoY = round(DoY)) %>% 
  select(Lake, Year, roundDoY, DoY, Chl_HYLB) %>% 
  group_by(Lake, Year, roundDoY) %>% 
  summarize(Chl_HYLB = mean((Chl_HYLB), na.rm = TRUE)) %>% 
  mutate(Lake = "T")

# read in the manual pigments and combine with morning average sonde
man.pigs.morning = read.csv("./data/formatted data/simulation model inputs 2013-2015 2024 2025 v4.csv") %>% 
  rename(roundDoY = DOY) %>% 
  select(Lake, Year, roundDoY, Manual_Chl) %>% 
  left_join(morning.pigs, by = c("Lake", "Year", "roundDoY")) %>% 
  filter(Lake == "T") %>% 
  mutate(lman = log10(Manual_Chl), lsonde = log10(Chl_HYLB+ 2)) %>% 
  mutate(
    Manual_Chl = case_when(
      Year == 2013 & (roundDoY >= 245 | roundDoY <= 145) ~  NA, #### remove periods of time at beginning and end of season that were interpolated for simulation model
      Year == 2014 & (roundDoY >= 242 | roundDoY <= 145)  ~ NA,
      Year == 2015 & (roundDoY >= 247 | roundDoY <= 144)  ~ NA,
      Year == 2024 & (roundDoY >= 240 | roundDoY <= 147)  ~ NA,
      Year == 2025 & (roundDoY >= 235 | roundDoY <= 145)  ~ NA,
      TRUE ~ Manual_Chl)) 

# plot the manuals
ggplot(man.pigs.morning, aes(x = roundDoY, y = Manual_Chl, color = as.factor(Year)))+
  geom_line(size = 0.7)+
  geom_point(size = 1)+
  theme_classic()+
  facet_wrap(~Year)

# plot log of the manuals vs log of the sonde
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

# plot manual vs sonde all in one panel
ggplot(man.pigs.morning, aes(x = lsonde, y = log10(Manual_Chl), color = as.factor(Year)))+
  #geom_line(size = 0.7)+
  geom_point(size = 1)+
  theme_classic()+
  geom_smooth(method = "lm", se = FALSE)
#facet_wrap(~Year)

# remove NA values, which are only at the beginning and end of years
allchl.ct = man.pigs.morning %>% 
  filter(!is.na(Manual_Chl) & !is.na(Chl_HYLB)) %>% 
  rename(doy = roundDoY, lake = Lake, year = Year)


# read in the high frequency data again for prediction later on
all.hylb = read.csv("./data/formatted data/HF data/Tuesday HYLB 2013-2015 2024 2025 log-trans NEW MARSS NOISE 2026-02-16.csv")

# convert DoY back to datetime
all.hylb <- all.hylb %>%
  mutate(
    datetime = make_datetime(Year, 1, 1, tz = "UTC") +
      ddays(DoY - 1)
  )

print('the dataframe has zero NA values',quote=F)
print(allchl.ct[1,])
print(dim(allchl.ct))
print(dim(na.omit(allchl.ct)))

# transform chl
print(c('range Manual Chl ',range(allchl.ct$Manual_Chl)),quote=F)
print(c('range Sonde Chl ',range(allchl.ct$Chl_HYLB)),quote=F)

### rename to datall
# log-transform the pigment columns for closer to normal model residuals
datall = allchl.ct
datall$Mchl = log10(allchl.ct$Manual_Chl)
datall$Hchl = log10(allchl.ct$Chl_HYLB + 2)



## 2013 ##
# Try auto.arima on 2013 
print('auto.arima for 2013',quote=F)
dat13 = subset(datall,subset=(year == 2013))
X.c = dat13$Hchl - mean(dat13$Hchl)  # center Hchl
Y.c = dat13$Mchl - mean(dat13$Mchl)  # center Mchl

# fit the model
aa13 = auto.arima(y=Y.c,ic='aicc',xreg=X.c,
                  stepwise=F,test='adf',parallel=F)
print(summary(aa13))
print('correlation of y and yhat',quote=F)
print(cor(Y.c,aa13$fitted))

# dataframe of model predicted and original values
# with the mean added back in
predicted13 = data.frame(
  year = 2013,
  doy = dat13$doy,
  predicted = aa13$fitted + mean(dat13$Mchl),
  original = Y.c + mean(dat13$Mchl))

# plot the results
ggplot(predicted13, aes(x = original, y = as.numeric(predicted)))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)+
  stat_poly_eq(
    formula = y ~ x,
    aes(label = ..rr.label..),
    parse = TRUE,
    size = 4,
    label.x = 0.05,  
    label.y = 0.95)

# plot as time series
ggplot(predicted13, aes(x = doy, y = 10^as.numeric(predicted)))+
  geom_point()+
  geom_line(data = predicted13, aes(x = doy, y = 10^original))





## 2014 ##
# Try auto.arima on 2014 
print('auto.arima for 2014',quote=F)
dat14 = subset(datall,subset=(year == 2014))
X.c = dat14$Hchl - mean(dat14$Hchl)  # center Hchl
Y.c = dat14$Mchl - mean(dat14$Mchl)  # center Mchl

# fit the model
aa14 = auto.arima(y=Y.c,ic='aicc',xreg=X.c,
                  stepwise=F,test='adf',parallel=F)

print(summary(aa14))
print('correlation of y and yhat',quote=F)
print(cor(Y.c,aa14$fitted))

# make dataframe of fitted and original values
predicted14 = data.frame(
  year = 2014,
  doy = dat14$doy,
  predicted = aa14$fitted + mean(dat14$Mchl),
  original = Y.c + mean(dat14$Mchl))

# plot the corrected data vs original
ggplot(predicted14, aes(x = original, y = as.numeric(predicted)))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)+
  stat_poly_eq(
    formula = y ~ x,
    aes(label = ..rr.label..),
    parse = TRUE,
    size = 4,
    label.x = 0.05,  
    label.y = 0.05)

ggplot(predicted14, aes(x = doy, y = 10^as.numeric(predicted)))+
  geom_point()+
  geom_line(data = predicted14, aes(x = doy, y = 10^original))





## 2015 ##
# Try auto.arima on 2015 
print('auto.arima for 2015',quote=F)
dat15 = subset(datall,subset=(year == 2015))
X.c = dat15$Hchl - mean(dat15$Hchl)  # center Hchl
Y.c = dat15$Mchl - mean(dat15$Mchl)  # center Mchl
aa15 = auto.arima(y=Y.c,ic='aicc',xreg=X.c,
                  stepwise=F,test='adf',parallel=F)
print(summary(aa15))
print('correlation of y and yhat',quote=F)
print(cor(Y.c,aa15$fitted))

predicted15 = data.frame(
  year = 2015,
  doy = dat15$doy,
  predicted = aa15$fitted + mean(dat15$Mchl),
  original = Y.c + mean(dat15$Mchl))

ggplot(predicted15, aes(x = original, y = as.numeric(predicted)))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)+
  geom_smooth(method = "lm", se = FALSE)+
  stat_poly_eq(
    formula = y ~ x,
    aes(label = ..rr.label..),
    parse = TRUE,
    size = 4,
    label.x = 0.05,  
    label.y = 0.05)

ggplot(predicted15, aes(x = doy, y = 10^as.numeric(predicted)))+
  geom_point()+
  geom_line(data = predicted15, aes(x = doy, y = 10^original))





## 2024 ##
# Try auto.arima on 2024 
print('auto.arima for 2024',quote=F)
dat24 = subset(datall,subset=(year == 2024))
X.c = dat24$Hchl - mean(dat24$Hchl)  # center Hchl
Y.c = dat24$Mchl - mean(dat24$Mchl)  # center Mchl

# fit the model
aa24 = auto.arima(y=Y.c,ic='aicc',xreg=X.c,
                  stepwise=F,test='adf',parallel=F)

print(summary(aa24))
print('correlation of y and yhat',quote=F)
print(cor(Y.c,aa24$fitted))

# make dataframe of fitted and original values
predicted24 = data.frame(
  year = 2024,
  doy = dat24$doy,
  predicted = aa24$fitted + mean(dat24$Mchl),
  original = Y.c + mean(dat24$Mchl))

ggplot(predicted24, aes(x = original, y = as.numeric(predicted)))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)+
  geom_smooth(method = "lm", se = FALSE)+
  stat_poly_eq(
    formula = y ~ x,
    aes(label = ..rr.label..),
    parse = TRUE,
    size = 4,
    label.x = 0.05,  
    label.y = 0.05)

ggplot(predicted24, aes(x = doy, y = 10^as.numeric(predicted)))+
  geom_point()+
  geom_line(data = predicted24, aes(x = doy, y = 10^original))




## 2025 ##
# Try auto.arima on 2025 
print('auto.arima for 2025',quote=F)
dat25 = subset(datall,subset=(year == 2025))
X.c = dat25$Hchl - mean(dat25$Hchl)  # center Hchl
Y.c = dat25$Mchl - mean(dat25$Mchl)  # center Mchl

# fit the model
aa25 = auto.arima(y=Y.c,ic='aicc',xreg=X.c,
                  stepwise=F,test='adf',parallel=F)
print(summary(aa25))
print('correlation of y and yhat',quote=F)
print(cor(Y.c,aa25$fitted))

# dataframe of fitted and original
predicted25 = data.frame(
  year = 2025,
  doy = dat25$doy,
  predicted = aa25$fitted + mean(dat25$Mchl),
  original = Y.c + mean(dat25$Mchl))

# plot original and corrected
ggplot(predicted25, aes(x = original, y = as.numeric(predicted)))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)+
  geom_smooth(method = "lm", se = FALSE)+
  stat_poly_eq(
    formula = y ~ x,
    aes(label = ..rr.label..),
    parse = TRUE,
    size = 4,
    label.x = 0.05,  
    label.y = 0.05)

ggplot(predicted25, aes(x = doy, y = 10^as.numeric(predicted)))+
  geom_point()+
  geom_line(data = predicted25, aes(x = doy, y = 10^original))


###### CHECK MODEL RESIDUALS FOR EACH YEAR #######
par(mfrow = c(2, 3))  # 2 rows, 3 columns


qqnorm(residuals(aa13), main = "2013 QQ plot")
qqline(residuals(aa13))

qqnorm(residuals(aa14), main = "2014 QQ plot")
qqline(residuals(aa14))

qqnorm(residuals(aa15), main = "2015 QQ plot")
qqline(residuals(aa15))

qqnorm(residuals(aa24), main = "2024 QQ plot")
qqline(residuals(aa24))

qqnorm(residuals(aa25), main = "2025 QQ plot")
qqline(residuals(aa25))






#========================================================================================================================================#
##### PREDICT MANUAL CHL FROM HIGH-FREQUENCY SONDE DATA #####
# After fitting aa13, aa14, aa15, aa24, aa25 above, use each model to 
# predict manual chl across the full HF time series.
#========================================================================================================================================#

predict_manual_from_hf <- function(model, calibration_data, hf_data, yr) {
  
  # Pull the mean Hchl from the calibration data (same centering used during fitting)
  hchl_mean <- mean(calibration_data$Hchl)
  mchl_mean <- mean(calibration_data$Mchl)
  
  # Subset HF data to this year and compute the same log transform
  hf_yr <- hf_data %>%
    filter(Year == yr) %>%
    mutate(Hchl = log10(Chl_HYLB + 2)) %>%   # same transform as training
    mutate(Hchl_c = Hchl - hchl_mean)          # center using calibration mean
  
  # forecast() needs xreg as a matrix
  xreg_new <- matrix(hf_yr$Hchl_c, ncol = 1)
  
  # Predict: h = number of new HF time steps
  preds <- forecast(model, xreg = xreg_new, h = nrow(hf_yr))
  
  # Back-transform: add calibration mean back, then 10^x
  hf_yr %>%
    mutate(
      predicted_lman  = as.numeric(preds$mean) + mchl_mean,   # log10 scale
      predicted_manual = 10^predicted_lman,                    # original scale
      predicted_lower  = 10^(as.numeric(preds$lower[, 2]) + mchl_mean),  # 95% CI lower
      predicted_upper  = 10^(as.numeric(preds$upper[, 2]) + mchl_mean)   # 95% CI upper
    ) %>%
    select(Year, DoY, Chl_HYLB, predicted_manual, predicted_lower, predicted_upper)
}

# Apply to each year using its fitted model and calibration subset
hf_predicted <- bind_rows(
  predict_manual_from_hf(aa13, subset(datall, year == 2013), all.hylb, 2013),
  predict_manual_from_hf(aa14, subset(datall, year == 2014), all.hylb, 2014),
  predict_manual_from_hf(aa15, subset(datall, year == 2015), all.hylb, 2015),
  predict_manual_from_hf(aa24, subset(datall, year == 2024), all.hylb, 2024),
  predict_manual_from_hf(aa25, subset(datall, year == 2025), all.hylb, 2025)
)

# Quick sanity check plot
ggplot(hf_predicted, aes(x = DoY)) +
  # geom_ribbon(aes(ymin = predicted_lower, ymax = predicted_upper), alpha = 0.2, fill = "steelblue") +
  geom_line(aes(y = predicted_manual), color = "steelblue") +
  facet_wrap(~Year, scales = "free_y") +
  theme_classic() +
  labs(x = "Day of Year", 
       y = "Predicted Manual Chlorophyll (μg/L)",
       title = "Tuesday Lake: HF Sonde → Predicted Manual Chl (ARIMA)")

# Bring in the manual grab data (same source as before, already filtered)
manuals_for_plot <- man.pigs.morning %>%
  filter(!is.na(Manual_Chl)) %>%
  rename(Year = Year, DoY = roundDoY) %>%
  select(Year, DoY, Manual_Chl)

# Join to HF predictions
hf_predicted_with_manual <- hf_predicted %>%
  left_join(manuals_for_plot, by = c("Year", "DoY"))

# Plot predicted HF line + manual grab points
ggplot(hf_predicted_with_manual %>% filter(Year == 2015), aes(x = DoY)) +
  # geom_ribbon(aes(ymin = predicted_lower, ymax = predicted_upper), 
  #             alpha = 0.2, fill = "steelblue") +
  geom_line(aes(y = predicted_manual), color = "steelblue", linewidth = 0.7) +
  geom_point(aes(y = Manual_Chl), color = "firebrick", size = 1.5, na.rm = TRUE) +
  facet_wrap(~Year, scales = "free_y") +
  theme_classic() +
  labs(x = "Day of Year",
       y = "Chlorophyll (μg/L)",
       title = "Tuesday Lake: ARIMA-Predicted Manual Chl vs. Observed Manual Grabs",
       caption = "Blue line = HF predicted, red points = manual grabs") +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        strip.text = element_text(size = 12))


















#### combine all and plot with each other #####

predicted13 = predicted13 %>% mutate(predicted = as.numeric(predicted))
predicted14 = predicted14 %>% mutate(predicted = as.numeric(predicted))
predicted15 = predicted15 %>% mutate(predicted = as.numeric(predicted))
predicted24 = predicted24 %>% mutate(predicted = as.numeric(predicted))
predicted25 = predicted25 %>% mutate(predicted = as.numeric(predicted))

all.predicted = rbind(predicted13, predicted14, predicted15, predicted24, predicted25)

ggplot(all.predicted, aes(x = doy, y = 10^predicted, color = as.factor(year)))+
  geom_point()+
  geom_line()+
  theme_classic()



###### predict HF data as the difference needed to adjust the daily mean ######


datall = allchl.ct
datall$Mchl = log10(allchl.ct$Manual_Chl)
datall$Hchl = log10(allchl.ct$Chl_HYLB + 2)


correction.data = all.predicted %>% 
  left_join(datall %>% 
              select(lake, year, doy, Mchl, Hchl), by = c("doy", "year")) %>% 
  mutate(correction = predicted - Hchl) %>% 
  select(year, doy, correction, Mchl)




# read in the sonde data
#chl.sonde = read.csv("./data/formatted data/HF data/Sonde correction/Predicted Tuesday HYLB on Manual Scale log-trans NOISY.csv")

all.hylb = read.csv("./data/formatted data/HF data/Tuesday HYLB 2013-2015 2024 2025 log-trans NEW MARSS NOISE 2026-02-16.csv")

hf15 = all.hylb %>% filter(Year == 2015)

hf15 <- all.hylb %>%
  filter(Year == 2015 & DoY > 150)

hf.X <- log10(hf15$Chl_HYLB + 2)
hf.X = hf.X - mean(hf.X)

newxreg <- matrix(hf.X, ncol = 1)

hf.pred <- predict(aa15, newxreg = newxreg)


hf_out <- hf15 %>%
  mutate(
    pred_manual_log = hf.pred$pred + mean(dat15$Mchl),
    pred_manual = 10^pred_manual_log,
    DoY = hf15$DoY
  )

pred.df <- data.frame(
  DoY = hf15$DoY,
  pred_manual = 10^(as.numeric(pred$pred) + mean(dat15$Mchl))
)

ggplot(hf_out, aes(x = DoY, y = pred_manual))+
  geom_line()+
  geom_point(
    data = datall %>% filter(year == 2015),
    aes(x = doy, y = Manual_Chl),
    color = "red",
    size = 2
  )

newxreg <- matrix(hf.X, ncol = ncol(aa15$xreg))

pred <- predict(aa15, newxreg = newxreg)




hf.X <- log10(hf15$Chl_HYLB + 2) - mean(dat15$Hchl)

newxreg <- matrix(hf.X, ncol = 1)

pred <- predict(aa15, newxreg = newxreg)

pred.df <- data.frame(
  DoY = hf15$DoY,
  pred_manual = 10^(pred$pred + mean(dat15$Mchl))
)


pred = data.frame(pred)

# need year to be a factor
correction.data = correction.data %>% 
  mutate(year = as.factor(year))

# correct the sonde data by adding the correction by day
all.hylb = all.hylb %>% 
  rename(year = Year, lake = Lake) %>% 
  mutate(year = as.factor(year)) %>% 
  mutate(doy = round(DoY)) %>% 
  left_join(correction.data, by = c("year", "doy")) %>% 
  mutate(corrected.sonde = correction + log10(Chl_HYLB+2)) # run on transformed data that matches the model fit

# plot the corrected and Manual chlorophyll to see how well correction worked
ggplot(all.hylb, aes(x = DoY, y = corrected.sonde, color = as.factor(year)))+
  geom_line()+
  facet_wrap(~year)+
  geom_point(data = all.hylb, aes(x = DoY, y = Mchl), color = "black")+
  theme_bw()+
  labs(x = "DOY", y = "log10(Chlorophyll)")

# get daily mean of the corrected data
mean.corrected = all.hylb %>% 
  group_by(year, doy) %>% 
  summarize(mean.chl = mean(corrected.sonde, na.rm = TRUE),
            mean.manual = mean(Mchl))

# plot daily mean of corrected along with manual as time series
ggplot(mean.corrected, aes(x = doy, y = 10^mean.chl))+
  geom_line(data = mean.corrected, aes(x = doy, y = 10^mean.manual),  color = "blue", size = 1.5)+
  geom_point(color = "red")+
  facet_wrap(~year)
  

ggplot(mean.corrected, aes(x = mean.manual, y = mean.chl)) +
  geom_point() +
  facet_wrap(~year)+
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  theme_classic()


# first rename to match the original dataframe
# and make a datetime column

final.T = all.hylb %>% 
  mutate(
    datetime = make_datetime(as.numeric(as.character(year)), 1, 1, tz = "UTC") +
      ddays(DoY - 1)) %>% 
  rename(Year = year, Lake = lake, lsonde_cal = corrected.sonde) %>% 
  mutate(Chl_HYLB_cal = 10^lsonde_cal -2) %>%  # create original data column
  select(Year, Lake, DoY, Chl_HYLB, Chl_logged_HYLB, datetime, lsonde_cal, Chl_HYLB_cal)

# Chl_HYLB is original HYLB data
# Chl_logged_HYLB is log(Chl_HYLB + 2)
# lsonde_cal is the calibrated log(x+ 2) data
# Chl_HYLB_cal is the back-transformed data. No bias correction applied

# finally, remove NA values at the beginning and end of years
# these NA values occur because the sonde time series are longer than the manual time series

# verify that there are only NA values at the beginning and end of years
na_gaps <- final.T %>%
  arrange(Year, datetime) %>%
  group_by(Year) %>%
  mutate(
    is_na = is.na(lsonde_cal),
    gap_id = cumsum(is_na & !lag(is_na, default = FALSE))
  ) %>%
  filter(is_na) %>%
  group_by(Year, gap_id) %>%
  summarize(
    gap_start = min(datetime),
    gap_end   = max(datetime),
    n_missing_points = n(),
    gap_hours = n_missing_points * 5 / 60,
    .groups = "drop"
  )

# remove them
final.T = final.T %>% 
  filter(!is.na(lsonde_cal))




### save the Tuesday dataframe
write.csv(final.T, "./data/formatted data/HF data/Predicted Tuesday HYLB on Manual Scale log-trans NOISY ARIMA.csv", row.names = FALSE)

#========================================================================================================================================#
###### PAUL #######

hf = read.csv("./data/formatted data/HF data/Paul HYLB 2013-2015 2024 2025 log-trans NEW MARSS NOISE 2026-02-25.csv") %>% 
  filter(DoY < 244)

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

# take the mean of the sonde pigments in the morning (6AM - 8AM)
# this is so we can compare to the morning manual grabs and avoid photoquenching
morning.pigs = hf %>% 
  filter(format(datetime, "%H") %in% c("06", "07", "08", "09")) %>% 
  mutate(roundDoY = round(DoY)) %>% 
  select(Lake, Year, roundDoY, DoY, Chl_HYLB) %>% 
  group_by(Lake, Year, roundDoY) %>% 
  summarize(Chl_HYLB = mean((Chl_HYLB), na.rm = TRUE)) %>% 
  mutate(Lake = "L")

# read in the manual pigments and combine with morning average sonde
man.pigs.morning = read.csv("./data/formatted data/simulation model inputs 2013-2015 2024 2025 v4.csv") %>% 
  rename(roundDoY = DOY) %>% 
  select(Lake, Year, roundDoY, Manual_Chl) %>% 
  left_join(morning.pigs, by = c("Lake", "Year", "roundDoY")) %>% 
  filter(Lake == "L") %>% 
  mutate(lman = log10(Manual_Chl), lsonde = log10(Chl_HYLB + 2)) %>% 
  mutate(
    Manual_Chl = case_when(
      Year == 2013 & (roundDoY >= 245 | roundDoY <= 145) ~  NA, #### remove periods of time at beginning and end of season that were interpolated for simulation model
      Year == 2014 & (roundDoY >= 242 | roundDoY <= 145)  ~ NA,
      Year == 2015 & (roundDoY >= 247 | roundDoY <= 144)  ~ NA,
      Year == 2024 & (roundDoY >= 240 | roundDoY <= 147)  ~ NA,
      Year == 2025 & (roundDoY >= 235 | roundDoY <= 145)  ~ NA,
      TRUE ~ Manual_Chl)) 

# plot the manuals
ggplot(man.pigs.morning, aes(x = roundDoY, y = Manual_Chl, color = as.factor(Year)))+
  geom_line(size = 0.7)+
  geom_point(size = 1)+
  theme_classic()+
  facet_wrap(~Year)

# plot log of the manuals vs log of the sonde
ggplot(man.pigs.morning %>% filter(lsonde < 1), aes(x = lsonde, y = log10(Manual_Chl)))+
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

# plot manual vs sonde all in one panel
ggplot(man.pigs.morning, aes(x = lsonde, y = log10(Manual_Chl), color = as.factor(Year)))+
  #geom_line(size = 0.7)+
  geom_point(size = 1)+
  theme_classic()+
  geom_smooth(method = "lm", se = FALSE)
#facet_wrap(~Year)

# remove NA values, which are only at the beginning and end of years
allchl.ct = man.pigs.morning %>% 
  filter(!is.na(Manual_Chl) & !is.na(Chl_HYLB)) %>% 
  rename(doy = roundDoY, lake = Lake, year = Year)


# read in the high frequency data again for prediction later on
all.hylb = read.csv("./data/formatted data/HF data/Paul HYLB 2013-2015 2024 2025 log-trans NEW MARSS NOISE 2026-02-25.csv")

# convert DoY back to datetime
all.hylb <- all.hylb %>%
  mutate(
    datetime = make_datetime(Year, 1, 1, tz = "UTC") +
      ddays(DoY - 1)
  )

print('the dataframe has zero NA values',quote=F)
print(allchl.ct[1,])
print(dim(allchl.ct))
print(dim(na.omit(allchl.ct)))

# transform chl
print(c('range Manual Chl ',range(allchl.ct$Manual_Chl)),quote=F)
print(c('range Sonde Chl ',range(allchl.ct$Chl_HYLB)),quote=F)

### rename to datall
# log-transform the pigment columns for closer to normal model residuals
datall = allchl.ct
datall$Mchl = log10(allchl.ct$Manual_Chl)
datall$Hchl = log10(allchl.ct$Chl_HYLB + 2)



## 2013 ##
# Try auto.arima on 2013 
print('auto.arima for 2013',quote=F)
dat13 = subset(datall,subset=(year == 2013))
X.c = dat13$Hchl - mean(dat13$Hchl)  # center Hchl
Y.c = dat13$Mchl - mean(dat13$Mchl)  # center Mchl

# fit the model
aa13 = auto.arima(y=Y.c,ic='aicc',xreg=X.c,
                  stepwise=F,test='adf',parallel=F)
print(summary(aa13))
print('correlation of y and yhat',quote=F)
print(cor(Y.c,aa13$fitted))

# dataframe of model predicted and original values
# with the mean added back in
predicted13 = data.frame(
  year = 2013,
  doy = dat13$doy,
  predicted = aa13$fitted + mean(dat13$Mchl),
  original = Y.c + mean(dat13$Mchl))

# plot the results
ggplot(predicted13, aes(x = original, y = as.numeric(predicted)))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)+
  stat_poly_eq(
    formula = y ~ x,
    aes(label = ..rr.label..),
    parse = TRUE,
    size = 4,
    label.x = 0.05,  
    label.y = 0.95)

# plot as time series
ggplot(predicted13, aes(x = doy, y = 10^as.numeric(predicted)))+
  geom_point()+
  geom_line(data = predicted13, aes(x = doy, y = 10^original))





## 2014 ##
# Try auto.arima on 2014 
print('auto.arima for 2014',quote=F)
dat14 = subset(datall,subset=(year == 2014))
X.c = dat14$Hchl - mean(dat14$Hchl)  # center Hchl
Y.c = dat14$Mchl - mean(dat14$Mchl)  # center Mchl

# fit the model
aa14 = auto.arima(y=Y.c,ic='aicc',xreg=X.c,
                  stepwise=F,test='adf',parallel=F)

print(summary(aa14))
print('correlation of y and yhat',quote=F)
print(cor(Y.c,aa14$fitted))

# make dataframe of fitted and original values
predicted14 = data.frame(
  year = 2014,
  doy = dat14$doy,
  predicted = aa14$fitted + mean(dat14$Mchl),
  original = Y.c + mean(dat14$Mchl))

# plot the corrected data vs original
ggplot(predicted14 %>% filter(original < 0.8 & predicted < 0.5), aes(x = original, y = as.numeric(predicted)))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)+
  stat_poly_eq(
    formula = y ~ x,
    aes(label = ..rr.label..),
    parse = TRUE,
    size = 4,
    label.x = 0.05,  
    label.y = 0.05)

ggplot(predicted14, aes(x = doy, y = 10^as.numeric(predicted)))+
  geom_point()+
  geom_line(data = predicted14, aes(x = doy, y = 10^original))





## 2015 ##
# Try auto.arima on 2015 
print('auto.arima for 2015',quote=F)
dat15 = subset(datall,subset=(year == 2015))
X.c = dat15$Hchl - mean(dat15$Hchl)  # center Hchl
Y.c = dat15$Mchl - mean(dat15$Mchl)  # center Mchl
aa15 = auto.arima(y=Y.c,ic='aicc',xreg=X.c,
                  stepwise=F,test='adf',parallel=F)
print(summary(aa15))
print('correlation of y and yhat',quote=F)
print(cor(Y.c,aa15$fitted))

predicted15 = data.frame(
  year = 2015,
  doy = dat15$doy,
  predicted = aa15$fitted + mean(dat15$Mchl),
  original = Y.c + mean(dat15$Mchl))

ggplot(predicted15, aes(x = original, y = as.numeric(predicted)))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)+
  geom_smooth(method = "lm", se = FALSE)+
  stat_poly_eq(
    formula = y ~ x,
    aes(label = ..rr.label..),
    parse = TRUE,
    size = 4,
    label.x = 0.05,  
    label.y = 0.05)

ggplot(predicted15, aes(x = doy, y = 10^as.numeric(predicted)))+
  geom_point()+
  geom_line(data = predicted15, aes(x = doy, y = 10^original))



## 2024 ##
# Try auto.arima on 2024 
print('auto.arima for 2024',quote=F)
dat24 = subset(datall,subset=(year == 2024))
X.c = dat24$Hchl - mean(dat24$Hchl)  # center Hchl
Y.c = dat24$Mchl - mean(dat24$Mchl)  # center Mchl

# fit the model
aa24 = auto.arima(y=Y.c,ic='aicc',xreg=X.c,
                  stepwise=F,test='adf',parallel=F)

print(summary(aa24))
print('correlation of y and yhat',quote=F)
print(cor(Y.c,aa24$fitted))

# make dataframe of fitted and original values
predicted24 = data.frame(
  year = 2024,
  doy = dat24$doy,
  predicted = aa24$fitted + mean(dat24$Mchl),
  original = Y.c + mean(dat24$Mchl))

ggplot(predicted24, aes(x = original, y = as.numeric(predicted)))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)+
  geom_smooth(method = "lm", se = FALSE)+
  stat_poly_eq(
    formula = y ~ x,
    aes(label = ..rr.label..),
    parse = TRUE,
    size = 4,
    label.x = 0.05,  
    label.y = 0.05)

ggplot(predicted24, aes(x = doy, y = 10^as.numeric(predicted)))+
  geom_line(color = "red", size = 1)+
  geom_line(data = predicted24, aes(x = doy, y = 10^original))




## 2025 ##
# Try auto.arima on 2025 
print('auto.arima for 2025',quote=F)
dat25 = subset(datall,subset=(year == 2025))
X.c = dat25$Hchl - mean(dat25$Hchl)  # center Hchl
Y.c = dat25$Mchl - mean(dat25$Mchl)  # center Mchl

# fit the model
aa25 = auto.arima(y=Y.c,ic='aicc',xreg=X.c,
                  stepwise=F,test='adf',parallel=F)
print(summary(aa25))
print('correlation of y and yhat',quote=F)
print(cor(Y.c,aa25$fitted))

# dataframe of fitted and original
predicted25 = data.frame(
  year = 2025,
  doy = dat25$doy,
  predicted = aa25$fitted + mean(dat25$Mchl),
  original = Y.c + mean(dat25$Mchl))

# plot original and corrected
ggplot(predicted25, aes(x = original, y = as.numeric(predicted)))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)+
  geom_smooth(method = "lm", se = FALSE)+
  stat_poly_eq(
    formula = y ~ x,
    aes(label = ..rr.label..),
    parse = TRUE,
    size = 4,
    label.x = 0.05,  
    label.y = 0.05)

ggplot(predicted25, aes(x = doy, y = 10^as.numeric(predicted)))+
  geom_point()+
  geom_line(data = predicted25, aes(x = doy, y = 10^original))


###### CHECK MODEL RESIDUALS FOR EACH YEAR #######
par(mfrow = c(2, 3))  # 2 rows, 3 columns


qqnorm(residuals(aa13), main = "2013 QQ plot")
qqline(residuals(aa13))

qqnorm(residuals(aa14), main = "2014 QQ plot")
qqline(residuals(aa14))

qqnorm(residuals(aa15), main = "2015 QQ plot")
qqline(residuals(aa15))

qqnorm(residuals(aa24), main = "2024 QQ plot")
qqline(residuals(aa24))

qqnorm(residuals(aa25), main = "2025 QQ plot")
qqline(residuals(aa25))


#### combine all and plot with each other #####

predicted13 = predicted13 %>% mutate(predicted = as.numeric(predicted))
predicted14 = predicted14 %>% mutate(predicted = as.numeric(predicted))
predicted15 = predicted15 %>% mutate(predicted = as.numeric(predicted))
predicted24 = predicted24 %>% mutate(predicted = as.numeric(predicted))
predicted25 = predicted25 %>% mutate(predicted = as.numeric(predicted))

all.predicted = rbind(predicted13, predicted14, predicted15, predicted24, predicted25)

ggplot(all.predicted, aes(x = doy, y = 10^predicted, color = as.factor(year)))+
  geom_point()+
  geom_line()+
  theme_classic()



###### predict HF data as the difference needed to adjust the daily mean ######


datall = allchl.ct
datall$Mchl = log10(allchl.ct$Manual_Chl)
datall$Hchl = log10(allchl.ct$Chl_HYLB + 2)


correction.data = all.predicted %>% 
  left_join(datall %>% 
              select(lake, year, doy, Mchl, Hchl), by = c("doy", "year")) %>% 
  mutate(correction = predicted - Hchl) %>% 
  select(year, doy, correction, Mchl)




# read in the sonde data
#chl.sonde = read.csv("./data/formatted data/HF data/Sonde correction/Predicted Paul HYLB on Manual Scale log-trans NOISY.csv")

all.hylb = read.csv("./data/formatted data/HF data/Paul HYLB 2013-2015 2024 2025 log-trans NEW MARSS NOISE 2026-02-25.csv")

# need year to be a factor
correction.data = correction.data %>% 
  mutate(year = as.factor(year))

# correct the sonde data by adding the correction by day
all.hylb = all.hylb %>% 
  rename(year = Year, lake = Lake) %>% 
  mutate(year = as.factor(year)) %>% 
  mutate(doy = round(DoY)) %>% 
  left_join(correction.data, by = c("year", "doy")) %>% 
  mutate(corrected.sonde = correction + log10(Chl_HYLB + 2))

# plot the time series and Manual chlorophyll to see how well correction worked
ggplot(all.hylb, aes(x = DoY, y = corrected.sonde, color = as.factor(year)))+
  geom_line()+
  facet_wrap(~year)+
  geom_point(data = all.hylb, aes(x = DoY, y = Mchl), color = "black")+
  theme_bw()+
  labs(x = "DOY", y = "log10(Chlorophyll)")

# get daily mean of the corrected data
mean.corrected = all.hylb %>% 
  group_by(year, doy) %>% 
  summarize(mean.chl = mean(corrected.sonde, na.rm = TRUE),
            mean.manual = mean(Mchl))

# plot daily mean of corrected along with manual as time series
ggplot(mean.corrected, aes(x = doy, y = 10^mean.chl))+
  geom_line(data = mean.corrected, aes(x = doy, y = 10^mean.manual),  color = "blue", size = 1.5)+
  geom_point(color = "red")+
  facet_wrap(~year)


ggplot(mean.corrected, aes(x = mean.manual, y = mean.chl)) +
  geom_point() +
  facet_wrap(~year)+
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  theme_classic()


# first rename to match the original dataframe
# and make a datetime column

final.L = all.hylb %>% 
  mutate(
    datetime = make_datetime(as.numeric(as.character(year)), 1, 1, tz = "UTC") +
      ddays(DoY - 1)) %>% 
  rename(Year = year, Lake = lake, lsonde_cal = corrected.sonde) %>% 
  mutate(Chl_HYLB_cal = 10^lsonde_cal -2) %>%  # create original data column
  select(Year, Lake, DoY, Chl_HYLB, Chl_logged_HYLB, datetime, lsonde_cal, Chl_HYLB_cal)

# Chl_HYLB is original HYLB data
# Chl_logged_HYLB is log(Chl_HYLB + 2)
# lsonde_cal is the calibrated log(x+ 2) data
# Chl_HYLB_cal is the back-transformed data. No bias correction applied

# finally, remove NA values at the beginning and end of years
# these NA values occur because the sonde time series are longer than the manual time series

# verify that there are only NA values at the beginning and end of years
na_gaps <- final.L %>%
  arrange(Year, datetime) %>%
  group_by(Year) %>%
  mutate(
    is_na = is.na(lsonde_cal),
    gap_id = cumsum(is_na & !lag(is_na, default = FALSE))
  ) %>%
  filter(is_na) %>%
  group_by(Year, gap_id) %>%
  summarize(
    gap_start = min(datetime),
    gap_end   = max(datetime),
    n_missing_points = n(),
    gap_hours = n_missing_points * 5 / 60,
    .groups = "drop"
  )

# remove them
final.L = final.L %>% 
  filter(!is.na(lsonde_cal))


### save the Tuesday dataframe
write.csv(final.L, "./data/formatted data/HF data/Predicted Paul HYLB on Manual Scale log-trans NOISY ARIMA.csv", row.names = FALSE)

