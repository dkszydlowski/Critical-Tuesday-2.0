# Danny's conversion 0f sonde chl to manual chl 2026-06-03

#rm(list = ls()) 
#graphics.off()

library(tidyverse)
library(forecast)
library(parallel)

options(mc.cores = parallel::detectCores())

# read in the chl data for correction
# Manual_Chl is the manual chlorophyll
# Chl_HYLB is the sonde chlorophyll
# lman is the log10-transformed manual chlorophyll
# lsonde is the log10-transformed sonde chlorophyll

allchl.ct = read.csv("./Tuesday chlorophyll data for calibration.csv")

print('the dataframe has zero NA values',quote=F)
print(allchl.ct[1,])
print(dim(allchl.ct))
print(dim(na.omit(allchl.ct)))

# transform chl
print(c('range Manual Chl ',range(allchl.ct$Manual_Chl)),quote=F)
print(c('range Sonde Chl ',range(allchl.ct$Chl_HYLB)),quote=F)
datall = allchl.ct
datall$Mchl = log10(allchl.ct$Manual_Chl)
datall$Hchl = log10(allchl.ct$Chl_HYLB)

# how many years?
uy = unique(datall$year)
print(c('unique years ',t(uy)),quote=F)

## 2013 ##
# Try auto.arima on 2013 
print('auto.arima for 2013',quote=F)
dat13 = subset(datall,subset=(year == 2013))
X.c = dat13$Hchl - mean(dat13$Hchl)  # center Hchl
Y.c = dat13$Mchl - mean(dat13$Mchl)  # center Mchl
aa13 = auto.arima(y=Y.c,ic='aicc',xreg=X.c,
                  stepwise=F,test='adf',parallel=F)
print(summary(aa13))
print('correlation of y and yhat',quote=F)
print(cor(Y.c,aa13$fitted))

fcst13 = forecast(aa13, level=c(0.9), xreg= X.c,bootstrap=F)

predicted13 = data.frame(
  year = 2013,
  doy = dat13$doy,
  predicted = aa13$fitted + mean(dat13$Mchl),
  original = Y.c + mean(dat13$Mchl))

ggplot(predicted13, aes(x = original, y = as.numeric(predicted)))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)

ggplot(predicted13, aes(x = doy, y = 10^as.numeric(predicted)))+
  geom_point()+
  geom_line(data = predicted13, aes(x = doy, y = 10^original))


## 2014 ##
# Try auto.arima on 2014 
print('auto.arima for 2014',quote=F)
dat14 = subset(datall,subset=(year == 2014))
X.c = dat14$Hchl - mean(dat14$Hchl)  # center Hchl
Y.c = dat14$Mchl - mean(dat14$Mchl)  # center Mchl
aa14 = auto.arima(y=Y.c,ic='aicc',xreg=X.c,
                  stepwise=F,test='adf',parallel=F)
print(summary(aa14))
print('correlation of y and yhat',quote=F)
print(cor(Y.c,aa14$fitted))

fcst14 = forecast(aa14, level=c(0.9), xreg= X.c,bootstrap=F)

predicted14 = data.frame(
  year = 2014,
  doy = dat14$doy,
  predicted = aa14$fitted + mean(dat14$Mchl),
  original = Y.c + mean(dat14$Mchl))

ggplot(predicted14, aes(x = original, y = as.numeric(predicted)))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)

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

fcst15 = forecast(aa15, level=c(0.9), xreg= X.c,bootstrap=F)

predicted15 = data.frame(
  year = 2015,
  doy = dat15$doy,
  predicted = aa15$fitted + mean(dat15$Mchl),
  original = Y.c + mean(dat15$Mchl))

ggplot(predicted15, aes(x = original, y = as.numeric(predicted)))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)

ggplot(predicted15, aes(x = doy, y = 10^as.numeric(predicted)))+
  geom_point()+
  geom_line(data = predicted15, aes(x = doy, y = 10^original))



## 2024 ##
# Try auto.arima on 2024 
print('auto.arima for 2024',quote=F)
dat24 = subset(datall,subset=(year == 2024))
X.c = dat24$Hchl - mean(dat24$Hchl)  # center Hchl
Y.c = dat24$Mchl - mean(dat24$Mchl)  # center Mchl
aa24 = auto.arima(y=Y.c,ic='aicc',xreg=X.c,
                  stepwise=F,test='adf',parallel=F)
print(summary(aa24))
print('correlation of y and yhat',quote=F)
print(cor(Y.c,aa24$fitted))

fcst24 = forecast(aa24, level=c(0.9), xreg= X.c,bootstrap=F)

predicted24 = data.frame(
  year = 2024,
  doy = dat24$doy,
  predicted = aa24$fitted + mean(dat24$Mchl),
  original = Y.c + mean(dat24$Mchl))

ggplot(predicted24, aes(x = original, y = as.numeric(predicted)))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)

ggplot(predicted24, aes(x = doy, y = 10^as.numeric(predicted)))+
  geom_point()+
  geom_line(data = predicted24, aes(x = doy, y = 10^original))




## 2025 ##
# Try auto.arima on 2025 
print('auto.arima for 2025',quote=F)
dat25 = subset(datall,subset=(year == 2025))
X.c = dat25$Hchl - mean(dat25$Hchl)  # center Hchl
Y.c = dat25$Mchl - mean(dat25$Mchl)  # center Mchl
aa25 = auto.arima(y=Y.c,ic='aicc',xreg=X.c,
                  stepwise=F,test='adf',parallel=F)
print(summary(aa25))
print('correlation of y and yhat',quote=F)
print(cor(Y.c,aa25$fitted))

fcst25 = forecast(aa25, level=c(0.9), xreg= X.c,bootstrap=F)

predicted25 = data.frame(
  year = 2025,
  doy = dat25$doy,
  predicted = aa25$fitted + mean(dat25$Mchl),
  original = Y.c + mean(dat25$Mchl))

ggplot(predicted25, aes(x = original, y = as.numeric(predicted)))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)

ggplot(predicted25, aes(x = doy, y = 10^as.numeric(predicted)))+
  geom_point()+
  geom_line(data = predicted25, aes(x = doy, y = 10^original))





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



### make a ggridges plot? Or compare to the original transformation somehow



ggplot(all.predicted,
                        aes(x = tindex, 
                            y = (year),     
                            height = 10^predicted, 
                            group = year, 
                            fill = year)) +
  geom_ridgeline(
    scale = 0.09,
    alpha = 1,
    color = "black",
    size = 0.3
  ) +
  # scale_fill_manual(values = setNames((green_palette), sort(year_ranks$Year))) +
  # theme_bw() +
  # theme(
  #   legend.position = "none",
  #   panel.grid = element_blank()
  # ) +
  # geom_text(
  #   data = peak_chl,
  #   aes(
  #     x = tindex,
  #     y = y_peak,
  #     label = paste(round(Chl_HYLB, 0), "μg/L", sep = " ")
  #   ),
  #   inherit.aes = FALSE,
  #   nudge_y = 0,
  #   size = 5
  # ) +
  labs(x = "Day of Year", 
       y = "",
       title = "Tuesday Lake Chlorophyll")+
  theme(axis.title = element_text(size = 16), axis.text = element_text(size = 14),
        plot.title = element_text(size = 18, hjust = 0.5))+
  xlim(131, 244)



library(ggridges)

ggplot(all.predicted,
       aes(x = tindex,
           y = factor(year),
           height = predicted,
           fill = factor(year))) +
  geom_ridgeline(
    scale = 0.09,
    alpha = 1,
    color = "black",
    linewidth = 0.3
  ) +
  labs(
    x = "Day of Year",
    y = NULL,
    title = "Tuesday Lake Chlorophyll"
  ) +
  coord_cartesian(xlim = c(131, 244)) +
  theme_classic() +
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    plot.title = element_text(size = 18, hjust = 0.5)
  )





#### compare original regression data with new data ####

# read in the data
chl.sonde = read.csv("./data/formatted data/HF data/Sonde correction/Predicted Tuesday HYLB on Manual Scale log-trans NOISY.csv")

# calculate morning average
chl.sonde <- chl.sonde %>%
  mutate(
    datetime = make_datetime(Year, 1, 1, tz = "UTC") +
      ddays(DoY - 1)
  )

# take the mean of the sonde pigments in the morning
morning.pigs = chl.sonde %>% 
  filter(format(datetime, "%H") %in% c("06", "07", "08", "09")) %>% 
  mutate(roundDoY = round(DoY)) %>% 
  select(Lake, Year, roundDoY, DoY, Chl_HYLB_cal) %>% 
  group_by(Lake, Year, roundDoY) %>% 
  summarize(Chl_HYLB = mean((Chl_HYLB_cal), na.rm = TRUE)) %>% 
  mutate(Lake = "T", Year = as.factor(Year))



all.predicted = all.predicted %>% 
  rename(Year = year)

all.predicted = all.predicted %>% 
  rename(roundDoY = doy)

all.predicted = all.predicted %>% 
  mutate(Year = as.factor(Year))


compare.pigs = morning.pigs %>% 
  left_join(all.predicted, by = c("Year", "roundDoY"))


ggplot(compare.pigs, aes(x = roundDoY, y = (Chl_HYLB)))+
  geom_line(color = "blue", size = 0.8)+
  facet_wrap(~Year)+
  labs(y = "Chlorophyll")+
  geom_line(data = compare.pigs, aes(x = roundDoY, y = 10^predicted), color = "red", size = 0.8, alpha = 0.5)
  #geom_point(data = compare.pigs, aes(x = roundDoY, y = original))


ggplot(compare.pigs, aes(x = roundDoY, y = original))+
  geom_point()+
  facet_wrap(~Year)+
  geom_line(data = compare.pigs, aes(x = roundDoY, y = predicted))+
  labs(x = "DoY", y = "log10(Chlorophyll)")

