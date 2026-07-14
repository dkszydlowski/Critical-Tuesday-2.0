
#### Figure 1 ####

library(tidyverse)
library(ggpubr)
library(ggh4x)
library(ggridges)



###### ===================================================================================================================############
#### make a new version of the figure with Tuesday on the left and Paul on the right


# read in the data
chl.sonde = read.csv("./data/formatted data/HF data/Predicted Tuesday HYLB on Manual Scale log-trans NOISY ARIMA.csv")

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


year_ranks <- morning.pigs %>%
  group_by(Year) %>%
  summarise(max_chl = max(Chl_HYLB, na.rm = TRUE)) %>%
  arrange(max_chl)  # lowest at bottom, highest at top



peak_chl <- morning.pigs %>%
  filter(Lake == "T",
         roundDoY >= 131,
         roundDoY <= 244) %>%
  group_by(Year) %>%
  slice_max(Chl_HYLB, n = 1, with_ties = FALSE) %>%
  ungroup() %>% 
  mutate(y_peak = as.numeric(Year) + Chl_HYLB * 0.07) %>% 
  mutate(roundDoY = roundDoY -5) %>% 
  mutate()
#mutate(roundDoY = replace(roundDoY, Year == 2025, 170))

morning.pigs$Date <- as.Date(morning.pigs$roundDoY - 1,
                             origin = paste0(morning.pigs$Year, "-01-01"))

morning.pigs.T = morning.pigs

sonde.chl.plot.T = ggplot(morning.pigs %>% filter(Lake == "T"),
                          aes(x = roundDoY, 
                              y = Year,     
                              height = Chl_HYLB, 
                              group = Year, 
                              fill = Year)) +
  geom_ridgeline(
    scale = 0.09,
    alpha = 1,
    color = "black",
    size = 0.3
  ) +
  scale_fill_manual(values = setNames((green_palette), sort(year_ranks$Year))) +
  theme_bw() +
  theme(
    legend.position = "none",
    panel.grid = element_blank()
  ) +
  geom_text(
    data = peak_chl,
    aes(
      x = roundDoY,
      y = y_peak,
      label = paste(round(Chl_HYLB, 0), "μg/L", sep = " ")
    ),
    inherit.aes = FALSE,
    nudge_y = 0,
    size = 5
  ) +
  labs(x = "Day of Year", 
       y = "",
       title = "Tuesday Lake Chlorophyll")+
  theme(axis.title = element_text(size = 16), axis.text = element_text(size = 14),
        plot.title = element_text(size = 18, hjust = 0.5))+
  xlim(131, 244)+
  scale_y_discrete(limits = as.character(c(2013:2015, 2024, 2025)))







# read in the data
chl.sonde = read.csv("./data/formatted data/HF data/Predicted Paul HYLB on Manual Scale log-trans NOISY ARIMA.csv")

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
  mutate(Lake = "L", Year = as.factor(Year))



peak_chl <- morning.pigs %>%
  filter(Lake == "L",
         roundDoY >= 131,
         roundDoY <= 244) %>%
  group_by(Year) %>%
  slice_max(Chl_HYLB, n = 1, with_ties = FALSE) %>%
  ungroup() %>% 
  mutate(y_peak = as.numeric(Year) + Chl_HYLB * 0.07) %>% 
  mutate(roundDoY = roundDoY -5) %>% 
  mutate()
#mutate(roundDoY = replace(roundDoY, Year == 2025, 170))

morning.pigs$Date <- as.Date(morning.pigs$roundDoY - 1,
                             origin = paste0(morning.pigs$Year, "-01-01"))



morning.pig.L = morning.pigs


sonde.chl.plot.L = ggplot(morning.pigs %>% filter(Lake == "L"),
                          aes(x = roundDoY, 
                              y = Year,     
                              height = Chl_HYLB, 
                              group = Year, 
                              fill = Year)) +
  geom_ridgeline(
    scale = 0.09,
    alpha = 1,
    color = "black",
    size = 0.3
  ) +
  scale_fill_manual(values = setNames((green_palette), sort(year_ranks$Year))) +
  theme_bw() +
  theme(
    legend.position = "none",
    panel.grid = element_blank()
  ) +
  geom_text(
    data = peak_chl,
    aes(
      x = roundDoY,
      y = y_peak,
      label = paste(round(Chl_HYLB, 0), "μg/L", sep = " ")
    ),
    inherit.aes = FALSE,
    nudge_y = 0,
    size = 5
  ) +
  labs(x = "Day of Year", 
       y = "")+
  theme(axis.title = element_text(size = 16), axis.text = element_text(size = 14),
        plot.title = element_text(size = 18, hjust = 0.5))+
  xlim(131, 244)+
  scale_y_discrete(limits = as.character(c(2013:2015, 2024, 2025)))








### both with facet_wrap
morning.pigs.both = rbind(morning.pigs.T, morning.pig.L)

morning.pigs.both$Lake <- factor(morning.pigs.both$Lake, levels = c("T", "L"))


green_palette <- c("#CBD4AC", "#b4c187", "#80914b", "#5a6b3a", "#496231")
brown_palette <- c("#5E4939", "#755A42", "#A37D55", "#B09269", "#D9C3A7")

# Reorder Lake so T is left, L is right (optional)
morning.pigs.both$Lake <- factor(morning.pigs.both$Lake, levels = c("L", "T"))

morning.pigs.both = morning.pigs.both %>% 
  mutate(Lake = factor(Lake, levels = c("T", "L")),
         fill_color = ifelse(Lake == "L", green_palette[1], as.character(Year)))



both.plotted = ggplot(morning.pigs.both,
                      aes(x = roundDoY, 
                          y = Year,     
                          height = Chl_HYLB, 
                          group = Year, 
                          fill = fill_color)) +
  facet_wrap2(~Lake, 
              strip = strip_themed(
                background_x = list(
                  element_rect(fill = brown_palette[2]),  # Tuesday (T) - matches kNC brown
                  element_rect(fill = "#44729C")   # Paul (L) - matches kNC blue
                  
                ),
                text_x = list(
                  element_text(color = "white", size = 12, face = "bold"),
                  element_text(color = "white", size = 12, face = "bold")
                )
              ),
              labeller = as_labeller(c("L" = "Paul Lake (reference)", "T" = "Tuesday Lake (experimental)"))) +
  geom_ridgeline(
    scale = 0.09,
    alpha = 1,
    color = "black",
    size = 0.3
  ) +
  scale_fill_manual(values = c(green_palette[1], green_palette)) +
  theme_bw() +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    strip.text = element_text(size = 16)
  ) +
  labs(x = "Date", y = "chlorophyll (μg/L)") +
  theme(
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    plot.margin = margin(l = 0, r = 1, t = 0, b = 1, unit = "pt")) +
  scale_y_discrete(limits = as.character(c(2013:2015, 2024, 2025))) +
  scale_x_continuous(
    breaks = c(152, 182, 213, 244),
    labels = c("Jun", "Jul", "Aug", "Sep"))




#### add in a plot of kNC for both #####

data = read.csv("./data/formatted data/simulation model inputs 2013-2015 2024 2025 v4.csv") %>% 
  mutate(kNC = kPAR - 0.0177*Manual_Chl) %>% 
  filter(Lake %in% (c("L", "T")))


data <- data %>%
  mutate(
    Lake = factor(Lake, levels = c("T", "L")),
    lake_year = paste(Lake, Year, sep = "_")
  )


fill_cols <- c(
  setNames(rep("#44729C", 5),
           paste0("L_", sort(unique(data$Year)))),
  setNames(brown_palette,
           paste0("T_", sort(unique(data$Year))))
)


knc.plot = ggplot(
  data %>% filter(kNC > 0),
  aes(
    x = as.factor(Year),
    y = kNC,
    fill = lake_year
  )
) +
  geom_boxplot(size = 0.4) +
  scale_fill_manual(values = fill_cols) +
  facet_wrap(~Lake) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(
    x = "Year",
    y = "non-chlorophyll\nlight attenuation (kNC)"
  ) +
  theme(
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    plot.title = element_text(size = 12),
    axis.text.x = element_text(size = 10),
    strip.text = element_blank(),
    plot.margin = margin(l = 0, r = 1, t = 1, b = 0, unit = "pt"),
    panel.grid = element_blank()
  )




ggarrange(both.plotted, knc.plot, nrow = 2, ncol = 1, heights = c(1, 1.3), align = "hv")


png("./figures/FIGURE 1 - chl time series and knc.png", height = 120, width = 173, units = "mm", res = 600)
ggarrange(both.plotted, knc.plot, nrow = 2, ncol = 1, align = "v")
dev.off()


#### plot of all of the data stacked #####

chl.sonde = read.csv("./data/formatted data/HF data/Sonde correction/Predicted Tuesday HYLB on Manual Scale log-trans NOISY.csv")

chl.sonde = chl.sonde %>% 
  arrange(datetime) %>% 
  mutate(datetime = ymd_hms(datetime))

png("./figures/DEFENSE/All chl stacked/")

chl.hourly <- chl.sonde %>%
  mutate(hour = floor_date(datetime, "hour")) %>%
  group_by(Year, hour) %>%
  slice(1) %>%
  ungroup()

ggplot(chl.hourly %>% filter(lsonde_cal > 0), aes(x = as.numeric(row.names(chl.hourly %>% filter(lsonde_cal > 0))), y = lsonde_cal))+
  geom_line()+
  geom_area(fill = "#6C752C")+
  theme_classic()



#### Plot Tuesday and Paul kNC by year

data = read.csv("./data/formatted data/simulation model inputs 2013-2015 2024 2025 v4.csv") %>% 
  mutate(kNC = kPAR - 0.0177*Manual_Chl) %>% 
  filter(Lake %in% (c("L", "T")))

mean.kNC = data %>% group_by(Lake, Year) %>% 
  filter(kNC > 0) %>% 
  summarize(mean.kNC  = mean(kNC))

ggplot(data %>% filter(Lake == "L" | Lake == "T" & kNC > 0), aes(x = as.factor(Year), y = kPAR, color = Lake))+
  geom_boxplot()

ggplot(data %>% filter(Lake == "L" | Lake == "T" & kNC > 0), aes(x =Lake, y = kNC, fill = Lake))+
  geom_boxplot()+
  geom_point(alpha = 0.1)+
  scale_fill_manual(values = c("L" = "lightblue", "T" = "#755A42"))

ggplot(
  data %>% filter(Lake %in% c("L", "T") & kNC > 0),
  aes(x = Lake, y = kNC, fill = Lake)) +
  geom_boxplot() +
  geom_point(alpha = 0.1) +
  scale_fill_manual(values = c("L" = "lightblue", "T" = "#755A42")) +
  geom_point(
    data = mean.kNC,
    aes(x = Lake, y = mean.kNC),
    color = "black",
    fill = "white",
    pch = 21,
    size = 5)+
  geom_text(
    data = mean.kNC,
    aes(x = Lake, y = mean.kNC, label = Year),
    nudge_x = 0.15,
    size = 3)+
  theme_classic()+
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))
