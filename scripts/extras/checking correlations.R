#### check chlorophyll against kNC ####

library(tidyverse)
library(ggpmisc)
library(ggpubr)
library(ggh4x)



## read in the manual chlorophyll and kNC time series ##

data = read.csv("./data/formatted data/simulation model inputs 2013-2015 2024 2025 v4.csv")

# Tuesday
man.pigs.morning.T = read.csv("./data/formatted data/simulation model inputs 2013-2015 2024 2025 v4.csv") %>% 
  rename(roundDoY = DOY) %>% 
  select(Lake, Year, roundDoY, Manual_Chl, kPAR) %>% 
  filter(Lake == "T") %>% 
  mutate(
    Manual_Chl = case_when(
      Year == 2013 & (roundDoY >= 245 | roundDoY <= 145) ~  NA, #### remove periods of time at beginning and end of season that were interpolated for simulation model
      Year == 2014 & (roundDoY >= 242 | roundDoY <= 145)  ~ NA,
      Year == 2015 & (roundDoY >= 247 | roundDoY <= 144)  ~ NA,
      Year == 2024 & (roundDoY >= 240 | roundDoY <= 147)  ~ NA,
      Year == 2025 & (roundDoY >= 235 | roundDoY <= 145)  ~ NA,
      TRUE ~ Manual_Chl))  %>% 
  mutate(kNC = kPAR - 0.0177*Manual_Chl)


### get yearly averages
avg.pigs.T = man.pigs.morning.T %>% 
  group_by(Lake, Year) %>% 
  summarize(mean.kNC = mean(kNC, na.rm = TRUE), mean.chl = mean(Manual_Chl, na.rm = TRUE), mean.kPAR = mean(kPAR, na.rm = TRUE))

T.kNC = ggplot(avg.pigs.T, aes(x = mean.chl, y = mean.kNC))+
  geom_point(size = 3, color = "#755A42")+
  labs(x = "Mean chlorophyll (ug/L)", y = "mean kNC (m-1)")+
  theme_bw()+
  geom_smooth(method = "lm", se = FALSE, color = "#755A42", linetype = "dashed")+
  stat_poly_eq(
    aes(label = paste(..rr.label..)),
    formula = y ~ x,
    parse = TRUE,
    geom = "text",
    label.x = 31,
    label.y = "top",
    color = "black",
    size = 3
  )


T.kPAR = ggplot(avg.pigs.T, aes(x = mean.chl, y = mean.kPAR))+
  geom_point(size = 3, color = "#755A42")+
  labs(x = "Mean chlorophyll (ug/L)", y = "mean kPAR (m-1)")+
  theme_bw()+
  geom_smooth(method = "lm", se = FALSE, color = "#755A42", linetype = "dashed")+
  stat_poly_eq(
    aes(label = paste(..rr.label..)),
    formula = y ~ x,
    parse = TRUE,
    geom = "text",
    label.x = 31,
    label.y = "top",
    color = "black",
    size = 3
  )


# Paul
man.pigs.morning.L = read.csv("./data/formatted data/simulation model inputs 2013-2015 2024 2025 v4.csv") %>% 
  rename(roundDoY = DOY) %>% 
  select(Lake, Year, roundDoY, Manual_Chl, kPAR) %>% 
  filter(Lake == "L") %>% 
  mutate(
    Manual_Chl = case_when(
      Year == 2013 & (roundDoY >= 245 | roundDoY <= 145) ~  NA, #### remove periods of time at beginning and end of season that were interpolated for simulation model
      Year == 2014 & (roundDoY >= 242 | roundDoY <= 145)  ~ NA,
      Year == 2015 & (roundDoY >= 247 | roundDoY <= 144)  ~ NA,
      Year == 2024 & (roundDoY >= 240 | roundDoY <= 147)  ~ NA,
      Year == 2025 & (roundDoY >= 235 | roundDoY <= 145)  ~ NA,
      TRUE ~ Manual_Chl))  %>% 
  mutate(kNC = kPAR - 0.0177*Manual_Chl)


### get yearly averages
avg.pigs.L = man.pigs.morning.L %>% 
  group_by(Lake, Year) %>% 
  summarize(mean.kNC = mean(kNC, na.rm = TRUE), mean.chl = mean(Manual_Chl, na.rm = TRUE), mean.kPAR = mean(kPAR, na.rm = TRUE))

L.kNC = ggplot(avg.pigs.L, aes(x = mean.chl, y = mean.kNC))+
  geom_point(size = 3, color = "#44729C")+
  labs(x = "Mean chlorophyll (ug/L)", y = "mean kNC (m-1)")+
  theme_bw()+
  geom_smooth(method = "lm", se = FALSE, color = "#44729C", linetype = "dashed")+
  stat_poly_eq(
    aes(label = paste(..rr.label..)),
    formula = y ~ x,
    parse = TRUE,
    geom = "text",
    label.x = 4.2,
    label.y = "top",
    color = "black",
    size = 3
  )


L.kPAR = ggplot(avg.pigs.L, aes(x = mean.chl, y = mean.kPAR))+
  geom_point(size = 3, color = "#44729C")+
  labs(x = "Mean chlorophyll (ug/L)", y = "mean kPAR (m-1)")+
  theme_bw()+
  geom_smooth(method = "lm", se = FALSE, color = "#44729C", linetype = "dashed")+
  stat_poly_eq(
    aes(label = paste(..rr.label..)),
    formula = y ~ x,
    parse = TRUE,
    geom = "text",
    label.x = 4.2,
    label.y = "top",
    color = "black",
    size = 3
  )


ggarrange(T.kNC, T.kPAR, L.kNC, L.kPAR)





#### CHECK WEEKLY CORRELATIONS ####
# yearly averages are not resolved enough to get an effect


data = read.csv("./data/formatted data/simulation model inputs 2013-2015 2024 2025 v4.csv") %>% 
  filter(Lake %in% c("L", "T") & !is.na(Ztherm)) %>% 
  mutate(kNC = kPAR - 0.0177*Manual_Chl)


ggplot(data %>% filter(!is.na(kPAR)), aes(x = DOC, y = kPAR))+
  geom_point()+
  facet_wrap(~Lake, scales = "free")+
  geom_smooth(method = "lm", se = FALSE, color = "#44729C", linetype = "dashed")+
  stat_poly_eq(
    aes(label = paste(..rr.label..)),
    formula = y ~ x,
    parse = TRUE,
    geom = "text",
    label.x = 4.2,
    label.y = "top",
    color = "black",
    size = 3
  )




ggplot(data %>% filter(!is.na(kPAR)), aes(x = Manual_Chl, y = kPAR))+
  geom_point()+
  facet_wrap(~Lake, scales = "free")+
  geom_smooth(method = "lm", se = FALSE, color = "#44729C", linetype = "dashed")+
  stat_poly_eq(
    aes(label = paste(..rr.label..)),
    formula = y ~ x,
    parse = TRUE,
    geom = "text",
    label.x = 4.2,
    label.y = "top",
    color = "black",
    size = 3
  )


ggplot(data %>% filter(!is.na(kPAR)), aes(x = DOC, y = kPAR))+
  geom_point()+
  facet_wrap(~Lake, scales = "free")+
  geom_smooth(method = "lm", se = FALSE, color = "#44729C", linetype = "dashed")+
  stat_poly_eq(
    aes(label = paste(..rr.label..)),
    formula = y ~ x,
    parse = TRUE,
    geom = "text",
    label.x = 4.2,
    label.y = "top",
    color = "black",
    size = 3
  )







#### compare kPAR to color, g440 #####

# read in the color data

color = read.csv("./data/formatted data/cascade_carbon_v05.csv") %>% 
  filter(lakeid %in% c("L", "T"), year4 %in% c(2013, 2014, 2015, 2024, 2025) & depth == "PML") %>% 
  select(lakeid, year4, daynum, absorbance) %>% 
  rename(Lake = lakeid, DOY = daynum, Year = year4)

data = data %>% 
  left_join(color, by = c("Lake", "Year", "DOY"))

# calculate g440 and re-level
data = data %>% 
  mutate(g440 = 2.303*absorbance/0.1) %>% 
  mutate(Lake = factor(Lake, levels = c("T", "L")))

# compare to color
comp.color = ggplot(data %>% filter(!is.na(kPAR)), aes(x = g440, y = kPAR, color = Lake)) +
  geom_point(size = 1.5) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  stat_poly_eq(
    formula = y ~ x,
    aes(label = ..rr.label..),
    parse = TRUE,
    size = 4,
    label.x = 0.05,
    label.y = 0.95,
    color = "black"
  ) +
  theme_bw() +
  scale_color_manual(values = c("L" = "#44729C", "T" = "#755A42")) +
  facet_wrap2(
    ~ Lake,
    scales = "free",
    strip = strip_themed(
      background_x = elem_list_rect(
        fill = c(
          "T" = "#755A42",
          "L" = "#44729C"
        ),
        colour = NA
      ),
      text_x = elem_list_text(
        color = "white",
        size = 10
      )
    ),
    labeller = labeller(
      Lake = c(
        "T" = "Tuesday (experimental)",
        "L" = "Paul (reference)"
      )
    )
  ) +
  labs(x = "g440 (m-1)", y = "kPAR") +
  theme(legend.position = "none")



# compare to chl

comp.chl = ggplot(data %>% filter(!is.na(kPAR)), aes(x = Manual_Chl, y = kPAR, color = Lake)) +
  geom_point(size = 1.5) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  stat_poly_eq(
    formula = y ~ x,
    aes(label = ..rr.label..),
    parse = TRUE,
    size = 4,
    label.x = 0.95,
    label.y = 0.95,
    color = "black"
  ) +
  theme_bw() +
  scale_color_manual(values = c("L" = "#44729C", "T" = "#755A42")) +
  facet_wrap2(
    ~ Lake,
    scales = "free",
    strip = strip_themed(
      background_x = elem_list_rect(
        fill = c(
          "T" = "#755A42",
          "L" = "#44729C"
        ),
        colour = NA
      ),
      text_x = elem_list_text(
        color = "white",
        size = 10
      )
    ),
    labeller = labeller(
      Lake = c(
        "T" = "Tuesday (experimental)",
        "L" = "Paul (reference)"
      )
    )
  ) +
  labs(x = "Chlorophyll (ug/L)", y = "kPAR") +
  theme(legend.position = "none")


ggarrange(comp.color, comp.chl, nrow = 2, ncol = 1)



#### how many kNC values are negative? ####

data = read.csv("./data/formatted data/simulation model inputs 2013-2015 2024 2025 v4.csv") %>% 
  mutate(kNC = kPAR - 0.0177*Manual_Chl) %>% 
  filter(!(is.na(Ztherm)))
