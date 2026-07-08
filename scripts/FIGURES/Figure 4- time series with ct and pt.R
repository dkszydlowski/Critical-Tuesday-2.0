#### plot passage times with critical transitions ####

library(tidyverse)
library(ggh4x)


####===============================================================================================================================================================================================================#
#### plot with horizontal bars when the ecosystem was in different states ####

# Load DLM result
#save(useBGA,Tstep,X.dlm,level,levelsd,stdlevel,file=Fname)  
# load(file='DLMresult_YSI_Peter19.Rdata')
load('./results/DLMresult_HYLB_Paul_ALL_Chl_Predicted to Manual Scale 098 NOISY ARIMA.Rdata')

# thin the data to match DDJ
aropt=3
nx = length(stdlevel)
ikeep = seq(1, nx, by = aropt)

stdlevel = stdlevel[ikeep]
Tstep = Tstep[ikeep]

# put Tstep and stdlevel into a dataset
dat0 = as.data.frame(cbind(Tstep,stdlevel)) %>% 
  mutate(year = trunc(Tstep))


# convert Tstep back to a time
L.all = read.csv("./data/formatted data/HF data/Predicted Paul HYLB on Manual Scale log-trans NOISY ARIMA.csv") %>% 
  mutate(Lake = "L") %>% 
  arrange(datetime)



### Create a Tscore that combines year and DoY
mindoy = min(L.all$DoY, na.rm = TRUE)
maxdoy = max(L.all$DoY, na.rm = TRUE)

dat0 = dat0 %>% 
  mutate(doy =  mindoy + (dat0$Tstep - dat0$year) * (maxdoy - mindoy + 1)) %>% 
  filter(Tstep > 2013.11800 )

# Load DDJ data and apply to dataframe
load('./results/DDJ results Paul ARIMA-corrected data.Rdata')

low = xeq2[1]
thresh = xeq2[2]
high = xeq2[3]


dat0  = dat0 %>% 
  mutate(basin = case_when(stdlevel > thresh~"high-pigment",
                           stdlevel < thresh~"low-pigment")) %>% 
  mutate(equilibria = case_when(stdlevel > thresh~high,
                                stdlevel < thresh~low))




dat0 = dat0 %>%
  arrange(year, doy) %>%
  mutate(basin = case_when(stdlevel > thresh ~ "high-pigment",
                           stdlevel < thresh ~ "low-pigment")) %>%
  # create a group that breaks line at color changes
  group_by(year) %>%
  mutate(group = cumsum(basin != lag(basin, default = first(basin)))) %>%
  ungroup()

ggplot(dat0, aes(x = doy, y = stdlevel, group = group, color = basin)) +
  geom_line(size = 1, alpha = 0.75) +
  geom_line(aes(x = doy, y = equilibria), size = 2, color = "black") +
  geom_hline(yintercept = thresh, linetype = "dashed") +
  scale_color_manual(values = c("high-pigment" = "#b4c187", "low-pigment" = "#44729C")) +
  facet_wrap(~year) +
  theme_classic() +
  labs(x = "DOY", y = "stdlevel")+
  theme(legend.position = "none")




#### Add in the critical transitions ####

# locatino of critical transitions with manual results
# "./scripts/Multivariate DLM/Tuesday MANUAL eigenvalues 2026-02-27 NO BGA log-transformed chl only.csv"


ct = read.csv("./results/Paul CT DLM.csv") %>%
  rename(year = Year) %>%
  filter(delta == 0.90) %>%
  group_by(year) %>%
  arrange(doy, .by_group = TRUE) %>%
  filter(eigvals >= 1 & lag(eigvals, default = 0) < 1) %>%
  ungroup()

ggplot(dat0, aes(x = doy, y = stdlevel, group = group, color = basin)) +
  geom_line(size = 1, alpha = 0.75) +
  geom_line(aes(x = doy, y = equilibria), size = 2, color = "black") +
  geom_hline(yintercept = thresh, linetype = "dashed") +
  # Add vertical lines for eigenvalues > 1
  geom_vline(
    data = ct %>% filter(eigvals > 1),
    aes(xintercept = doy),
    color = "black",
    linetype = "dashed",
    size = 1
  ) +
  scale_color_manual(values = c("high-pigment" = "#5a6b3a", "low-pigment" = "#44729C")) +
  facet_wrap(~year) +
  theme_classic() +
  labs(x = "DOY", y = "Chlorophyll (standard level)") +
  theme(legend.position = "none")


dat0.L = dat0 %>% 
  mutate(lake = "Paul")

ct.L = ct %>% 
  mutate(lake = "Paul")




###### combine with the Tuesday runs #######



####===============================================================================================================================================================================================================#
#### plot with horizontal bars when the ecosystem was in different states ####

# Load DLM result
#save(useBGA,Tstep,X.dlm,level,levelsd,stdlevel,file=Fname)  
# load(file='DLMresult_YSI_Peter19.Rdata')
load('./results/DLMresult_HYLB_Tuesday_ALL_Chl_Predicted to Manual Scale 098 NOISY ARIMA.Rdata')

# thin the data to match DDJ
aropt=3
nx = length(stdlevel)
ikeep = seq(1, nx, by = aropt)

stdlevel = stdlevel[ikeep]
Tstep = Tstep[ikeep]

# put Tstep and stdlevel into a dataset
dat0 = as.data.frame(cbind(Tstep,stdlevel)) %>% 
  mutate(year = trunc(Tstep))


# convert Tstep back to a time
T.all = read.csv("./data/formatted data/HF data/Predicted Tuesday HYLB on Manual Scale log-trans NOISY ARIMA.csv") %>% 
  mutate(Lake = "T") %>% 
  arrange(datetime)



### Create a Tscore that combines year and DoY
mindoy = min(T.all$DoY, na.rm = TRUE)
maxdoy = max(T.all$DoY, na.rm = TRUE)

dat0 = dat0 %>% 
  mutate(doy =  mindoy + (dat0$Tstep - dat0$year) * (maxdoy - mindoy + 1))

# Load DDJ data and apply to dataframe
load('./results/DDJ results Tuesday ARIMA-corrected data.Rdata')

low = xeq2[1]
thresh = xeq2[2]
high = xeq2[3]


dat0  = dat0 %>% 
  mutate(basin = case_when(stdlevel > thresh~"high-pigment",
                           stdlevel < thresh~"low-pigment")) %>% 
  mutate(equilibria = case_when(stdlevel > thresh~high,
                                stdlevel < thresh~low))




dat0 = dat0 %>%
  arrange(year, doy) %>%
  mutate(basin = case_when(stdlevel > thresh ~ "high-pigment",
                           stdlevel < thresh ~ "low-pigment")) %>%
  # create a group that breaks line at color changes
  group_by(year) %>%
  mutate(group = cumsum(basin != lag(basin, default = first(basin)))) %>%
  ungroup()

ggplot(dat0, aes(x = doy, y = stdlevel, group = group, color = basin)) +
  geom_line(size = 1, alpha = 0.75) +
  geom_line(aes(x = doy, y = equilibria), size = 2, color = "black") +
  geom_hline(yintercept = thresh, linetype = "dashed") +
  scale_color_manual(values = c("high-pigment" = "#5a6b3a", "low-pigment" = "#533113")) +
  facet_wrap(~year) +
  theme_classic() +
  labs(x = "DOY", y = "stdlevel")+
  theme(legend.position = "none")


#### Add in the critical transitions ####
# ct = read.csv("./scripts/Multivariate DLM/eigenvalues 2026-01-27.csv") %>% 
#   filter(delta == 0.95 & eigvals >= 1) %>% 
#   rename(year = Year)

# locatino of critical transitions with manual results
# "./scripts/Multivariate DLM/Tuesday MANUAL eigenvalues 2026-02-27 NO BGA log-transformed chl only.csv"


ct = read.csv("./results/Tuesday CT DLM.csv") %>%
  rename(year = Year) %>%
  filter(delta == 0.90) %>%
  group_by(year) %>%
  arrange(doy, .by_group = TRUE) %>%
  filter(eigvals >= 1 & lag(eigvals, default = 0) < 1) %>%
  ungroup()

ggplot(dat0, aes(x = doy, y = stdlevel, group = group, color = basin)) +
  geom_line(size = 1, alpha = 0.75) +
  geom_line(aes(x = doy, y = equilibria), size = 2, color = "black") +
  geom_hline(yintercept = thresh, linetype = "dashed") +
  # Add vertical lines for eigenvalues > 1
  geom_vline(
    data = ct %>% filter(eigvals > 1),
    aes(xintercept = doy),
    color = "steelblue3",
    linetype = "solid",
    size = 1
  ) +
  scale_color_manual(values = c("high-pigment" = "#5a6b3a", "low-pigment" = "#533113")) +
  facet_wrap(~year) +
  theme_classic() +
  labs(x = "DOY", y = "Chlorophyll (standard level)") +
  theme(legend.position = "none")



dat0.T = dat0 %>% 
  mutate(lake = "Tuesday")

ct.T = ct %>% 
  mutate(lake = "Tuesday")





#####=================================================================================================================================================#
###### COMBINE THE TWO ######

dat0.all = rbind(dat0.T, dat0.L)

ct.all = rbind(ct.T, ct.L) %>% 
  mutate(lake = factor(lake, levels = c("Tuesday", "Paul")))


dat0.all <- dat0.all %>%
  mutate(
    basin_lake = case_when(
      lake == "Paul"    & basin == "low-pigment"  ~ "Paul_low",
      lake == "Paul"    & basin == "high-pigment" ~ "Paul_high",
      lake == "Tuesday" & basin == "low-pigment"  ~ "Tuesday_low",
      lake == "Tuesday" & basin == "high-pigment" ~ "Tuesday_high"
    )
  ) %>% 
  mutate(lake = factor(lake, levels = c("Tuesday", "Paul")))

ggplot(
  dat0.all,
  aes(x = doy, y = stdlevel, group = interaction(group, lake), color = basin_lake)) +
  geom_line(linewidth = 1, alpha = 0.75) +
  
  geom_line(
    aes(y = equilibria),
    color = "black",
    linewidth = 1.5) +
  
  geom_hline(
    yintercept = thresh,
    linetype = "dashed") +
  
  geom_vline(
    data = ct.all,
    aes(xintercept = doy),
    color = "#CC79A7",
    linewidth = 1) +
  
  scale_color_manual(
    values = c(
      "Paul_low"    = "#44729C",  # blue
      "Paul_high"   = "#5a6b3a",  # green
      "Tuesday_low" = "#533113",  # brown
      "Tuesday_high"= "#5a6b3a"   # same green
    )) +
  
  facet_grid(
    lake ~ year,
    labeller = labeller(
      lake = c(
        "Tuesday" = "Tuesday (experimental)",
        "Paul"    = "Paul (reference)"
      )
    )
  )+
  theme_bw() +
  labs(
    x = "Day of Year",
    y = "Chlorophyll (standard level)") +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(size = 14),
    strip.placement = "outside",
    strip.text.y.left = element_text(angle = 90))+
  scale_x_continuous(
    breaks = c(121, 152, 182, 213),
    labels = c("May", "Jun", "Jul", "Aug")
  )





png("./figures/Figure 4 pt and ct.png", res = 600, height = 100, width = 173, units = "mm") 



ggplot(
  dat0.all,
  aes(x = doy, y = stdlevel,
      group = interaction(group, lake),
      color = basin_lake)
) +
  geom_line(linewidth = 0.7, alpha = 0.75) +
  
  geom_line(aes(y = equilibria),
            color = "black",
            linewidth = 1.5) +
  
  geom_hline(yintercept = thresh,
             linetype = "dashed") +
  
  geom_vline(data = ct.all,
             aes(xintercept = doy),
             color = "#CC79A7",
             linewidth = 1, alpha = 0.7) +
  
  scale_color_manual(
    values = c(
      "Paul_low"    = "#44729C",
      "Paul_high"   = "#5a6b3a",
      "Tuesday_low" = "#533113",
      "Tuesday_high"= "#5a6b3a"
    )
  ) +
  
  facet_grid2(
    lake ~ year,
    strip = strip_themed(
      
      # YEARS (top) — completely clean
      background_x = elem_list_rect(
        fill = "transparent",
        colour = NA
      ),
      text_x = elem_list_text(size = 10),
      
      # LAKES (left) — colored
      background_y = elem_list_rect(
        fill = c(
          "Tuesday" = "#533113",
          "Paul"    = "#44729C"
        ),
        colour = NA
      ),
      text_y = elem_list_text(
        color = "white",
        size = 10
      )
    ),
    
    labeller = labeller(
      lake = c(
        "Tuesday" = "Tuesday (experimental)",
        "Paul"    = "Paul (reference)"
      )
    )
  )+
  theme_bw() +
  labs(
    x = "Date",
    y = "Chlorophyll (standard level)"
  ) +
  
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    strip.placement = "outside"
  ) +
  
  scale_x_continuous(
    breaks = c(121, 152, 182, 213),
    labels = c("May", "Jun", "Jul", "Aug")
  )


dev.off()
