#### Supplemental figures, including figures of the model outputs and fits

library(tidyverse)
library(ggpubr)
library(ggh4x)
library(ggtext)






#### FIGURE S1 - plot kNC boxplots for both Paul and Tuesday ####

data = read.csv("./data/formatted data/simulation model inputs 2013-2015 2024 2025 v4.csv") %>% 
  filter(Lake %in% c("L", "T")) %>% 
  mutate(kNC = kPAR - 0.0177*Manual_Chl)


thermo.comp = ggplot(data, aes(x = as.factor(Year), y = Ztherm, fill = Lake))+
  geom_boxplot()+
  scale_fill_manual(values = c("L" = "steelblue3", "T" = "#8c5c2b"),
                    labels = c("L" = "Paul", "T" = "Tuesday"))+
  theme_bw()+
  labs(x = "", y = "Thermocline depth (m)")+
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 12))

DOC.comp = ggplot(data, aes(x = as.factor(Year), y = DOC, fill = Lake))+
  geom_boxplot()+
  scale_fill_manual(values = c("L" = "steelblue3", "T" = "#8c5c2b"),
                    labels = c("L" = "Paul", "T" = "Tuesday"))+
  theme_bw()+
  labs(x = "", y = "DOC (mg/L)")+
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 12))


kNC.comp = ggplot(data %>% filter(kNC > 0), aes(x = as.factor(Year), y = kNC, fill = Lake))+
  geom_boxplot()+
  scale_fill_manual(values = c("L" = "steelblue3", "T" = "#8c5c2b"),
                    labels = c("L" = "Paul", "T" = "Tuesday"))+
  theme_bw()+
  labs(x = "", y = "kNC")+
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 12))+
  ylim(0, 2.5)


Chl.comp = ggplot(data %>% filter(kNC > 0), aes(x = as.factor(Year), y = Manual_Chl, fill = Lake))+
  geom_boxplot()+
  scale_fill_manual(values = c("L" = "steelblue3", "T" = "#8c5c2b"),
                    labels = c("L" = "Paul", "T" = "Tuesday"))+
  theme_bw()+
  labs(x = "", y = "Manual Chlorophyll (μg/L)")+
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 12))



PAR.comp = ggplot(data %>% filter(kNC > 0), aes(x = as.factor(Year), y = kPAR, fill = Lake))+
  geom_boxplot()+
  scale_fill_manual(values = c("L" = "steelblue3", "T" = "#8c5c2b"),
                    labels = c("L" = "Paul", "T" = "Tuesday"))+
  theme_bw()+
  labs(x = "", y = "kPAR")+
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 12))+
  ylim(0, 2.5)

ggarrange(DOC.comp, thermo.comp, PAR.comp, kNC.comp, common.legend = TRUE, nrow = 2, ncol = 2)








#### FIGURES S2 and S3 - Plot weekly zooplankton for both lakes ####

zoops = read.csv("./data/formatted data/cascade_zooplankton_v07_DTH.csv")

# sum by year and doy
sum.zoops = zoops %>% 
  group_by(year4, lakeid, daynum) %>% 
  summarize(total.biomass = sum(biomass, na.rm = TRUE)) %>% 
  filter(lakeid %in% c("L", "T"))


ggplot(sum.zoops, aes(x = as.factor(year4), y = total.biomass, color = lakeid))+
  geom_boxplot()


mean.biomass = sum.zoops %>% 
  filter(year4 %in% c(2013:2015, 2024, 2025)) %>% 
  group_by(year4, lakeid) %>% 
  summarize(mean.biomass = median(total.biomass, na.rm = TRUE),
            sd.biomass = sd(total.biomass, na.rm = TRUE))


# filter to relevant years and lakes and plot for supplement
sum.zoops.relevant = sum.zoops %>% 
  filter(year4 %in% c(2013:2015, 2024, 2025))


week.zoop.bio = ggplot(sum.zoops.relevant, aes(x = as.factor(year4), y = total.biomass, fill = lakeid))+
  geom_boxplot()+
  scale_fill_manual(values = c("L" = "steelblue3", "T" = "#8c5c2b"),
                    labels = c("L" = "Paul", "T" = "Tuesday"))+
  theme_bw()+
  labs(x = "", y = expression("Weekly zooplankton biomass (g/m"^2*")"))+
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.title = element_blank(),
        legend.text = element_text(size = 12))


zoops.relevant = zoops %>% 
  filter(lakeid %in% c("L", "T"), year4 %in% c(2013:2015, 2024, 2025))

ggplot(zoops.relevant, aes(x = biomass, fill = group_code)) + 
  geom_histogram(position = "stack") + 
  # scale_fill_discrete(palette = c("#0f3375", "#046dc8","#6fb1a0", "#a1c349", "#1f6924","#b4418e", "#ea515f", "#fe7434","#fea802")) +
  # scale_x_continuous(transform = "log10") +
  theme_bw() + 
  facet_grid(lakename~year4) +
  # facet_wrap(~year_frac) + 
  # ggtitle("Tuesday Zooplankton Biomass Distributions",
  #         subtitle = "Sample dates ordered from spring to late summer") +
  xlab("Biomass")




ggplot(zoops.relevant %>% filter(taxon_name == "Daphnia"), aes(x = biomass, fill = group_code)) + 
  geom_histogram(position = "stack") + 
  # scale_fill_discrete(palette = c("#0f3375", "#046dc8","#6fb1a0", "#a1c349", "#1f6924","#b4418e", "#ea515f", "#fe7434","#fea802")) +
  # scale_x_continuous(transform = "log10") +
  theme_bw() + 
  facet_grid(lakename~year4) +
  # facet_wrap(~year_frac) + 
  # ggtitle("Tuesday Zooplankton Biomass Distributions",
  #         subtitle = "Sample dates ordered from spring to late summer") +
  xlab("Biomass")



zoops.relevant = zoops.relevant %>% 
  mutate(lakename = factor(lakename, levels = c("Tuesday Lake", "Paul Lake")))


week.daph.bio = ggplot(zoops.relevant  %>% filter(taxon_name == "Daphnia"), aes(x = as.factor(year4), y = biomass,  fill = lakeid)) + 
  geom_boxplot() + 
  # scale_fill_discrete(palette = c("#0f3375", "#046dc8","#6fb1a0", "#a1c349", "#1f6924","#b4418e", "#ea515f", "#fe7434","#fea802")) +
  # scale_x_continuous(transform = "log10") +
  theme_bw()+
  scale_fill_manual(values = c("L" = "steelblue3", "T" = "#8c5c2b"),
                    labels = c("L" = "Paul", "T" = "Tuesday"))+
  labs(x = "", y = expression("Weekly Daphnia biomass (g/m"^2*")"))+
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.title = element_blank(),
        legend.text = element_text(size = 12))


png("./figures/Supplemental figures/Figure S2 zoop boxplots.png", res = 600, height = 120, width = 225, units = "mm") 

ggarrange(week.zoop.bio, week.daph.bio, nrow = 1, ncol = 2, common.legend = TRUE)

dev.off()


png("./figures/Supplemental figures/Figure S3 zoop histograms.png", res = 600, height = 120, width = 225, units = "mm") 


ggplot(zoops.relevant, aes(x = log10(biomass), fill = group_code)) + 
  geom_histogram(position = "stack") +
  scale_fill_viridis_d(option = "mako", direction = -1, end = 0.8,
                       labels = c(
                         "CCOP" = "Carnivorous copepods",
                         "OCOP"  = "Omnivorous copepods",
                         "CLAD" = "Cladocerans",
                         "ROT"  = "Rotifers"
                       )) +
  theme_bw() + 
  facet_grid2(
    lakename ~ year4,
    strip = strip_themed(
      background_x = elem_list_rect(fill = "transparent", colour = NA),
      text_x = elem_list_text(size = 10),
      background_y = elem_list_rect(
        fill = c("Tuesday" = "#755A42", "Paul" = "#44729C"),
        colour = NA
      ),
      text_y = elem_list_text(color = "white", size = 12)
    ),
    labeller = labeller(
      lakename = c(
        "Tuesday Lake" = "Tuesday Lake\n (experimental)",
        "Paul Lake" = "Paul Lake\n (reference)"
      )
    )
  ) +
  xlab(expression(" log10(zooplankton biomass g/m"^2*")")) +
  theme(legend.title = element_blank(),
        axis.text = element_text(size = 12), 
        legend.position = "top")


dev.off()







#### FIGURES S4 and S5 - ARIMA fitted vs observed #####
t.arima = read.csv("./results/ARIMA model fits/Tuesday ARIMA fits.csv") %>% 
  mutate(lake = "T")
L.arima = read.csv("./results/ARIMA model fits/Paul ARIMA fits.csv")

all.arima = rbind(t.arima, L.arima) %>% 
  mutate(lake = factor(lake, levels = c("T", "L")))

png("./figures/Supplemental figures/Figure S4 ARIMA fits.png", res = 600, height = 100, width = 173, units = "mm") 


ggplot(all.arima, aes(x = original, y = predicted, color = lake)) +
  geom_point(alpha = 0.5, size = 0.9) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  stat_poly_eq(
    formula = y ~ x,
    aes(label = ..rr.label..),
    parse = TRUE,
    size = 4,
    label.x = 0.05,  
    label.y = 0.95,
    color = "black") +
  theme_bw() +
  scale_color_manual(values = c("L" = "#44729C", "T" = "#755A42")) +
  facet_grid2(
    lake ~ year,
    scales = "free",
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
          "T" = "#755A42",
          "L" = "#44729C"
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
        "T" = "Tuesday (experimental)",
        "L" = "Paul (reference)"
      )
    )
  ) +
  labs(x = "log10(observed manual chlorophyll)", y = "log10(ARIMA-predicted manual chlorophyll)") +
  theme(legend.position = "none")

dev.off()



## ARIMA time series manual and corrected sonde ###
l.corrected = read.csv("./results/ARIMA model fits/Paul HF and manual corrected for comparison.csv") %>% 
  mutate(lake = "L")

t.corrected = read.csv("./results/ARIMA model fits/Tuesday HF and manual corrected for comparison.csv") %>% 
  mutate(lake = "T")

all.corrected = rbind(l.corrected, t.corrected)

all.corrected = all.corrected %>% 
  mutate(lake = factor(lake, levels = c("T", "L")))

png("./figures/Supplemental figures/Figure S5 ARIMA manual and corrected.png", res = 600, height = 100, width = 173, units = "mm") 

ggplot(all.corrected, aes(x = DoY, y = corrected.sonde, color = lake)) +
  geom_line(size = 0.3) +
  geom_point(data = all.corrected, aes(x = DoY, y = Mchl), color = "black", size = 0.7) +
  theme_bw() +
  scale_color_manual(values = c("L" = "#44729C", "T" = "#755A42")) +
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
          "T" = "#755A42",
          "L" = "#44729C"
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
        "T" = "Tuesday (experimental)",
        "L" = "Paul (reference)"
      )
    )
  ) +
  scale_x_continuous(
    breaks = c(152, 182, 213, 244),
    labels = c("Jun", "Jul", "Aug", "Sep")
  ) +
  labs(x = "Date", y = "log10(Chlorophyll)") +
  theme(legend.position = "none")

dev.off()



#-------------------------------------------------------------------------------------------------------------------------------------------#
#### FIGURES S6 - S10: PLOT DLM RESULTS #####


load('./results/DLMresult_HYLB_Tuesday_ALL_Chl_Predicted to Manual Scale 098 NOISY ARIMA.Rdata')

Nstep = length(Tstep)

dlm.results.actual = data.frame(Tstep = Tstep, X.dlm = X.dlm, datetime = useChl$datetime)
dlm.results.predicted = data.frame(Tstep = Tstep[2:Nstep], yhat = Yyhat[, 3], datetime = useChl$datetime[2:Nstep])

dlm.results.tuesday = dlm.results.actual %>% 
  full_join(dlm.results.predicted, by = c("Tstep", "datetime")) %>% 
  mutate(year = floor(Tstep), lake = "T")

load('./results/DLMresult_HYLB_Paul_ALL_Chl_Predicted to Manual Scale 098 NOISY ARIMA.Rdata')

Nstep = length(Tstep)

dlm.results.actual = data.frame(Tstep = Tstep, X.dlm = X.dlm, datetime = useChl$datetime)
dlm.results.predicted = data.frame(Tstep = Tstep[2:Nstep], yhat = Yyhat[, 3], datetime = useChl$datetime[2:Nstep])

dlm.results.paul = dlm.results.actual %>% 
  full_join(dlm.results.predicted, by = c("Tstep", "datetime")) %>% 
  mutate(year = floor(Tstep), lake = "L")



# combine
all.dlm = bind_rows(dlm.results.tuesday, dlm.results.paul) %>% 
  mutate(lake = factor(lake, levels = c("T", "L")))   # Tuesday row on top


png("./figures/Supplemental figures/Figure S6 2013 DLM fit.png", res = 600, height = 150, width = 250, units = "mm") 


# combined plot: Tuesday & Paul side by side, all years
ggplot(all.dlm %>% filter(year == 2013), aes(x = ymd_hms(datetime))) +
  geom_point(aes(y = X.dlm), color = "black", size = 0.3, alpha = 0.3) +
  geom_line(aes(y = yhat, color = lake), size = 0.6) +
  scale_color_manual(values = c("T" = "#755A42", "L" = "#44729C")) +
  facet_grid2(
    lake ~ year,
    scales = "free_x",
    strip = strip_themed(
      background_x = elem_list_rect(fill = "transparent", colour = NA),
      text_x = elem_list_text(size = 12),
      background_y = elem_list_rect(
        fill = c("T" = "#755A42", "L" = "#44729C"),
        colour = NA
      ),
      text_y = elem_list_text(color = "white", size = 12)
    ),
    labeller = labeller(
      lake = c("T" = "Tuesday (experimental)", "L" = "Paul (reference)")
    )
  ) +
  theme_bw() +
  labs(x = "Date", y = "Standardized level of chlorophyll") +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 16),
    strip.text = element_text(size = 16),
    legend.position = "none"
  )

dev.off()




png("./figures/Supplemental figures/Figure S7 2014 DLM fit.png", res = 600, height = 150, width = 250, units = "mm") 


# combined plot: Tuesday & Paul side by side, all years
ggplot(all.dlm %>% filter(year == 2014), aes(x = ymd_hms(datetime))) +
  geom_point(aes(y = X.dlm), color = "black", size = 0.3, alpha = 0.3) +
  geom_line(aes(y = yhat, color = lake), size = 0.6) +
  scale_color_manual(values = c("T" = "#755A42", "L" = "#44729C")) +
  facet_grid2(
    lake ~ year,
    scales = "free_x",
    strip = strip_themed(
      background_x = elem_list_rect(fill = "transparent", colour = NA),
      text_x = elem_list_text(size = 12),
      background_y = elem_list_rect(
        fill = c("T" = "#755A42", "L" = "#44729C"),
        colour = NA
      ),
      text_y = elem_list_text(color = "white", size = 12)
    ),
    labeller = labeller(
      lake = c("T" = "Tuesday (experimental)", "L" = "Paul (reference)")
    )
  ) +
  theme_bw() +
  labs(x = "Date", y = "Standardized level of chlorophyll") +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 16),
    strip.text = element_text(size = 16),
    legend.position = "none"
  )

dev.off()



png("./figures/Supplemental figures/Figure S8 2015 DLM fit.png", res = 600, height = 150, width = 250, units = "mm") 


# combined plot: Tuesday & Paul side by side, all years
ggplot(all.dlm %>% filter(year == 2015), aes(x = ymd_hms(datetime))) +
  geom_point(aes(y = X.dlm), color = "black", size = 0.3, alpha = 0.3) +
  geom_line(aes(y = yhat, color = lake), size = 0.6) +
  scale_color_manual(values = c("T" = "#755A42", "L" = "#44729C")) +
  facet_grid2(
    lake ~ year,
    scales = "free_x",
    strip = strip_themed(
      background_x = elem_list_rect(fill = "transparent", colour = NA),
      text_x = elem_list_text(size = 12),
      background_y = elem_list_rect(
        fill = c("T" = "#755A42", "L" = "#44729C"),
        colour = NA
      ),
      text_y = elem_list_text(color = "white", size = 12)
    ),
    labeller = labeller(
      lake = c("T" = "Tuesday (experimental)", "L" = "Paul (reference)")
    )
  ) +
  theme_bw() +
  labs(x = "Date", y = "Standardized level of chlorophyll") +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 16),
    strip.text = element_text(size = 16),
    legend.position = "none"
  )

dev.off()



png("./figures/Supplemental figures/Figure S9 2024 DLM fit.png", res = 600, height = 150, width = 250, units = "mm") 


# combined plot: Tuesday & Paul side by side, all years
ggplot(all.dlm %>% filter(year == 2024), aes(x = ymd_hms(datetime))) +
  geom_point(aes(y = X.dlm), color = "black", size = 0.3, alpha = 0.3) +
  geom_line(aes(y = yhat, color = lake), size = 0.6) +
  scale_color_manual(values = c("T" = "#755A42", "L" = "#44729C")) +
  facet_grid2(
    lake ~ year,
    scales = "free_x",
    strip = strip_themed(
      background_x = elem_list_rect(fill = "transparent", colour = NA),
      text_x = elem_list_text(size = 12),
      background_y = elem_list_rect(
        fill = c("T" = "#755A42", "L" = "#44729C"),
        colour = NA
      ),
      text_y = elem_list_text(color = "white", size = 12)
    ),
    labeller = labeller(
      lake = c("T" = "Tuesday (experimental)", "L" = "Paul (reference)")
    )
  ) +
  theme_bw() +
  labs(x = "Date", y = "Standardized level of chlorophyll") +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 16),
    strip.text = element_text(size = 16),
    legend.position = "none"
  )

dev.off()




png("./figures/Supplemental figures/Figure S10 2025 DLM fit.png", res = 600, height = 150, width = 250, units = "mm") 


# combined plot: Tuesday & Paul side by side, all years
ggplot(all.dlm %>% filter(year == 2025), aes(x = ymd_hms(datetime))) +
  geom_point(aes(y = X.dlm), color = "black", size = 0.3, alpha = 0.3) +
  geom_line(aes(y = yhat, color = lake), size = 0.6) +
  scale_color_manual(values = c("T" = "#755A42", "L" = "#44729C")) +
  facet_grid2(
    lake ~ year,
    scales = "free_x",
    strip = strip_themed(
      background_x = elem_list_rect(fill = "transparent", colour = NA),
      text_x = elem_list_text(size = 12),
      background_y = elem_list_rect(
        fill = c("T" = "#755A42", "L" = "#44729C"),
        colour = NA
      ),
      text_y = elem_list_text(color = "white", size = 12)
    ),
    labeller = labeller(
      lake = c("T" = "Tuesday (experimental)", "L" = "Paul (reference)")
    )
  ) +
  theme_bw() +
  labs(x = "Date", y = "Standardized level of chlorophyll") +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 16),
    strip.text = element_text(size = 16),
    legend.position = "none"
  )

dev.off()



#### FIGURE S11 - actual DDJ results ####
load('./results/DDJ results Tuesday ARIMA-corrected data.Rdata')

ddj.result = data.frame(D1, D2, avec)


# take the sqrt of D2 because it has units of chl^2
ddj.result = ddj.result %>% 
  mutate(D2 = sqrt(2*D2)) %>% 
  pivot_longer(cols = c(D1, D2), names_to = "estimate") %>%
  mutate(estimate = recode(estimate,
                           D1 = "Drift",
                           D2 = "Diffusion (as s.d.)"))
library(ggtext)

DDJ.plot = ggplot(ddj.result, aes(x = avec, y = value, color = estimate))+
  geom_line()+
  theme_bw()+
  geom_line(size = 1.2) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  labs(x = "", y = "Drift or Diffusion", title = "Tuesday Lake (experimental)") +
  scale_color_manual(values = c("Drift" = "blue4",
                                "Diffusion (as s.d.)" = "red4")) +
  scale_linetype_manual(values = c("Drift" = "solid",
                                   "Diffusion" = "dashed")) +
  theme(
    legend.title = element_blank(),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.position = c(0.05, 0.05),          # coordinates inside plot
    legend.justification = c(0, 0),           # aligns bottom-left of legend box to these coordinates
    plot.title = ggtext::element_textbox_simple(
      fill = "#755A42",
      color = "white",
      face = "bold",
      size = 13,
      halign = 0.5,
      linetype = 1,
      box.color = "black",
      linewidth = 0.5,
      padding = margin(5, 5, 5, 5),
      margin = margin(b = 8)
    )
  )+
  ylim(-4, 5)

EP.all = data.frame(chl = EPout[[1]][2:100], EP = EPout[[2]]) # EP only goes from 2:100 of chl

EP.plot = ggplot(EP.all, aes(x = chl, y = EP))+
  geom_line(size = 1.2)+
  theme_bw()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12))+
  ylim(0, 2.5)+
  labs(x = "Chlorophyll, standardized level", y = "Effective Potential")


ggarrange(DDJ.plot, EP.plot, nrow = 2, ncol = 1, align = "v")


load('./results/DDJ results Paul ARIMA-corrected data.Rdata')


ddj.result.L = data.frame(D1, D2, avec)


# take the sqrt of D2 because it has units of chl^2
ddj.result.L = ddj.result.L %>% 
  mutate(D2 = sqrt(2*D2)) %>% 
  pivot_longer(cols = c(D1, D2), names_to = "estimate") %>%
  mutate(estimate = recode(estimate,
                           D1 = "Drift",
                           D2 = "Diffusion (as s.d.)"))




DDJ.plot.L = ggplot(ddj.result.L, aes(x = avec, y = value, color = estimate))+
  geom_line()+
  theme_bw()+
  geom_line(size = 1.2) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  labs(x = "", y = "", title = "Paul Lake (reference)") +
  scale_color_manual(values = c("Drift" = "blue4",
                                "Diffusion (as s.d.)" = "red4")) +
  scale_linetype_manual(values = c("Drift" = "solid",
                                   "Diffusion" = "dashed")) +
  theme(
    legend.title = element_blank(),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.position = c(0.05, 0.05),
    legend.justification = c(0, 0),
    plot.title = ggtext::element_textbox_simple(
      fill = "#44729C",
      color = "white",
      face = "bold",
      size = 13,
      halign = 0.5,
      linetype = 1,
      box.color = "black",
      linewidth = 0.5,
      padding = margin(5, 5, 5, 5),
      margin = margin(b = 8)
    )
  )+
  
  ylim(-4, 5)

EP.all.L = data.frame(chl = EPout[[1]][2:100], EP = EPout[[2]]) # EP only goes from 2:100 of chl

EP.plot.L = ggplot(EP.all.L, aes(x = chl, y = EP))+
  geom_line(size = 1.2)+
  theme_bw()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12))+
  labs(x = "Chlorophyll, standardized level", y = "")+
  ylim(0, 2.5)

png("./figures/Supplemental figures/Figure S11 DDJ models and EP.png", res = 600, height = 200, width = 200, units = "mm") 


ggarrange(nrow = 2, ncol = 2, DDJ.plot, DDJ.plot.L, EP.plot, EP.plot.L)


dev.off()




#### FIGURE S12 - Full distributions of passage times #####






#### FIGURE S13 - passage times compared to nutrient loading #####






#### FIGURE S14 - passage times compared to mean zooplankton biomass #####









#=========================================================================================================================#
#### FIGURE S15- plotting the bootstrapped DDJ and EP #####

## TUESDAY ##

load('./results/bootstrapped results/DDJ_boot_Tuesday 1000.Rdata')

# load in the epfeq function
source('./scripts/Step 3- Fit DDJ models/0_EPFunction+EQ.R')


# Average across bootstrap replicates
D1_mean = rowMeans(D1mat, na.rm = TRUE)
D2_mean = sqrt(2*rowMeans(D2mat, na.rm = TRUE)) # sqrt-transform to get as s.d.

D1_sd = apply(D1mat, 1, sd, na.rm = TRUE)
D2_sd = sqrt(2*apply(D2mat, 1, sd, na.rm = TRUE))

# avec grid (usually identical across columns, so take first)
avec = amat[,1]

avg_df = data.frame(
  avec = avec,
  D1 = D1_mean,
  D1sd = D1_sd,
  D2 = D2_mean,
  D2sd = D2_sd
)


avg_df = avg_df %>% 
  mutate(D2_high = D2 + D2sd,
         D2_low = D2 - D2_sd,
         D1_high = D1 + D1sd,
         D1_low = D1 - D1sd)

drift_diff_avg =
  avg_df %>%
  pivot_longer(cols = c(D1, D2), names_to = "estimate") %>%
  mutate(estimate = recode(estimate,
                           D1 = "Drift",
                           D2 = "Diffusion (as s.d.)"))



DDJboot = ggplot(drift_diff_avg,
                 aes(x = avec, y = value, color = estimate, linetype = estimate)) +
  geom_ribbon(data = avg_df,
              aes(x = avec, ymin = D1_low, ymax = D1_high),
              inherit.aes = FALSE,
              fill = "grey30", alpha = 0.2) +
  geom_ribbon(data = avg_df,
              aes(x = avec, ymin = D2_low, ymax = D2_high),
              inherit.aes = FALSE,
              fill = "grey30", alpha = 0.2) +
  geom_line(size = 1.2) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  theme_classic() +
  labs(x = "", y = "Drift or Diffusion", title = "Tuesday Lake (experimental)") +
  scale_color_manual(values = c("Drift" = "blue4",
                                "Diffusion (as s.d.)" = "red4")) +
  scale_linetype_manual(values = c("Drift" = "solid",
                                   "Diffusion (as s.d.)" = "dashed")) +
  theme(
    legend.title = element_blank(),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.position = c(0.05, 0.80),          # coordinates inside plot
    legend.justification = c(0, 0),           # aligns bottom-left of legend box to these coordinates
      plot.title = ggtext::element_textbox_simple(
        fill = "#755A42",
        color = "white",
        face = "bold",
        size = 13,
        halign = 0.5,
        linetype = 1,
        box.color = "black",
        linewidth = 0.5,
        padding = margin(5, 5, 5, 5),
        margin = margin(b = 8)
      )
)+
ylim(-5, 12)+
  xlim(-10, 10)

#### plot the distribution of equilibrium points...not great.

# Convert the matrix to long format
xeq_df <- as.data.frame(t(xeqmat))  # transpose so columns are eq1, eq2, eq3
colnames(xeq_df) <- c("Eq1","Eq2","Eq3")

xeq_long <- xeq_df %>%
  pivot_longer(cols = everything(), names_to = "Equilibrium", values_to = "Value")

# Plot density
ggplot(xeq_long, aes(x = Value, fill = Equilibrium)) +
  geom_boxplot(alpha = 0.5, color = "black") +
  theme_minimal() +
  labs(title = "Bootstrap Equilibrium Points",
       x = "Equilibrium Value",
       y = "Density")



### instead, re-calculate effective potential for each column of the matrix ###

# Total D2 from Johannes: sum of diffusion & jump variances

sig.D2 = sqrt(D2) # To stick with the original EPFQ, we want to use just D2, not 2*D2

D2_sig = sqrt(D2mat)

# check equilibria of effective potential
EPinput = as.data.frame(cbind(avec,D1,sig.D2))

# screen out missing values if present
#EPin = na.omit(EPinput)

Nboot = ncol(D1mat)
nx = nrow(amat)

EPmat = matrix(NA, 99, Nboot)
EPchl = matrix(NA, 100, Nboot)

for(i in 1:Nboot){
  
  avec_i   = amat[, i]
  D1_i     = D1mat[, i]
  sigD2_i  = D2_sig[, i]
  
  # keep = !(is.na(avec_i) | is.na(D1_i) | is.na(sigD2_i))
  
  EPout = EPFEQ(avec_i, D1_i, sigD2_i)
  
  EPmat[, i] = EPout[[2]]
  EPchl[, i] = EPout[[1]] # also save the chl associated with each EP
  
}


EP_mean = apply(EPmat, 1, mean, na.rm = TRUE)
chl_mean = apply(EPchl, 1, mean, na.rm = TRUE)
EP_sd = apply(EPmat, 1, sd, na.rm = TRUE)

chl_mean = chl_mean[2:length(chl_mean)] # because EP_mean only has 99 obs

EP.all = data.frame(chl_mean, EP_mean, EP_sd, index = c(1:length(EP_mean))) %>% 
  mutate(EP_high = EP_mean + EP_sd, 
         EP_low = EP_mean - EP_sd)


EP.boot = ggplot(EP.all, aes(x = chl_mean, y = EP_mean))+
  geom_line(size = 1.2)+
  geom_ribbon(data = EP.all,
              aes(x = chl_mean, ymin = EP_low, ymax = EP_high),
              inherit.aes = FALSE,
              fill = "grey30", alpha = 0.2) +
  theme_classic()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12))+
  labs(x = "Chlorophyll, standardized level", y = "Mean Effective Potential")+
  ylim(0.3, 2)+
  xlim(-10, 10)




ggarrange(DDJboot, EP.boot, nrow = 2, ncol = 1, align = "v")




### PAUL ###

load('./results/bootstrapped results/DDJ_boot_Paul 1000.Rdata')

# load in the epfeq function
source('./scripts/Step 3- Fit DDJ models/0_EPFunction+EQ.R')


# Average across bootstrap replicates
D1_mean = rowMeans(D1mat, na.rm = TRUE)
D2_mean = sqrt(2*rowMeans(D2mat, na.rm = TRUE)) # sqrt-transform to get as s.d.

D1_sd = apply(D1mat, 1, sd, na.rm = TRUE)
D2_sd = sqrt(2*apply(D2mat, 1, sd, na.rm = TRUE))

# avec grid (usually identical across columns, so take first)
avec = amat[,1]

avg_df = data.frame(
  avec = avec,
  D1 = D1_mean,
  D1sd = D1_sd,
  D2 = D2_mean,
  D2sd = D2_sd
)


avg_df = avg_df %>% 
  mutate(D2_high = D2 + D2sd,
         D2_low = D2 - D2_sd,
         D1_high = D1 + D1sd,
         D1_low = D1 - D1sd)

drift_diff_avg =
  avg_df %>%
  pivot_longer(cols = c(D1, D2), names_to = "estimate") %>%
  mutate(estimate = recode(estimate,
                           D1 = "Drift",
                           D2 = "Diffusion (as s.d.)"))



DDJboot.L = ggplot(drift_diff_avg,
                 aes(x = avec, y = value, color = estimate, linetype = estimate)) +
  geom_ribbon(data = avg_df,
              aes(x = avec, ymin = D1_low, ymax = D1_high),
              inherit.aes = FALSE,
              fill = "grey30", alpha = 0.2) +
  geom_ribbon(data = avg_df,
              aes(x = avec, ymin = D2_low, ymax = D2_high),
              inherit.aes = FALSE,
              fill = "grey30", alpha = 0.2) +
  geom_line(size = 1.2) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  theme_classic() +
  labs(x = "", y = "Drift or Diffusion", title = "Paul Lake (reference)") +
  scale_color_manual(values = c("Drift" = "blue4",
                                "Diffusion (as s.d.)" = "red4")) +
  scale_linetype_manual(values = c("Drift" = "solid",
                                   "Diffusion (as s.d.)" = "dashed")) +
  theme(
    legend.title = element_blank(),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.position = c(0.05, 0.80),          # coordinates inside plot
    legend.justification = c(0, 0),           # aligns bottom-left of legend box to these coordinates
      plot.title = ggtext::element_textbox_simple(
        fill = "#44729C",
        color = "white",
        face = "bold",
        size = 13,
        halign = 0.5,
        linetype = 1,
        box.color = "black",
        linewidth = 0.5,
        padding = margin(5, 5, 5, 5),
        margin = margin(b = 8)
      )
  )+
  ylim(-5, 12)+
  xlim(-10, 10)


#### plot the distribution of equilibrium points...not great.

# Convert the matrix to long format
xeq_df <- as.data.frame(t(xeqmat))  # transpose so columns are eq1, eq2, eq3
colnames(xeq_df) <- c("Eq1","Eq2","Eq3")

xeq_long <- xeq_df %>%
  pivot_longer(cols = everything(), names_to = "Equilibrium", values_to = "Value")

# Plot density
ggplot(xeq_long, aes(x = Value, fill = Equilibrium)) +
  geom_boxplot(alpha = 0.5, color = "black") +
  theme_minimal() +
  labs(title = "Bootstrap Equilibrium Points",
       x = "Equilibrium Value",
       y = "Density")



### instead, re-calculate effective potential for each column of the matrix ###

# Total D2 from Johannes: sum of diffusion & jump variances

sig.D2 = sqrt(D2) # To stick with the original EPFQ, we want to use just D2, not 2*D2

D2_sig = sqrt(D2mat)

# check equilibria of effective potential
EPinput = as.data.frame(cbind(avec,D1,sig.D2))

# screen out missing values if present
#EPin = na.omit(EPinput)

Nboot = ncol(D1mat)
nx = nrow(amat)

EPmat = matrix(NA, 99, Nboot)
EPchl = matrix(NA, 100, Nboot)

for(i in 1:Nboot){
  
  avec_i   = amat[, i]
  D1_i     = D1mat[, i]
  sigD2_i  = D2_sig[, i]
  
  # keep = !(is.na(avec_i) | is.na(D1_i) | is.na(sigD2_i))
  
  EPout = EPFEQ(avec_i, D1_i, sigD2_i)
  
  EPmat[, i] = EPout[[2]]
  EPchl[, i] = EPout[[1]] # also save the chl associated with each EP
  
}


EP_mean = apply(EPmat, 1, mean, na.rm = TRUE)
chl_mean = apply(EPchl, 1, mean, na.rm = TRUE)
EP_sd = apply(EPmat, 1, sd, na.rm = TRUE)

chl_mean = chl_mean[2:length(chl_mean)] # because EP_mean only has 99 obs

EP.all = data.frame(chl_mean, EP_mean, EP_sd, index = c(1:length(EP_mean))) %>% 
  mutate(EP_high = EP_mean + EP_sd, 
         EP_low = EP_mean - EP_sd)


EP.boot.L = ggplot(EP.all, aes(x = chl_mean, y = EP_mean))+
  geom_line(size = 1.2)+
  geom_ribbon(data = EP.all,
              aes(x = chl_mean, ymin = EP_low, ymax = EP_high),
              inherit.aes = FALSE,
              fill = "grey30", alpha = 0.2) +
  theme_classic()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12))+
  labs(x = "Chlorophyll, standardized level", y = "Mean Effective Potential")+
  ylim(0.3, 2)+
  xlim(-10, 10)



png("./figures/Supplemental figures/Figure S15 Bootstrapped DDJ models and EP.png", res = 600, height = 200, width = 200, units = "mm") 


ggarrange(DDJboot, DDJboot.L, EP.boot, EP.boot.L, nrow = 2, ncol = 2, align = "v")


dev.off()








#### FIGURE S16 - Bootstrapped passage times compared to kNC ####
