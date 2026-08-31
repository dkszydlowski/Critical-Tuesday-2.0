# script for plotting passage time results for the global DDJ estimates
# run through line 548 to get Tuesday plots for combining with Paul in passage times plotting - PAUL.R

library(tidyverse)
library(ggridges)
library(ggpubr)
library(ggpmisc)
library(ggrepel)
library(ggh4x)
library(ggtext)


# read in the data
t25 = read.csv("./results/passage times/Tuesday ARIMA-corrected 2025 global 2026-06-18 THINNED.csv")
t24 = read.csv("./results/passage times/Tuesday ARIMA-corrected 2024 global 2026-06-18 THINNED.csv")
t15 = read.csv("./results/passage times/Tuesday ARIMA-corrected 2015 global 2026-06-18 THINNED.csv")
t14 = read.csv("./results/passage times/Tuesday ARIMA-corrected 2014 global 2026-06-18 THINNED.csv")
t13 = read.csv("./results/passage times/Tuesday ARIMA-corrected 2013 global 2026-06-18 THINNED.csv")


# read in Tukey's letters
T.letters = read.csv("./results/passage times/tukey_letters 2026-07-30.csv") %>% 
  filter(lake == "Tuesday") %>% 
  mutate(year = as.factor(year))

pt.all = rbind(t25, t24, t15, t14, t13) %>% 
  mutate(hours = minutes/60) 

pt.days = pt.all %>% 
  mutate(days = hours/24)

green_palette <- c("#CBD4AC", "#b4c187", "#80914b", "#5a6b3a", "#496231")
brown_palette <- c("#CFB491", "#9c7744", "#8c5c2b", "#533113", "#361c07")

T.letters.right = T.letters %>% filter(basin == "right")

rb.plot = ggplot(pt.all %>% filter(basin == "right" & minutes > 30), aes(x = as.factor(year), y = (hours)/24, fill = factor(year))) +
  geom_boxplot(alpha = 0.8)+
  labs(y = "", x = "") +
  geom_text(data = T.letters.right, aes(x = year, y = y_pos, label = Letters),
            inherit.aes = FALSE, size = 5) +
  theme(legend.position = "none", axis.text = element_text(size = 14), axis.title = element_text(size = 16))+
  scale_fill_manual(values = green_palette)+
  theme_classic()+
  scale_y_log10(breaks = c(0.1, 1, 10, 100), limits = c(0.005, 100))+
  theme(legend.position = "none")+
  theme(
    axis.text.y  = element_text(size = 12),
    axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
    axis.title = element_text(size = 12),
    strip.text = element_text(size = 12),
    legend.position = "none"
  ) +
  theme(plot.margin = margin(l = 0, r = 0, t = 0, b = 0))




#aov_left <- aov(hours ~ factor(year), data = subset(pt.all %>% filter(minutes > 30), basin == ""))
#summary(aov_left)

#TukeyHSD(aov_left)

# kruskal.test(hours ~ factor(year), data = subset(pt.all, basin == "left"))
# pairwise.t.test(
#   x = log10(subset(pt.all, basin == "left")$hours),
#   g = subset(pt.all, basin == "left")$year,
#   p.adjust.method = "BH"   # or "bonferroni"
# )


T.letters.left = T.letters %>% filter(basin == "left")

lb.plot = ggplot(pt.all %>% filter(basin == "left" & minutes > 30), aes(x = as.factor(year), y = (hours)/24, fill = factor(year))) +
  geom_boxplot(alpha = 0.8)+
  labs(y = "passage time (days)", x = "") +
  geom_text(data = T.letters.left, aes(x = year, y = y_pos, label = Letters),
            inherit.aes = FALSE, size = 5) +
  theme(legend.position = "none", axis.text = element_text(size = 14), axis.title = element_text(size = 16))+
  scale_fill_manual(values = rev(brown_palette))+
  theme_classic()+
  scale_y_log10(breaks = c(0.1, 1, 10, 100), limits = c(0.005, 100))+
  theme(legend.position = "none")+
  theme(
    axis.text.y  = element_text(size = 12),
    axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
    axis.title = element_text(size = 12),
    strip.text = element_text(size = 12),
    legend.position = "none"
  ) +
  theme(plot.margin = margin(l = 0, r = 0, t = 0, b = 0))




ptimes = ggarrange(lb.plot, rb.plot, align = "h")


#### version of the plot with no filtering ####
rb.plot = ggplot(pt.all %>% filter(basin == "right"), aes(x = as.factor(year), y = (hours)/24, fill = factor(year))) +
  geom_boxplot(alpha = 0.8)+
  geom_point()+
  labs(y = "", x = "Year", title = "high-chlorophyll") +
  theme(legend.position = "none", axis.text = element_text(size = 14), axis.title = element_text(size = 16))+
  scale_fill_manual(values = green_palette)+
  theme_classic()+
  scale_y_log10(breaks = c(0.1, 1, 10, 100), limits = c(0.005, 100))+
  theme(legend.position = "none")+
  theme(
    axis.text.y  = element_text(size = 12),
    axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
    axis.title = element_text(size = 12),
    strip.text = element_text(size = 12),
    legend.position = "none"
  ) +
  theme(plot.margin = margin(l = 0, r = 0, t = 0, b = 0))



lb.plot = ggplot(pt.all %>% filter(basin == "left"), aes(x = as.factor(year), y = (hours)/24, fill = factor(year))) +
  geom_boxplot(alpha = 0.8)+
  geom_point()+
  labs(y = "passage time (days)", x = "Year", title = "low-chlorophyll") +
  theme(legend.position = "none", axis.text = element_text(size = 14), axis.title = element_text(size = 16))+
  scale_fill_manual(values = rev(brown_palette))+
  theme_classic()+
  scale_y_log10(breaks = c(0.1, 1, 10, 100), limits = c(0.005, 100))+
  theme(legend.position = "none")+
  theme(
    axis.text.y  = element_text(size = 12),
    axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
    axis.title = element_text(size = 12),
    strip.text = element_text(size = 12),
    legend.position = "none"
  ) +
  theme(plot.margin = margin(l = 0, r = 0, t = 0, b = 0))



### density ridgeline plots of passage time ###

pt.left.density =  ggplot(
  pt.all %>% filter(basin == "left"),
  aes(x = (hours)/24, y = factor(year), fill = factor(year))
) +
  geom_density_ridges(scale = 1.2, alpha = 0.8, color = "white") +
  scale_fill_manual(values = rev(brown_palette)) +
  scale_x_log10(breaks = c(0.1, 1, 10, 100), limits = c(0.005, 100)) +
  labs(
    x = "passage time (days)",
    y = "Year",
    title = "low-chlorophyll"
  ) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    legend.position = "none"
  )+ 
  geom_vline(xintercept = 0.5/24, linetype = "dashed", size = 0.7)


pt.right.density = ggplot(
  pt.all %>% filter(basin == "right"),
  aes(x = (hours)/24, y = factor(year), fill = factor(year))
) +
  geom_density_ridges(scale = 1.2, alpha = 0.8, color = "white") +
  scale_fill_manual(values = (green_palette)) +
  scale_x_log10(breaks = c(0.1, 1, 10, 100), limits = c(0.005, 100)) +
  labs(
    x = "passage time (days)",
    y = "Year",
    title = "high-chlorophyll"
  ) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    legend.position = "none"
  )+
  geom_vline(xintercept = 0.5/24, linetype = "dashed", size = 0.7)




ggarrange(lb.plot, rb.plot, pt.left.density, pt.right.density, nrow = 2, ncol = 2)





# create combined fill variable
pt.all2 <- pt.all %>%
  mutate(
    basin_year = paste(basin, year, sep = "_")
  ) %>% 
  filter(minutes > 30)

# named color vector
fill_vals <- c(
  setNames(rev(brown_palette), paste0("left_", sort(unique(pt.all$year)))),
  setNames(green_palette, paste0("right_", sort(unique(pt.all$year))))
)

ptimes <- ggplot(
  pt.all2,
  aes(
    x = as.factor(year),
    y = hours / 24,
    fill = basin_year
  )
) +
  geom_text(
    data = T.letters,
    aes(
      x = year,
      y = y_pos,
      label = Letters
    ),
    inherit.aes = FALSE,
    size = 3) +
  geom_boxplot(alpha = 0.8) +
  facet_wrap(~ basin, nrow = 1) +
  scale_fill_manual(values = fill_vals) +
  scale_y_log10(
    limits = c(0.005, 100),
    breaks = c(0.01, 0.1, 1, 10, 100),
    labels = c("0.01", "0.1", "1", "10", "100"))+
  labs(x = "", y = "passage time (days)") +
  theme_bw() +
  theme(
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
    axis.title = element_text(size = 12),
    legend.position = "none",
    panel.grid = element_blank(),
    plot.margin = margin(r = 1, l = 0, t = 1, b = 0))+
  theme(strip.text = element_blank(),
        strip.background = element_blank())






## compare median and mean passage times to median and mean kNC

data = read.csv("./data/formatted data/simulation model inputs 2013-2015 2024 2025 v4.csv")

data %>% 
  group_by(Year) %>% 
  summarize(mean(grav.m2))

data.mean = data %>% 
  mutate(kNC = kPAR - 0.0177*Manual_Chl) %>% 
  filter(Lake == "T" & kNC > 0) %>% 
  filter(!is.na(Ztherm)) %>% 
  group_by(Year) %>% 
  summarize(mean.kNC = mean(kNC, na.rm = TRUE),
            median.kNC = median(kNC, na.rm = TRUE),
            total.nuts = max(cumulative.load, na.rm = TRUE),
            mean.kPAR = mean(kPAR, na.rm = TRUE)) 


# get median and mean passage times for each basin
pt.mean = pt.all %>% 
  # group_by(year) %>% 
  # mutate(total.time = sum(minutes)) %>% 
 filter(minutes > 30) %>% 
  ungroup() %>% 
  group_by(year, basin) %>% 
  summarize(mean.minutes = mean(hours, na.rm = TRUE),
            median.minutes = median(minutes, na.rm = TRUE),
            mean.hours = mean(hours, na.rm = TRUE)) %>% 
  rename(Year = year) %>% 
  mutate(mean.days = mean.hours/24) %>% 
  left_join(data.mean, by = "Year")

## add in zooplankton biomass ##
# these are mean values calculated in the Table 1 script
pt.mean = pt.mean %>% 
  mutate(zoop.mean = NA) %>% 
  mutate(zoop.mean = replace(zoop.mean, Year == 2013, 0.19)) %>% 
  mutate(zoop.mean = replace(zoop.mean, Year == 2014, 0.42)) %>% 
  mutate(zoop.mean = replace(zoop.mean, Year == 2015, 0.38)) %>% 
  mutate(zoop.mean = replace(zoop.mean, Year == 2024, 0.47)) %>% 
  mutate(zoop.mean = replace(zoop.mean, Year == 2025, 0.14))




compare.to.zoop = ggplot(pt.mean, aes(x = zoop.mean, y = (mean.days), color = basin)) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +
  geom_point(size = 3.8) +
  geom_text_repel(aes(label = Year), color = "black", size = 3,
                  show.legend = FALSE, max.overlaps = Inf,
                  box.padding = 0.6, point.padding = 0.5,
                  force = 2, min.segment.length = 0, segment.color = NA) +
  facet_wrap(~basin,
             labeller = as_labeller(c(left = "low-chlorophyll",
                                      right = "high-chlorophyll"))) +
  scale_color_manual(values = c(left = "#755A42",
                                right = "#5a6b3a")) +
  labs(x = "mean zooplankton biomass (g m-2)",
       y = "mean passage time (days)") +
  stat_poly_eq(aes(label = paste(..rr.label..)),
               formula = y ~ x, parse = TRUE,
               label.x = "left", label.y = "top", color = "black") +
  theme_classic() +
  scale_y_log10(limits = c(0.2, 10))+
  theme(legend.position = "none",
        strip.text = element_text(size = 12))+
  theme(
    axis.text  = element_text(size = 10),
    axis.title = element_text(size = 12),
    strip.text = element_text(size = 12),
    legend.position = "none"
  ) 





#png("./figures/ASLO 2026/zoop and pt.png", res = 300, units = "in", height = 4, width = 8)

compare.to.zoop

#dev.off()


pos.df <- pt.mean %>%
  group_by(basin) %>%
  summarise(
    x = max(mean.kNC) * 0.86,
    y = -1.8
  )


compare.to.knc = ggplot(pt.mean, aes(x = mean.kNC, y = (mean.days), fill = basin, color = basin)) +
  stat_poly_eq(
    data = subset(pt.mean, basin == "left"),
    aes(label = paste(..rr.label..)),
    formula = y ~ x,
    parse = TRUE,
    geom = "text",
    label.x = pos.df$x[pos.df$basin == "left"],
    label.y = pos.df$y[pos.df$basin == "left"],
    #fill = "white",
    color = "black",
    #label.size = 0.25,
    #label.r = unit(0, "lines"),
    size = 3
  ) +
  stat_poly_eq(
    data = subset(pt.mean, basin == "right"),
    aes(label = paste(..rr.label..)),
    formula = y ~ x,
    parse = TRUE,
    geom = "text",
    label.x = pos.df$x[pos.df$basin == "right"],
    label.y = pos.df$y[pos.df$basin == "right"],
    #fill = "white",
    color = "black",
    #label.size = 0.25,
    #label.r = unit(0, "lines"),
    size = 3
  )+
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +
  geom_point(size = 2, pch = 21, color = "black") +
  geom_text_repel(aes(label = Year), color = "black", size = 2.7,
                  show.legend = FALSE, max.overlaps = Inf,
                  box.padding = 0.2, point.padding = 1.5,
                  force = 2, min.segment.length = 0, segment.color = NA) +
  facet_wrap(~basin, scales = "free_x",
             labeller = as_labeller(c(left = "low-chlorophyll",
                                      right = "high-chlorophyll"))) +
  scale_fill_manual(values = c(left = "#755A42",
                               right = "#5a6b3a")) +
  
  scale_color_manual(values = c(left = "#755A42",
                                right = "#5a6b3a")) +
  labs(x = "non-chl light attenuation (kNC)",
       y = "mean passage time (days)") +
  theme_bw() +
  scale_y_log10(
    limits = c(0.01, 100),
    breaks = c(0.01, 0.1, 1, 10, 100),
    labels = c("0.01", "0.1", "1", "10", "100"))+
  facetted_pos_scales(
    x = list(
      basin == "left"  ~ scale_x_continuous(breaks = c(0.9, 1.2, 1.5, 1.8)),
      basin == "right" ~ scale_x_continuous(breaks = c(0.9, 1.2, 1.5))
    )
  )+
  theme(legend.position = "none",
        strip.text = element_text(size = 12))+
  theme(
    axis.text  = element_text(size = 10),
    axis.title = element_text(size = 12),
    strip.text = element_blank(),
    legend.position = "none",
    panel.grid = element_blank()
  ) +
  theme(plot.margin = margin(r = 1))


ggarrange(ptimes, compare.to.knc, nrow = 2, ncol = 1)




### compare to kPAR


compare.to.kPAR = ggplot(pt.mean, aes(x = mean.kPAR, y = (mean.days), fill = basin, color = basin)) +
  stat_poly_eq(
    data = subset(pt.mean, basin == "left"),
    aes(label = paste(..rr.label..)),
    formula = y ~ x,
    parse = TRUE,
    geom = "text",
    label.x = pos.df$x[pos.df$basin == "left"],
    label.y = pos.df$y[pos.df$basin == "left"],
    #fill = "white",
    color = "black",
    #label.size = 0.25,
    #label.r = unit(0, "lines"),
    size = 3
  ) +
  stat_poly_eq(
    data = subset(pt.mean, basin == "right"),
    aes(label = paste(..rr.label..)),
    formula = y ~ x,
    parse = TRUE,
    geom = "text",
    label.x = pos.df$x[pos.df$basin == "right"],
    label.y = pos.df$y[pos.df$basin == "right"],
    #fill = "white",
    color = "black",
    #label.size = 0.25,
    #label.r = unit(0, "lines"),
    size = 3
  )+
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +
  geom_point(size = 2, pch = 21, color = "black") +
  geom_text_repel(aes(label = Year), color = "black", size = 2.7,
                  show.legend = FALSE, max.overlaps = Inf,
                  box.padding = 0.2, point.padding = 1.5,
                  force = 2, min.segment.length = 0, segment.color = NA) +
  facet_wrap(~basin, scales = "free_x",
             labeller = as_labeller(c(left = "low-chlorophyll",
                                      right = "high-chlorophyll"))) +
  scale_fill_manual(values = c(left = "#755A42",
                               right = "#5a6b3a")) +
  
  scale_color_manual(values = c(left = "#755A42",
                                right = "#5a6b3a")) +
  labs(x = "light attenuation (kPAR)",
       y = "mean passage time (days)",
       title = "Tuesday") +
  theme_bw() +
  scale_y_log10(
    limits = c(0.01, 100),
    breaks = c(0.01, 0.1, 1, 10, 100),
    labels = c("0.01", "0.1", "1", "10", "100"))+
  facetted_pos_scales(
    x = list(
      basin == "left"  ~ scale_x_continuous(breaks = c(0.9, 1.2, 1.5, 1.8)),
      basin == "right" ~ scale_x_continuous(breaks = c(0.9, 1.2, 1.5))
    )
  )+
  theme(legend.position = "none",
        strip.text = element_text(size = 12))+
  theme(
    axis.text  = element_text(size = 10),
    axis.title = element_text(size = 12),
    strip.text = element_blank(),
    legend.position = "none",
    panel.grid = element_blank()
  ) +
  theme(plot.margin = margin(r = 1))

compare.to.kPAR

### compare to kNC for DEFENSE

#png("./figures/DEFENSE/pt to knc.png", height = 3.04, width = 5.93, units = "in", res = 300)

ggplot(pt.mean, aes(x = mean.kNC, y = (mean.days), color = basin)) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +
  geom_point(size = 4) +
  geom_text_repel(aes(label = Year), color = "black", size = 3,
                  show.legend = FALSE, max.overlaps = Inf,
                  box.padding = 0.6, point.padding = 0.5,
                  force = 2, min.segment.length = 0, segment.color = NA) +
  facet_wrap(~basin,
             labeller = as_labeller(c(left = "low-chlorophyll",
                                      right = "high-chlorophyll"))) +
  scale_color_manual(values = c(left = "#755A42",
                                right = "#5a6b3a")) +
  labs(x = "mean non-chl light attenuation (kNC)",
       y = "mean passage time (days)") +
  stat_poly_eq(aes(label = paste(..rr.label..)),
               formula = y ~ x, parse = TRUE,
               label.x = "left", label.y = "top", color = "black") +
  theme_classic() +
  scale_y_log10(limits = c(0.2, 10))+
  theme(legend.position = "none",
        strip.text = element_text(size = 12))+
  theme(
    axis.text  = element_text(size = 12),
    axis.title = element_text(size = 12),
    strip.text = element_blank(),
    legend.position = "none",
    strip.background = element_blank()
  ) 


#dev.off()


## compare to P loading total
# add known max P loading to pt.mean
pt.mean = pt.mean %>% 
  mutate(max.P = case_when(Year == 2013 ~ 219,
                           Year == 2014 ~ 267,
                           Year == 2015 ~ 267,
                           Year == 2024 ~ 216,
                           Year == 2025 ~ 270))


pandpt =  ggplot(pt.mean, aes(x = max.P, y = (mean.days), color = basin)) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +
  geom_point(size = 3.8) +
  geom_text_repel(aes(label = Year), color = "black", size = 3,
                  show.legend = FALSE, max.overlaps = Inf,
                  box.padding = 0.6, point.padding = 0.5,
                  force = 2, min.segment.length = 0, segment.color = NA) +
  facet_wrap(~basin,
             labeller = as_labeller(c(left = "low-chlorophyll",
                                      right = "high-chlorophyll"))) +
  scale_color_manual(values = c(left = "#755A42",
                                right = "#5a6b3a")) +
  labs(x = "cumulative P added (mg m-2 d-1)",
       y = "mean passage time (days)") +
  stat_poly_eq(aes(label = paste(..rr.label..)),
               formula = y ~ x, parse = TRUE,
               label.x = "left", label.y = "top", color = "black") +
  theme_classic() +
  scale_y_log10(limits = c(0.2, 10))+
  theme(legend.position = "none",
        strip.text = element_text(size = 12))+
  theme(
    axis.text  = element_text(size = 12),
    axis.title = element_text(size = 12),
    strip.text = element_text(size = 12),
    legend.position = "none"
  ) 


#png("./figures/ASLO 2026/total nutrients and pt.png", res = 300, width = 8, height = 4, units = "in")
pandpt
#dev.off()
### add in global effective potential
ep.global = read.csv("./results/DDJ results Tuesday ARIMA-correced.csv") %>% 
  mutate(year = "all years")

# set the threshold to the one found by DDJ
#eq = xeq[2]
eq <- 0.3255



ep.global2 <- ep.global %>%
  mutate(
    side = ifelse(X <= eq, "left", "right")
  )

ymax <- max(ep.global2$efective.potential)



global.ep.plot <- ggplot(
  ep.global2,
  aes(
    x = X,
    y = efective.potential,
    fill = side,
    group = interaction(year, side)
  )
) +
  geom_area(
    alpha = 0.9,
    color = "black",
    size = 1.0,
    position = "identity"
  ) +
  scale_fill_manual(
    values = c(
      left  = "#755A42",
      right = "#5a6b3a"
    )
  ) +
  theme_bw() +
  labs(
    x = "chlorophyll (standard level)",
    y = "effective potential",
    title = "Tuesday Lake (experimental)"
  ) +
  theme(
    axis.text  = element_text(size = 10),
    axis.title = element_text(size = 12),
    strip.text = element_text(size = 12),
    legend.position = "none",
    panel.grid = element_blank(),
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
  ) +
  xlim(-7, 8) +
  theme(plot.margin = margin(r = 1)) +
  annotate(
    "rect",
    xmin = -7, xmax = eq,
    ymin = ymax * 0.95, ymax = ymax * 1.16,
    fill = "#755A42",
    color = "black"
  ) +
  annotate(
    "rect",
    xmin = eq, xmax = 8,
    ymin = ymax * 0.95, ymax = ymax * 1.16,
    fill = "#5a6b3a",
    color = "black"
  ) +
  annotate(
    "text",
    x = (-7 + eq) / 2,
    y = ymax * 1.07,
    label = "low-chlorophyll",
    color = "white",
    fontface = "bold",
    size = 3.5
  ) +
  annotate(
    "text",
    x = (eq + 8) / 2,
    y = ymax * 1.07,
    label = "high-chlorophyll",
    color = "white",
    fontface = "bold",
    size = 3.5
  )


# png("./figures/draft 2026-02-27/passage times.png", res = 300, height = 8, width = 6, units = "in") 


tues.all = ggarrange(global.ep.plot, ptimes, compare.to.knc, nrow = 3, ncol = 1)























####################################################################################################################################################
# dev.off()

ggplot(pt.mean, aes(x = total.nuts, y = (mean.minutes), color = basin))+
  geom_point(size = 5)+
  geom_smooth(method = "lm", se = FALSE)+
  facet_wrap(~basin)+
  stat_poly_eq(
    aes(label = paste(..rr.label..)),
    formula = y ~ x,
    parse = TRUE,
    label.x = "left",
    label.y = "top"
  ) +
  geom_text_repel(aes(label = Year),
                  size = 4,
                  show.legend = FALSE) 




ggplot(pt.mean, aes(x = mean.kPAR, y = log10(mean.minutes), color = basin))+
  geom_point(size = 5)+
  geom_smooth(method = "lm", se = FALSE)+
  facet_wrap(~basin)+
  stat_poly_eq(
    aes(label = paste(..rr.label..)),
    formula = y ~ x,
    parse = TRUE,
    label.x = "left",
    label.y = "top"
  ) +
  geom_text_repel(aes(label = Year),
                  size = 4,
                  show.legend = FALSE) 

#### # How is time allocated among passage events?  Right--
# sET_Tues_r = sort(ET_Tues_r)
# sumET_Tues_r = sum(ET_Tues_r)
# pET_Tues_r = sET_Tues_r/sumET_Tues_r # proportion of total passage time per event
# cpET_Tues_r = cumsum(pET_Tues_r) # cumulative proportions
# #
# tET_Tues_r = sET_Tues_r*pET_Tues_r  # time spent in each passage event
# ctET_Tues_r = cumsum(tET_Tues_r) # cumulative time spent in passage events
# ptET_Tues_r = ctET_Tues_r/sumET_Tues_r # proportion of time for each passage


pt.all.r = pt.all %>% filter(basin == "right") %>% 
  group_by(year) %>% 
  mutate(sum = sum(minutes)) %>% 
  mutate(prop = minutes/sum) %>% 
  arrange(minutes) %>% 
  mutate(cumulative = cumsum(prop))


pt.all.l = pt.all %>% filter(basin == "left") %>% 
  group_by(year) %>% 
  mutate(sum = sum(minutes)) %>% 
  mutate(prop = minutes/sum) %>% 
  arrange(minutes) %>% 
  mutate(cumulative = cumsum(prop))


ggplot(pt.all.r, aes(x = log10(minutes), y = cumulative, color = as.factor(year)))+
  geom_point(size = 2)+
  geom_line(size = 1)+
  theme_bw()

ggplot(pt.all.r, aes(x = log10(minutes), y = prop, color = as.factor(year)))+
  geom_point(size = 2)+
  geom_line(size = 1)+
  theme_bw()




ggplot(pt.all.r, aes(x = (minutes), y = cumulative, color = as.factor(year)))+
  geom_point(size = 2)+
  geom_line(size = 1)+
  theme_bw()+
  scale_x_log10()

ggplot(pt.all.r, aes(x = (minutes), y = prop, color = as.factor(year)))+
  geom_point(size = 2)+
  geom_line(size = 1)+
  theme_bw()+
  scale_x_log10()





l.cum =  ggplot(pt.all.l, aes(x = (minutes), y = cumulative, color = as.factor(year)))+
  geom_point(size = 2)+
  geom_line(size = 1)+
  theme_bw()+
  scale_x_log10()+
  labs(x = "Left ET, Minutes", y = "Cumulative Proportion")+
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16))



l.prop = ggplot(pt.all.l, aes(x = (minutes), y = prop, color = as.factor(year)))+
  geom_point(size = 2)+
  geom_line(size = 1)+
  theme_bw()+
  scale_x_log10()+
  labs(x = "Left ET, Minutes", y = "Proportion of Time")+
  theme( axis.text = element_text(size = 14), axis.title = element_text(size = 16))


ggarrange(l.prop, l.cum, common.legend = T)





r.cum =  ggplot(pt.all.r, aes(x = (minutes), y = cumulative, color = as.factor(year)))+
  geom_point(size = 2)+
  geom_line(size = 1)+
  theme_bw()+
  scale_x_log10()+
  labs(x = "Right ET, Minutes", y = "Cumulative Proportion")+
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16))



r.prop = ggplot(pt.all.r, aes(x = (minutes), y = prop, color = as.factor(year)))+
  geom_point(size = 2)+
  geom_line(size = 1)+
  theme_bw()+
  scale_x_log10()+
  labs(x = "Right ET, Minutes", y = "Proportion of Time")+
  theme( axis.text = element_text(size = 14), axis.title = element_text(size = 16))


ggarrange(r.prop, r.cum, common.legend = T)



####===============================================================================================================================================================================================================#
#### plot with horizontal bars when the ecosystem was in different states ####

# Load DLM result
#save(useBGA,Tstep,X.dlm,level,levelsd,stdlevel,file=Fname)  
# load(file='DLMresult_YSI_Peter19.Rdata')
load('./scripts/Langevin/HF Langevin/Langevin analysis/Finalized 2026-01-16/DLMresult_HYLB_Tuesday_ALL_Chl_Predicted to Manual Scale 098 NOISY.Rdata')

# thin the data to match DDJ
aropt=2
nx = length(stdlevel)
ikeep = seq(1, nx, by = aropt)

stdlevel = stdlevel[ikeep]
Tstep = Tstep[ikeep]

# put Tstep and stdlevel into a dataset
dat0 = as.data.frame(cbind(Tstep,stdlevel)) %>% 
  mutate(year = trunc(Tstep))


# convert Tstep back to a time
T.all = read.csv("./data/formatted data/HF data/Sonde correction/Predicted Tuesday HYLB on Manual Scale log-trans NOISY.csv") %>% 
  mutate(Lake = "T") %>% 
  arrange(datetime)



### Create a Tscore that combines year and DoY
mindoy = min(T.all$DoY, na.rm = TRUE)
maxdoy = max(T.all$DoY, na.rm = TRUE)

dat0 = dat0 %>% 
  mutate(doy =  mindoy + (dat0$Tstep - dat0$year) * (maxdoy - mindoy + 1))

# Load DDJ data and apply to dataframe
load("./scripts/Langevin/HF Langevin/Langevin analysis/Results different data corrections/DDJ_HYLB_Tuesday_DLM_GLOBAL_Log_Chl_Predicted to Manual Scale POOLED AND GLOBAL NOISY 098 THINNED.Rdata")

low = xeq2[1]
thresh = xeq2[2]
high = xeq2[3]


dat0  = dat0 %>% 
  mutate(basin = case_when(stdlevel > thresh~"high-chlorophyll",
                           stdlevel < thresh~"low-chlorophyll")) %>% 
  mutate(equilibria = case_when(stdlevel > thresh~high,
                                stdlevel < thresh~low))




dat0 = dat0 %>%
  arrange(year, doy) %>%
  mutate(basin = case_when(stdlevel > thresh ~ "high-chlorophyll",
                           stdlevel < thresh ~ "low-chlorophyll")) %>%
  # create a group that breaks line at color changes
  group_by(year) %>%
  mutate(group = cumsum(basin != lag(basin, default = first(basin)))) %>%
  ungroup()

ggplot(dat0, aes(x = doy, y = stdlevel, group = group, color = basin)) +
  geom_line(size = 1, alpha = 0.75) +
  geom_line(aes(x = doy, y = equilibria), size = 2, color = "black") +
  geom_hline(yintercept = thresh, linetype = "dashed") +
  scale_color_manual(values = c("high-chlorophyll" = "#5a6b3a", "low-chlorophyll" = "#533113")) +
  facet_wrap(~year) +
  theme_classic() +
  labs(x = "DOY", y = "stdlevel")+
  theme(legend.position = "none")


#### Add in the critical transitions ####
ct = read.csv("./scripts/Multivariate DLM/eigenvalues 2026-01-27.csv") %>% 
  filter(delta == 0.95 & eigvals >= 1) %>% 
  rename(year = Year)

# locatino of critical transitions with manual results
# "./scripts/Multivariate DLM/Tuesday MANUAL eigenvalues 2026-02-27 NO BGA log-transformed chl only.csv"

ct = read.csv("./scripts/Multivariate DLM/Tuesday MANUAL eigenvalues 2026-02-27 NO BGA log-transformed chl only.csv") %>% 
  filter(delta == 0.90 & eigvals >= 1) %>% 
  rename(year = Year)

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
  scale_color_manual(values = c("high-chlorophyll" = "#5a6b3a", "low-chlorophyll" = "#533113")) +
  facet_wrap(~year) +
  theme_classic() +
  labs(x = "DOY", y = "Chlorophyll (standard level)") +
  theme(legend.position = "none")


png("./figures/DEFENSE/time series standardized")




ct.all = read.csv("./scripts/Multivariate DLM/eigenvalues 2026-01-27.csv") %>% 
  filter(delta == 0.94) %>% 
  rename(year = Year)

ggplot(ct.all, aes(x = doy, y = eigvals, color = as.factor(year)))+
  geom_point()+
  geom_line()+
  facet_wrap(~year, ncol = 1, nrow = 5)+
  theme_bw()+
  labs(title = "log-trans, Delta = 0.94")+
  theme(axis.text = element_text(size = 12))+
  geom_hline(yintercept = 1)




ct.all = read.csv("./scripts/Multivariate DLM/eigenvalues 2026-01-27.csv") %>% 
  filter(delta == 0.90) %>% 
  rename(year = Year)

ggplot(ct.all, aes(x = doy, y = eigvals, color = as.factor(year)))+
  geom_point()+
  geom_line()+
  facet_wrap(~year, ncol = 1, nrow = 5)+
  theme_bw()+
  labs(title = "log-trans, Delta = 0.90")+
  theme(axis.text = element_text(size = 12))+
  geom_hline(yintercept = 1)



ct.all = read.csv("./scripts/Multivariate DLM/eigenvalues 2026-01-27 NO BGA.csv") %>% 
  filter(delta == 0.90) %>% 
  rename(year = Year)

ggplot(ct.all, aes(x = doy, y = eigvals, color = as.factor(year)))+
  geom_point()+
  geom_line()+
  facet_wrap(~year, ncol = 1, nrow = 5)+
  theme_bw()+
  labs(title = "log-trans, NO BGA Delta = 0.90")+
  theme(axis.text = element_text(size = 12))+
  geom_hline(yintercept = 1)


ct.no.bga = read.csv("./scripts/Multivariate DLM/eigenvalues 2026-01-27 NO BGA log-transformed.csv") %>% 
  filter(delta == 0.94 & eigvals >= 1) %>% 
  rename(year = Year)

ggplot(ct.all, aes(x = doy, y = eigvals, color = as.factor(year)))+
  geom_point()+
  geom_line()+
  facet_wrap(~year, ncol = 1, nrow = 5)+
  theme_bw()+
  labs(title = "log-trans, NO BGA Delta = 0.94")+
  theme(axis.text = element_text(size = 12))+
  geom_hline(yintercept = 1)



ggplot(dat0, aes(x = doy, y = stdlevel, group = group, color = basin)) +
  geom_line(size = 1, alpha = 0.75) +
  geom_line(aes(x = doy, y = equilibria), size = 2, color = "black") +
  geom_hline(yintercept = thresh, linetype = "dashed") +
  # Add vertical lines for eigenvalues > 1
  geom_vline(
    data = ct.no.bga %>% filter(eigvals > 1),
    aes(xintercept = doy),
    color = "steelblue3",
    linetype = "solid",
    size = 1
  ) +
  scale_color_manual(values = c("high-chlorophyll" = "#5a6b3a", "low-chlorophyll" = "#533113")) +
  facet_wrap(~year) +
  theme_classic() +
  labs(x = "DOY", y = "stdlevel") +
  theme(legend.position = "none")



#### DEFENSE FIGURES #####

png("./figures/DEFENSE/chl time series no ct no pt.png", width = 7.5, height = 5.5, res = 300, units = "in")


ggplot(dat0, aes(x = doy, y = stdlevel)) +
  geom_line(size = 1, alpha = 0.75) +
  # geom_line(aes(x = doy, y = equilibria), size = 2, color = "black") +
  # geom_hline(yintercept = thresh, linetype = "dashed") +
  # Add vertical lines for eigenvalues > 1
  # geom_vline(
  #   data = ct %>% filter(eigvals > 1),
  #   aes(xintercept = doy),
  #   color = "steelblue3",
  #   linetype = "solid",
  #   size = 1
  # ) +
  # scale_color_manual(values = c("high-chlorophyll" = "#5a6b3a", "low-chlorophyll" = "#533113")) +
facet_wrap(~year) +
  theme_classic() +
  labs(x = "Date", y = "Chlorophyll (standard level)") +
  theme(legend.position = "none")+
  scale_x_continuous(
    breaks = c(121, 152, 182, 213, 244),
    labels = c("May", "Jun", "Jul", "Aug", "Sep")
  )+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14),
        strip.text = element_text(size = 12))

dev.off()





png("./figures/DEFENSE/chl time series no ct no pt.png", width = 7.5, height = 5.5, res = 300, units = "in")


ggplot(dat0, aes(x = doy, y = stdlevel)) +
  geom_line(size = 1, alpha = 0.75) +
  # geom_line(aes(x = doy, y = equilibria), size = 2, color = "black") +
  # geom_hline(yintercept = thresh, linetype = "dashed") +
  # Add vertical lines for eigenvalues > 1
  # geom_vline(
  #   data = ct %>% filter(eigvals > 1),
  #   aes(xintercept = doy),
  #   color = "steelblue3",
  #   linetype = "solid",
  #   size = 1
  # ) +
  #  scale_color_manual(values = c("high-chlorophyll" = "#5a6b3a", "low-chlorophyll" = "#533113")) +
facet_wrap(~year) +
  theme_classic() +
  labs(x = "Date", y = "Chlorophyll (standard level)") +
  theme(legend.position = "none")+
  scale_x_continuous(
    breaks = c(121, 152, 182, 213, 244),
    labels = c("May", "Jun", "Jul", "Aug", "Sep")
  )+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14),
        strip.text = element_text(size = 12))

dev.off()





png("./figures/DEFENSE/chl time series no ct threshold.png", width = 7.5, height = 5.5, res = 300, units = "in")


ggplot(dat0, aes(x = doy, y = stdlevel)) +
  geom_line(size = 1, alpha = 0.75) +
  # geom_line(aes(x = doy, y = equilibria), size = 2, color = "black") +
  geom_hline(yintercept = thresh, linetype = "dashed", size = 1.2) +
  # Add vertical lines for eigenvalues > 1
  # geom_vline(
  #   data = ct %>% filter(eigvals > 1),
  #   aes(xintercept = doy),
  #   color = "steelblue3",
  #   linetype = "solid",
  #   size = 1
  # ) +
  #scale_color_manual(values = c("high-chlorophyll" = "#5a6b3a", "low-chlorophyll" = "#533113")) +
  facet_wrap(~year) +
  theme_classic() +
  labs(x = "Date", y = "Chlorophyll (standard level)") +
  theme(legend.position = "none")+
  scale_x_continuous(
    breaks = c(121, 152, 182, 213, 244),
    labels = c("May", "Jun", "Jul", "Aug", "Sep")
  )+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14),
        strip.text = element_text(size = 12))

dev.off()



png("./figures/DEFENSE/chl time series no ct threshold colored states.png", width = 7.5, height = 5.5, res = 300, units = "in")


ggplot(dat0, aes(x = doy, y = stdlevel, color = basin, group = group)) +
  geom_line(size = 1, alpha = 0.75) +
  # geom_line(aes(x = doy, y = equilibria), size = 2, color = "black") +
  geom_hline(yintercept = thresh, linetype = "dashed", size = 1.2) +
  # Add vertical lines for eigenvalues > 1
  # geom_vline(
  #   data = ct %>% filter(eigvals > 1),
  #   aes(xintercept = doy),
  #   color = "steelblue3",
  #   linetype = "solid",
  #   size = 1
  # ) +
  scale_color_manual(values = c("high-chlorophyll" = "#5a6b3a", "low-chlorophyll" = "#533113")) +
  facet_wrap(~year) +
  theme_classic() +
  labs(x = "Date", y = "Chlorophyll (standard level)") +
  theme(legend.position = "none")+
  scale_x_continuous(
    breaks = c(121, 152, 182, 213, 244),
    labels = c("May", "Jun", "Jul", "Aug", "Sep")
  )+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14),
        strip.text = element_text(size = 12))

dev.off()






png("./figures/DEFENSE/chl time series threshold colored states.png", width = 7.5, height = 5.5, res = 300, units = "in")


ggplot(dat0, aes(x = doy, y = stdlevel, color = basin, group = group)) +
  geom_line(size = 1, alpha = 0.75) +
  geom_line(aes(x = doy, y = equilibria), size = 2, color = "black") +
  geom_hline(yintercept = thresh, linetype = "dashed", size = 1.2) +
  # Add vertical lines for eigenvalues > 1
  # geom_vline(
  #   data = ct %>% filter(eigvals > 1),
  #   aes(xintercept = doy),
  #   color = "steelblue3",
  #   linetype = "solid",
  #   size = 1
  # ) +
  scale_color_manual(values = c("high-chlorophyll" = "#5a6b3a", "low-chlorophyll" = "#533113")) +
  facet_wrap(~year) +
  theme_classic() +
  labs(x = "Date", y = "Chlorophyll (standard level)") +
  theme(legend.position = "none")+
  scale_x_continuous(
    breaks = c(121, 152, 182, 213, 244),
    labels = c("May", "Jun", "Jul", "Aug", "Sep")
  )+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14),
        strip.text = element_text(size = 12))

dev.off()





png("./figures/DEFENSE/chl time series threshold colored states ct.png", width = 7.5, height = 5.5, res = 300, units = "in")


ggplot(dat0, aes(x = doy, y = stdlevel, color = basin, group = group)) +
  geom_line(size = 1, alpha = 0.75) +
  geom_line(aes(x = doy, y = equilibria), size = 2, color = "black") +
  geom_hline(yintercept = thresh, linetype = "dashed", size = 1.2) +
  # Add vertical lines for eigenvalues > 1
  geom_vline(
    data = ct %>% filter(eigvals > 1),
    aes(xintercept = doy),
    color = "steelblue3",
    linetype = "solid",
    size = 1
  ) +
  scale_color_manual(values = c("high-chlorophyll" = "#5a6b3a", "low-chlorophyll" = "#533113")) +
  facet_wrap(~year) +
  theme_classic() +
  labs(x = "Date", y = "Chlorophyll (standard level)") +
  theme(legend.position = "none")+
  scale_x_continuous(
    breaks = c(121, 152, 182, 213, 244),
    labels = c("May", "Jun", "Jul", "Aug", "Sep")
  )+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14),
        strip.text = element_text(size = 12))

dev.off()


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
  scale_color_manual(values = c("high-chlorophyll" = "#5a6b3a", "low-chlorophyll" = "#533113")) +
  facet_wrap(~year) +
  theme_classic() +
  labs(x = "Date", y = "Chlorophyll (standard level)") +
  theme(legend.position = "none")+
  scale_x_continuous(
    breaks = c(121, 152, 182, 213, 244),
    labels = c("May", "Jun", "Jul", "Aug", "Sep")
  )+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14),
        strip.text = element_text(size = 12))


##### FIGURES 2026-02-27 ######

ct = read.csv("./scripts/Multivariate DLM/Tuesday MANUAL eigenvalues 2026-02-27 NO BGA log-transformed chl only.csv") %>% 
  filter(delta == 0.90 & eigvals >= 1) %>% 
  rename(year = Year)

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
  scale_color_manual(values = c("high-chlorophyll" = "#5a6b3a", "low-chlorophyll" = "#533113")) +
  facet_wrap(~year) +
  theme_classic() +
  labs(x = "Date", y = "Chlorophyll (standard level)") +
  theme(legend.position = "none")+
  scale_x_continuous(
    breaks = c(121, 152, 182, 213, 244),
    labels = c("May", "Jun", "Jul", "Aug", "Sep")
  )+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14),
        strip.text = element_text(size = 12))


### plot the critical transitions on top of the actual data for Tuesday ### 

eig = read.csv("./scripts/Multivariate DLM/Tuesday MANUAL eigenvalues 2026-02-27 NO BGA log-transformed chl only.csv") %>% 
  filter(delta == 0.90)

ct = read.csv("./scripts/Multivariate DLM/Tuesday MANUAL eigenvalues 2026-02-27 NO BGA log-transformed chl only.csv") %>% 
  filter(delta == 0.90 & eigvals >= 1)

Year = c(2013, 2014, 2015, 2024, 2025)
start = c(154, 153, 152, 162, 154)
end = c(238, 241, 240, 233, 198)

nut.additions = data.frame(Year, start, end)


daily_mean = read.csv("./scripts/Multivariate DLM/Tuesday multivariate daily average MANUAL CHL 2026-02-27.csv")




eig_plot = 
  ggplot(eig, aes(x = doy, y = eigvals)) + 
  #geom_point(color = 'darkblue')+
  geom_rect(
    data = nut.additions,
    aes(
      xmin = start,
      xmax = end,
      ymin = -Inf,
      ymax = Inf
    ),
    fill = "lightblue",
    alpha = 0.5,
    inherit.aes = FALSE
  ) +
  geom_path(color = 'darkblue', size = 1.2, alpha = 0.7) +
  theme_bw() + 
  xlab("") + ylab("Eigenvalue") +# ggtitle("Eigenvalues")+
  facet_wrap(~Year, nrow = 1, ncol = 5)+
  geom_hline(yintercept = 1, linetype = "dashed")+
  scale_x_continuous(
    breaks = c(121, 152, 182, 213),
    labels = c("May", "Jun", "Jul", "Aug")
  )+
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )+
  theme(
    plot.margin = margin(2, 5, 2, 5),
    panel.spacing = unit(0.1, "lines")
  )

chl_plot = 
  ggplot(daily_mean, aes(x = doy, y = (chl_mean))) + 
  #geom_point(color = '#117733')+
  geom_vline(
    data = ct %>% filter(eigvals > 1),
    aes(xintercept = doy),
    color = "grey55",
    linetype = "solid",
    size = 1) +
  # White "stroke" layer
  geom_path(color = "white", size = 2) +
  geom_path(color = '#117733', size = 1.2) +
  theme_bw() + 
  xlab("") + ylab("Chlorophyll (μg/L)") + #ggtitle("Chlorophyll, Log Transformed")+
  facet_wrap(~Year, nrow = 1, ncol = 5)+
  scale_y_log10()+
  scale_x_continuous(
    breaks = c(121, 152, 182, 213),
    labels = c("May", "Jun", "Jul", "Aug")
  )+
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )+theme(
    plot.margin = margin(2, 5, 2, 5),
    panel.spacing = unit(0.1, "lines")
  )+theme(
    strip.text = element_blank(),
    strip.background = element_blank()
  )


do_plot = 
  ggplot(daily_mean, aes(x = doy, y = do_mean)) + 
  geom_vline(
    data = ct %>% filter(eigvals > 1),
    aes(xintercept = doy),
    color = "grey55",
    linetype = "solid",
    size = 1
  )+
  # geom_point(color = '#882255')+
  # White "stroke" layer
  geom_path(color = "white", size = 2) +
  geom_path(color = '#882255', size = 1.2) +
  theme_bw() + 
  xlab("") + ylab("DO (% saturation)") + #ggtitle("DO (% Saturation)")+
  facet_wrap(~Year, nrow = 1, ncol = 5)+
  scale_x_continuous(
    breaks = c(121, 152, 182, 213),
    labels = c("May", "Jun", "Jul", "Aug")
  )+
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )+theme(
    plot.margin = margin(2, 5, 2, 5),
    panel.spacing = unit(0.1, "lines")
  )+
  theme(
    strip.text = element_blank(),
    strip.background = element_blank()
  )


ph_plot = 
  ggplot(daily_mean, aes(x = doy, y = ph_mean)) + 
  geom_vline(
    data = ct %>% filter(eigvals > 1),
    aes(xintercept = doy),
    color = "grey55",
    linetype = "solid",
    size = 1
  ) +
  #geom_point(color = '#CE9F2E')+
  # White "stroke" layer
  geom_path(color = "white", size = 2) +
  geom_path(color = '#CE9F2E', size = 1.2) +
  theme_bw() + 
  facet_wrap(~Year, nrow = 1, ncol = 5)+
  xlab("") + ylab("pH") + #ggtitle("pH")+
  scale_x_continuous(
    breaks = c(121, 152, 182, 213),
    labels = c("May", "Jun", "Jul", "Aug")
  )+theme(
    strip.text = element_blank(),
    strip.background = element_blank()
  )


ggarrange(eig_plot, chl_plot, do_plot, ph_plot, 
          nrow = 4, ncol = 1, align = 'v')



### get the first critical transition by year
ct.min = ct %>% 
  group_by(Year) %>% 
  summarize(min(doy, na.rm = TRUE))



######## PLOT FOR DEFENSE ###########


eig_plot = 
  ggplot(eig, aes(x = doy, y = eigvals)) + 
  #geom_point(color = 'darkblue')+
  # geom_rect(
  #   data = nut.additions,
  #   aes(
  #     xmin = start,
  #     xmax = end,
  #     ymin = -Inf,
  #     ymax = Inf
  #   ),
  #   fill = "lightblue",
  #   alpha = 0.5,
#   inherit.aes = FALSE
# ) +
geom_vline(
  data = ct %>% filter(eigvals > 1),
  aes(xintercept = doy),
  color = "#4E95D1",
  linetype = "solid",
  size = 1
) +
  geom_path(color = "white", size = 4) +
  geom_path(color = 'black', size = 1, alpha = 0.7) +
  theme_bw() + 
  xlab("") + ylab("Eigenvalue") +# ggtitle("Eigenvalues")+
  facet_wrap(~Year, nrow = 1, ncol = 5)+
  geom_hline(yintercept = 1, linetype = "dashed")+
  scale_x_continuous(
    breaks = c(121, 152, 182, 213),
    labels = c("May", "Jun", "Jul", "Aug")
  )+
  
  theme(
    plot.margin = margin(2, 5, 2, 5),
    panel.spacing = unit(0.1, "lines"),
    strip.backgroun = element_blank(),
    strip.text = element_blank()
  )

chl_plot = 
  ggplot(daily_mean, aes(x = doy, y = (chl_mean))) + 
  #geom_point(color = '#117733')+
  # geom_vline(
  #   data = ct %>% filter(eigvals > 1),
  #   aes(xintercept = doy),
  #   color = "grey55",
  #   linetype = "solid",
  #   size = 1) +
  # White "stroke" layer
  geom_path(color = "white", size = 1.5) +
  geom_path(color = '#117733', size = 1) +
  theme_bw() + 
  xlab("") + ylab("Chlorophyll (μg/L)") + #ggtitle("Chlorophyll, Log Transformed")+
  facet_wrap(~Year, nrow = 1, ncol = 5)+
  scale_y_log10()+
  scale_x_continuous(
    breaks = c(121, 152, 182, 213),
    labels = c("May", "Jun", "Jul", "Aug")
  )+
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )+theme(
    plot.margin = margin(2, 5, 2, 5),
    panel.spacing = unit(0.1, "lines")
  )


do_plot = 
  ggplot(daily_mean, aes(x = doy, y = do_mean)) + 
  # geom_vline(
  #   data = ct %>% filter(eigvals > 1),
  #   aes(xintercept = doy),
  #   color = "grey55",
  #   linetype = "solid",
  #   size = 1
  # )+
  # geom_point(color = '#882255')+
  # White "stroke" layer
  geom_path(color = "white", size = 1.5) +
  geom_path(color = '#882255', size = 1) +
  theme_bw() + 
  xlab("") + ylab("DO (% saturation)") + #ggtitle("DO (% Saturation)")+
  facet_wrap(~Year, nrow = 1, ncol = 5)+
  scale_x_continuous(
    breaks = c(121, 152, 182, 213),
    labels = c("May", "Jun", "Jul", "Aug")
  )+
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )+theme(
    plot.margin = margin(2, 5, 2, 5),
    panel.spacing = unit(0.1, "lines")
  )+
  theme(
    strip.text = element_blank(),
    strip.background = element_blank()
  )


ph_plot = 
  ggplot(daily_mean, aes(x = doy, y = ph_mean)) + 
  # geom_vline(
  #   data = ct %>% filter(eigvals > 1),
  #   aes(xintercept = doy),
  #   color = "grey55",
  #   linetype = "solid",
  #   size = 1
  # ) +
  #geom_point(color = '#CE9F2E')+
  # White "stroke" layer
  geom_path(color = "white", size = 1.5) +
  geom_path(color = '#CE9F2E', size = 1) +
  theme_bw() + 
  facet_wrap(~Year, nrow = 1, ncol = 5)+
  xlab("") + ylab("pH") + #ggtitle("pH")+
  scale_x_continuous(
    breaks = c(121, 152, 182, 213),
    labels = c("May", "Jun", "Jul", "Aug")
  )+theme(
    strip.text = element_blank(),
    strip.background = element_blank()
  )


data =  ggarrange(chl_plot, do_plot, ph_plot,
                  nrow = 3, ncol = 1, align = 'v')

png("./figures/DEFENSE/eigenvalues.png", height = 6, width = 9.16, units = "in", res = 300)

ggarrange(data, eig_plot, align = "v", nrow = 2, ncol = 1)

dev.off()

eig_plot












##### BALL AND CUP ANIMATION ######

# Load DLM result
#save(useBGA,Tstep,X.dlm,level,levelsd,stdlevel,file=Fname)  
# load(file='DLMresult_YSI_Peter19.Rdata')
load('./scripts/Langevin/HF Langevin/Langevin analysis/Finalized 2026-01-16/DLMresult_HYLB_Tuesday_ALL_Chl_Predicted to Manual Scale 098 NOISY.Rdata')

# thin the data to match DDJ
aropt=2
nx = length(stdlevel)
ikeep = seq(1, nx, by = aropt)

stdlevel = stdlevel[ikeep]
Tstep = Tstep[ikeep]

# put Tstep and stdlevel into a dataset
dat0 = as.data.frame(cbind(Tstep,stdlevel)) %>% 
  mutate(year = trunc(Tstep))



# add in closest effective potential
dat0 <- dat0 %>%
  rowwise() %>%
  mutate(
    efective.potential = ep.global2$efective.potential[
      which.min(abs(ep.global2$X - stdlevel))
    ]
  ) %>%
  ungroup() 

dat0 <- dat0 %>%
  arrange(Tstep) %>%
  mutate(
    frame = row_number()
  )



global.ep.plot <- ggplot(
  ep.global2,
  aes(
    x = X,
    y = efective.potential,
    fill = side,
    group = interaction(year, side)
  )
) +
  geom_area(
    alpha = 0.9,
    color = "black",
    size = 1.0,
    position = "identity"
  ) +
  scale_fill_manual(
    values = c(
      left  = "#533113",
      right = "#5a6b3a"
    )
  ) +
  theme_classic() +
  labs(
    x = "Chlorophyll (standard level)",
    y = "Effective Potential"
  ) +
  theme(
    axis.text  = element_text(size = 12),
    axis.title = element_text(size = 12),
    strip.text = element_text(size = 12),
    legend.position = "none"
  ) +
  xlim(-7, 8)



ts.plot <- ggplot(dat0, aes(x = Tstep, y = stdlevel)) +
  geom_line(aes(group = year), color = "black", alpha = 0.6) +
  
  geom_point(
    aes(x = Tstep, y = stdlevel),
    color = "red",
    size = 3
  ) +
  
  theme_classic()


ep.plot <- global.ep.plot +
  geom_point(
    data = dat0,
    aes(x = stdlevel, y = efective.potential),
    color = "black",
    size = 12,
    inherit.aes = FALSE
  )


library(patchwork)
library(gganimate)

combined_plot <- ts.plot / ep.plot

combined_plot

anim <- combined_plot +
  transition_manual(frame) +
  ease_aes("linear")

animate(anim, fps = 20, duration = 20)  











##### BALL AND CUP ANIMATION ORIGINAL ######


library(patchwork)
library(gganimate)

# Load DLM result
#save(useBGA,Tstep,X.dlm,level,levelsd,stdlevel,file=Fname)  
# load(file='DLMresult_YSI_Peter19.Rdata')
load('./scripts/Langevin/HF Langevin/Langevin analysis/Finalized 2026-01-16/DLMresult_HYLB_Tuesday_ALL_Chl_Predicted to Manual Scale 098 NOISY.Rdata')

# thin the data to match DDJ
aropt=2
nx = length(stdlevel)
ikeep = seq(1, nx, by = aropt)

stdlevel = stdlevel[ikeep]
Tstep = Tstep[ikeep]

# put Tstep and stdlevel into a dataset
dat0 = as.data.frame(cbind(Tstep,stdlevel)) %>% 
  mutate(year = trunc(Tstep))




# add in closest effective potential
dat0 <- dat0 %>%
  rowwise() %>%
  mutate(
    efective.potential = ep.global2$efective.potential[
      which.min(abs(ep.global2$X - stdlevel))
    ]
  ) %>%
  ungroup() 

dat0_thin <- dat0 %>%
  arrange(Tstep) %>%
  slice(seq(1, n(), by = 300))

dat0_thin$Tstep = as.numeric(row.names(dat0_thin))


global.ep.plot <- ggplot(
  ep.global2,
  aes(
    x = X,
    y = efective.potential,
    fill = side,
    group = interaction(year, side)
  )
) +
  geom_area(
    alpha = 0.9,
    color = "black",
    size = 1.0,
    position = "identity"
  ) +
  scale_fill_manual(
    values = c(
      left  = "#533113",
      right = "#5a6b3a"
    )
  ) +
  theme_classic() +
  labs(
    x = "Chlorophyll (standard level)",
    y = "Effective Potential"
  ) +
  theme(
    axis.text  = element_text(size = 12),
    axis.title = element_text(size = 12),
    strip.text = element_text(size = 12),
    legend.position = "none"
  ) +
  xlim(-7, 8)+
  geom_point(
    data = dat0_thin,
    aes(
      x = stdlevel,
      y = efective.potential,
      group = 1,
      Tstep = Tstep   # make sure this exists!
    ),
    color = "black",
    size = 12,
    inherit.aes = FALSE
  )








### try to animate just the effective potential plot
anim = global.ep.plot +
  transition_time(Tstep) +
  labs(title = "Time: {frame_time}") +
  ease_aes('cubic-in-out') 


animate(anim, nframes = 100, fps = 10, duration = 5)  












combined_plot <- ts.plot / ep.plot 


combined_plot


anim <- ep.plot +
  transition_time(Tstep) +
  labs(title = "Time: {frame_time}") +
  ease_aes('linear') 







ts.plot <- ggplot(dat0, aes(x = Tstep, y = stdlevel)) +
  geom_line(color = "black", alpha = 0.6, aes(group = year)) +
  
  # moving point
  geom_point(
    aes(group = 1),
    color = "red",
    size = 3
  ) +
  
  theme_classic() +
  labs(
    x = "Time",
    y = "Chlorophyll (standard level)"
  )


animate(anim, nframes = 200, fps = 10, duration = 10)  


anim <- ggplot(dat0, aes(x = stdlevel)) +
  
  geom_area(
    data = ep.global2,
    aes(x = X, y = efective.potential, fill = side, group = side),
    alpha = 0.9,
    color = "black"
  ) +
  
  coord_cartesian(xlim = c(-7, 8)) +   # KEEP AXES SAFE HERE
  
  scale_fill_manual(values = c(left = "#533113", right = "#5a6b3a")) +
  
  theme_classic() +
  
  labs(
    x = "Chlorophyll (standard level)",
    y = "Effective Potential",
    title = "Time: {frame_time}"
  ) +
  xlim(-7, 8)+
  
  transition_time(Tstep) +
  ease_aes("linear")

anim

anim <- ep.plot +
  transition_reveal(Tstep) +
  labs(title = "Time: {frame_along}")


anim <- ts.plot +
  transition_reveal(Tstep) +
  labs(title = "Time: {frame_along}")








library(ggplot2)
library(gganimate)
library(dplyr)

# ---- BASE LANDSCAPE (STATIC) ----
ep_base <- ggplot() +
  
  # effective potential landscape
  geom_area(
    data = ep.global2,
    aes(x = X, y = efective.potential, fill = side, group = side),
    alpha = 0.9,
    color = "black",
    size = 1
  ) +
  
  scale_fill_manual(
    values = c(
      left  = "#533113",
      right = "#5a6b3a"
    )
  ) +
  
  # xlim(-7, 8) +
  
  theme_classic() +
  labs(
    x = "Chlorophyll (standard level)",
    y = "Effective Potential"
  ) +
  theme(
    axis.text  = element_text(size = 12),
    axis.title = element_text(size = 12),
    legend.position = "none"
  )

# ---- ANIMATED BALL (ADDED LAYER) ----
anim <- ep_base +
  geom_point(
    data = dat0,
    aes(
      x = stdlevel,
      y = efective.potential
    ),
    color = "red",
    size = 12
  ) +
  
  transition_time(Tstep) +
  
  labs(title = "Time: {frame_time}") +
  
  ease_aes("linear")+
  anim <- anim +
  shadow_wake(
    wake_length = 0.2,
    alpha = FALSE
  )

anim








library(ggplot2)
library(gganimate)
library(dplyr)

# --- Step 1: Make sure your point sits on the curve ---
# Assume dat0_thin has stdlevel for each Tstep
# Pull the corresponding y from ep.global2

dat0_thin <- ep.global2 %>%
  group_by(Tstep) %>%
  slice(which.min(abs(X - stdlevel))) %>%  # closest point on landscape
  ungroup()

# --- Step 2: Base landscape plot ---
global.ep.plot <- ggplot(
  ep.global2,
  aes(
    x = X,
    y = efective.potential,
    fill = side,
    group = interaction(year, side)
  )
) +
  geom_area(
    alpha = 0.9,
    color = "black",
    size = 1,
    position = "identity"
  ) +
  scale_fill_manual(
    values = c(left = "#533113", right = "#5a6b3a")
  ) +
  scale_x_continuous(limits = c(-7, 8), breaks = seq(-7, 8, by = 1)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  theme_classic() +
  theme(
    axis.line = element_line(color = "black", size = 1),
    axis.ticks = element_line(color = "black", size = 0.8),
    axis.text  = element_text(size = 12),
    axis.title = element_text(size = 12),
    strip.text = element_text(size = 12),
    legend.position = "none"
  ) +
  labs(
    x = "Chlorophyll (standard level)",
    y = "Effective Potential"
  )

# --- Step 3: Add the ball (point) with a highlight ---
global.ep.plot <- global.ep.plot +
  geom_point(
    data = dat0_thin,
    aes(x = stdlevel, y = efective.potential),
    color = "black",
    size = 12,
    inherit.aes = FALSE
  ) +
  geom_point(
    data = dat0_thin,
    aes(x = stdlevel, y = efective.potential),
    color = "white",
    size = 4,
    inherit.aes = FALSE
  )

# --- Step 4: Animate the ball ---
anim <- global.ep.plot +
  transition_time(Tstep) +          # time progression
  ease_aes('cubic-in-out') +       # smooth acceleration/deceleration
  labs(title = "Time: {frame_time}") 
# shadow_wake(wake_length = 0.1, alpha = FALSE)  # motion trail

# --- Step 5: Render animation ---
animate(anim, nframes = 100, fps = 20, duration = 20, width = 800, height = 600)





##### how much did DOC vary?

doc.data = read.csv("./data/formatted data/simulation model inputs 2013-2015 2024 2025 v4.csv") %>% 
  filter(Lake == "T" & !is.na(Ztherm))

ggplot(doc.data, aes(x = as.factor(Year), y = DOC))+
  geom_boxplot()

