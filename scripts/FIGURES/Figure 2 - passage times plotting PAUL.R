#### plot passage times for Paul Lake #####
# run after Figure 2- passage times plotting Tuesday

# script for plotting passage time results for the global DDJ estimates

library(tidyverse)
library(ggridges)
library(ggpubr)
library(ggpmisc)
library(ggrepel)



# read in the data
t25 = read.csv("./results/passage times/Paul ARIMA-corrected 2025 global 2026-06-18 THINNED.csv")
t24 = read.csv("./results/passage times/Paul ARIMA-corrected 2024 global 2026-06-18 THINNED.csv")
t15 = read.csv("./results/passage times/Paul ARIMA-corrected 2015 global 2026-06-18 THINNED.csv")
t14 = read.csv("./results/passage times/Paul ARIMA-corrected 2014 global 2026-06-18 THINNED.csv")
t13 = read.csv("./results/passage times/Paul ARIMA-corrected 2013 global 2026-06-18 THINNED.csv")

# read in Tukey's letters
L.letters = read.csv("./results/passage times/tukey_letters 2026-07-30.csv") %>% 
  filter(lake == "Paul") %>% 
  mutate(year = as.factor(year))

pL.all = rbind(t25, t24, t15, t14, t13) %>% 
  mutate(hours = minutes/60) 

pt.days = pL.all %>% 
  mutate(days = hours/24)

green_palette <- c("#CBD4AC", "#b4c187", "#80914b", "#5a6b3a", "#496231")
#blue_palette <- c("#CFB491", "#9c7744", "#8c5c2b", "#533113", "#361c07")

# blue_palette <- c(
#   "#ACBDD4",
#   "#87A5C1",
#   "#4B6F91",
#   "#3A546B",
#   "#314962"
# )



blue_palette <- c(
  "#B4C5CF",
  "#44729C",
  "#2B5A8C",
  "#133353",
  "#071C36"
)


L.letters.right = L.letters %>% filter(basin == "right")

rb.plot = ggplot(pL.all %>% filter(basin == "right" & minutes > 30), aes(x = as.factor(year), y = (hours)/24, fill = factor(year))) +
  geom_boxplot(alpha = 0.8)+
  geom_text(data = L.letters.right, aes(x = year, y = y_pos, label = Letters),
            inherit.aes = FALSE, size = 5) +
  labs(y = "", x = "Year", title = "high-pigment") +
  theme(legend.position = "none", axis.text = element_text(size = 14), axis.title = element_text(size = 16))+
  scale_fill_manual(values = green_palette)+
  theme_bw()+
  scale_y_log10()+
  theme(legend.position = "none")+
  theme(
    axis.text.y  = element_text(size = 12),
    axis.text.x = element_text(size = 10),
    axis.title = element_text(size = 12),
    strip.text = element_text(size = 12),
    legend.position = "none"
  ) +
  theme(plot.margin = margin(l = 0.2))



# no log-transformation
# ggplot(pL.all %>% filter(basin == "right" & minutes > 30), aes(x = as.factor(year), y = (hours)/24, fill = factor(year))) +
#   geom_boxplot(alpha = 0.8)+
#   labs(y = "", x = "Year", title = "high-pigment") +
#   theme(legend.position = "none", axis.text = element_text(size = 14), axis.title = element_text(size = 16))+
#   scale_fill_manual(values = green_palette)+
#   theme_classic()+
#   #scale_y_log10()+
#   theme(legend.position = "none")+
#   theme(
#     axis.text.y  = element_text(size = 12),
#     axis.text.x = element_text(size = 10),
#     axis.title = element_text(size = 12),
#     strip.text = element_text(size = 12),
#     legend.position = "none"
#   ) 

# aov_left <- aov(hours ~ factor(year), data = subset(pL.all %>% filter(minutes > 30), basin == "right"))
# summary(aov_left)
# 
# TukeyHSD(aov_left)
# 
# kruskal.test(hours ~ factor(year), data = subset(pL.all, basin == "left"))
# pairwise.t.test(
#   x = log10(subset(pL.all, basin == "left")$hours),
#   g = subset(pL.all, basin == "left")$year,
#   p.adjust.method = "BH"   # or "bonferroni"
# )


L.letters.left = L.letters %>% filter(basin == "left")


lb.plot = ggplot(pL.all %>% filter(basin == "left" & minutes > 30), aes(x = as.factor(year), y = (hours)/24, fill = factor(year))) +
  geom_boxplot(alpha = 0.8)+
  labs(y = "passage time (days)", x = "Year", title = "low-pigment") +
  geom_text(data = L.letters.left, aes(x = year, y = y_pos, label = Letters),
            inherit.aes = FALSE, size = 5) +
  theme(legend.position = "none", axis.text = element_text(size = 14), axis.title = element_text(size = 16))+
  scale_fill_manual(values = rev(blue_palette))+
  theme_bw()+
  scale_y_log10()+
  theme(legend.position = "none")+
  theme(
    axis.text.y  = element_text(size = 12),
    axis.text.x = element_text(size = 10),
    axis.title = element_text(size = 12),
    strip.text = element_text(size = 12),
    legend.position = "none"
  ) 



ptimes = ggarrange(lb.plot, rb.plot, align = "h")


#### version of the plot with no filtering ####
# rb.plot = ggplot(pL.all %>% filter(basin == "right"), aes(x = as.factor(year), y = (hours)/24, fill = factor(year))) +
#   geom_boxplot(alpha = 0.8)+
#   #geom_point()+
#   labs(y = "", x = "") +
#   theme(legend.position = "none", axis.text = element_text(size = 14), axis.title = element_text(size = 16))+
#   scale_fill_manual(values = green_palette)+
#   theme_bw()+
#   scale_y_log10(breaks = c(0.1, 1, 10, 100), limits = c(0.005, 100))+
#   theme(legend.position = "none")+
#   theme(
#     axis.text.y  = element_text(size = 12),
#     axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
#     axis.title = element_text(size = 12),
#     strip.text = element_text(size = 12),
#     legend.position = "none"
#   ) +
#   theme(plot.margin = margin(r = 0, l = 0, t = 0.5, b = 0))


# 
# lb.plot = ggplot(pL.all %>% filter(basin == "left"), aes(x = as.factor(year), y = (hours)/24, fill = factor(year))) +
#   geom_boxplot(alpha = 0.8)+
#   #geom_point()+
#   labs(y = "", x = "") +
#   theme(legend.position = "none", axis.text = element_text(size = 14), axis.title = element_text(size = 16))+
#   scale_fill_manual(values = rev(blue_palette))+
#   theme_bw()+
#   scale_y_log10(breaks = c(0.1, 1, 10, 100), limits = c(0.005, 100))+
#   theme(legend.position = "none")+
#   theme(
#     axis.text.y  = element_text(size = 12),
#     axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
#     axis.title = element_text(size = 12),
#     strip.text = element_text(size = 12),
#     legend.position = "none"
#   ) +
#   theme(plot.margin = margin(r = 0, l = 0, t = 0.5, b = 0))


#ptimes = ggarrange(lb.plot, rb.plot, align = "h")


annot.df <- data.frame(
  basin = "left",
  year = 2025,
  y = 0.006,
  label = ""
)


# create combined fill variable
pL.all2 <- pL.all %>%
  mutate(
    basin_year = paste(basin, year, sep = "_")
  ) %>% 
  filter(minutes > 30)

# named color vector
fill_vals <- c(
  setNames(rep("#44729C", 5), paste0("left_", sort(unique(pL.all$year)))),
  setNames(rep("#b4c187", 5), paste0("right_", sort(unique(pL.all$year))))
)

ptimes <- ggplot(
  pL.all2,
  aes(
    x = as.factor(year),
    y = hours / 24,
    fill = basin_year
  )
) +
  geom_boxplot(alpha = 0.8) +
  facet_wrap(~ basin, nrow = 1) +
  scale_fill_manual(values = fill_vals) +
  scale_y_log10(
    limits = c(0.005, 100),
    breaks = c(0.01, 0.1, 1, 10, 100),
    labels = c("0.01", "0.1", "1", "10", "100"))+
  labs(x = "", y = "") +
  theme_bw() +
  geom_text(
    data = annot.df,
    aes(
      x = as.factor(year),
      y = y,
      label = label
    ),
    inherit.aes = FALSE,
    angle = 90,
    size = 3,
    hjust = 0,
    color = "black",
    fontface = "bold.italic"
  ) +
  geom_text(
    data = L.letters,
    aes(
      x = year,
      y = y_pos,
      label = Letters
    ),
    inherit.aes = FALSE,
    size = 3) +
  theme(
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
    axis.title = element_text(size = 12),
    legend.position = "none",
    panel.grid = element_blank(),
    plot.margin = margin(r = 0, l = 0, t = 1, b = 0))+
  theme(strip.text = element_blank(),
        strip.background = element_blank())








### density ridgeline plots of passage time ###

pt.left.density =  ggplot(
  pL.all %>% filter(basin == "left" & minutes),
  aes(x = (hours)/24, y = factor(year), fill = factor(year))
) +
  geom_density_ridges(scale = 1.2, alpha = 0.8, color = "white") +
  scale_fill_manual(values = rev(blue_palette)) +
  scale_x_log10(breaks = c(0.1, 1, 10, 100), limits = c(0.005, 100)) +
  labs(
    x = "passage time (days)",
    y = "Year",
    title = "low-pigment"
  ) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    legend.position = "none"
  )+ 
  geom_vline(xintercept = 0.5/24, linetype = "dashed", size = 0.7)


pt.right.density = ggplot(
  pL.all %>% filter(basin == "right"),
  aes(x = (hours)/24, y = factor(year), fill = factor(year))
) +
  geom_density_ridges(scale = 1.2, alpha = 0.8, color = "white") +
  scale_fill_manual(values = (green_palette)) +
  scale_x_log10(breaks = c(0.1, 1, 10, 100), limits = c(0.005, 100)) +
  labs(
    x = "passage time (days)",
    y = "Year",
    title = "high-pigment"
  ) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    legend.position = "none"
  )+
  geom_vline(xintercept = 0.5/24, linetype = "dashed", size = 0.7)




ggarrange(lb.plot, rb.plot, pt.left.density, pt.right.density, nrow = 2, ncol = 2)



## compare median and mean passage times to median and mean kNC

data = read.csv("./data/formatted data/simulation model inputs 2013-2015 2024 2025 v4.csv")

data %>% 
  group_by(Year) %>% 
  summarize(mean(grav.m2))

data.mean = data %>% 
  mutate(kNC = kPAR - 0.0177*Manual_Chl) %>% 
  filter(Lake == "L" & kNC > 0) %>% 
  group_by(Year) %>% 
  summarize(mean.kNC = mean(kNC, na.rm = TRUE),
            median.kNC = median(kNC, na.rm = TRUE),
            total.nuts = max(cumulative.load, na.rm = TRUE),
            mean.kPAR = mean(kPAR, na.rm = TRUE)) 


# get median and mean passage times for each basin
pt.mean = pL.all %>% 
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

if(nrow(pt.mean) < 10){
  pt.mean <- bind_rows(
    pt.mean,
    tibble(
      Year = 2025,
      basin = "left",
      mean.minutes = 0.5,
      median.minutes = 0.5,
      mean.hours = 0.5,
      mean.days = 0.5 / 24,
      mean.kNC = pt.mean$mean.kNC[pt.mean$Year == 2025],
      median.kNC = pt.mean$median.kNC[pt.mean$Year == 2025],
      total.nuts = pt.mean$total.nuts[pt.mean$Year == 2025],
      mean.kPAR = pt.mean$mean.kPAR[pt.mean$Year == 2025],
      max.P = pt.mean$max.P[pt.mean$Year == 2025]
    )
  )
}

## add in zooplankton biomass ##
# pt.mean = pt.mean %>% 
#   mutate(zoop.mean = NA) %>% 
#   mutate(zoop.mean = replace(zoop.mean, Year == 2013, 0.19)) %>% 
#   mutate(zoop.mean = replace(zoop.mean, Year == 2014, 0.42)) %>% 
#   mutate(zoop.mean = replace(zoop.mean, Year == 2015, 0.38)) %>% 
#   mutate(zoop.mean = replace(zoop.mean, Year == 2024, 0.47)) %>% 
#   mutate(zoop.mean = replace(zoop.mean, Year == 2025, 0.14))




# compare.to.zoop = ggplot(pt.mean, aes(x = zoop.mean, y = (mean.days), color = basin)) +
#   geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +
#   geom_point(size = 3.8) +
#   geom_text_repel(aes(label = Year), color = "black", size = 3,
#                   show.legend = FALSE, max.overlaps = Inf,
#                   box.padding = 0.6, point.padding = 0.5,
#                   force = 2, min.segment.length = 0, segment.color = NA) +
#   facet_wrap(~basin,
#              labeller = as_labeller(c(left = "low-pigment",
#                                       right = "high-pigment"))) +
#   scale_color_manual(values = c(left =    "#B4C5CF",
#                                 right = "#5a6b3a")) +
#   labs(x = "mean zooplankton biomass (g m-2)",
#        y = "mean passage time (days)") +
#   stat_poly_eq(aes(label = paste(..rr.label..)),
#                formula = y ~ x, parse = TRUE,
#                label.x = "left", label.y = "top", color = "black") +
#   theme_classic() +
#   scale_y_log10(limits = c(0.01, 100))+
#   theme(legend.position = "none",
#         strip.text = element_text(size = 12))+
#   theme(
#     axis.text  = element_text(size = 12),
#     axis.title = element_text(size = 12),
#     strip.text = element_text(size = 12),
#     legend.position = "none"
#   ) 
# 


#png("./figures/ASLO 2026/zoop and pt.png", res = 300, units = "in", height = 4, width = 8)

compare.to.zoop

#dev.off()
pos.df <- pt.mean %>%
  group_by(basin) %>%
  summarise(
    x = max(mean.kNC) * 0.93,
    y = -1.8
  )




compare.to.knc = ggplot(pt.mean, aes(x = mean.kNC, y = (mean.days), fill = basin, color = basin)) +
  scale_y_log10(
    limits = c(0.01, 100),
    breaks = c(0.01, 0.1, 1, 10, 100),
    labels = c("0.01", "0.1", "1", "10", "100"))+
  stat_poly_eq(
    data = subset(pt.mean, basin == "left"),
    aes(label = paste(..rr.label..)),
    formula = (y) ~ x,
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
    formula = (y) ~ x,
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
  # nudge_y = ifelse(pt.mean$Year == 2025 & pt.mean$basin == "right", -0.1, 0)) +
  facet_wrap(~basin,
             labeller = as_labeller(c(left = "low-pigment",
                                      right = "high-pigment"))) +
  scale_fill_manual(values = c(left =   "#44729C",
                               right = "#b4c187")) +
  
  scale_color_manual(values = c(left =   "#44729C",
                                right = "#b4c187")) +
  labs(x = "non-chl light attenuation (kNC)",
       y = "") +
  theme_bw() +
  theme(legend.position = "none",
        strip.text = element_text(size = 12))+
  theme(
    axis.text  = element_text(size = 10),
    axis.title = element_text(size = 12),
    strip.text = element_blank(),
    legend.position = "none",
    panel.grid = element_blank()
  ) +
  theme(plot.margin = margin(l = 0.2))





ggarrange(ptimes, compare.to.knc, nrow = 2, ncol = 1)


### compare to kNC for DEFENSE

#png("./figures/DEFENSE/pt to knc.png", height = 3.04, width = 5.93, units = "in", res = 300)

ggplot(pt.mean, aes(x = mean.kNC, y = (mean.days), color = basin)) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +
  geom_point(size = 4) +
  geom_text_repel(aes(label = Year), color = "black", size = 3,
                  show.legend = FALSE, max.overlaps = Inf,
                  box.padding = 0.05, point.padding = 0.1,
                  force = 0.2, min.segment.length = 0, segment.color = NA) +
  facet_wrap(~basin,
             labeller = as_labeller(c(left = "low-pigment",
                                      right = "high-pigment"))) +
  scale_color_manual(values = c(left =   "#44729C",
                                right = "#b4c187")) +
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
             labeller = as_labeller(c(left = "low-pigment",
                                      right = "high-pigment"))) +
  scale_color_manual(values = c(left =   "#44729C",
                                right = "#b4c187")) +
  labs(x = "cumulative P added (mg m-2 d-1)",
       y = "") +
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
#pandpt
#dev.off()


### add in global effective potential
ep.global = read.csv("./results/DDJ results Paul ARIMA-correced.csv") %>% 
  mutate(year = "all years")

eq <- -0.06 # from DDJ step


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
      left  = "#44729C",
      right = "#b4c187"
    )
  ) +
  theme_bw() +
  labs(
    x = "chlorophyll (standard level)",
    y = "",
    title = "Paul Lake (reference)"
  ) +
  theme(
    axis.text  = element_text(size = 10),
    axis.title = element_text(size = 12),
    strip.text = element_text(size = 12),
    legend.position = "none",
    panel.grid = element_blank(),
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
  ) +
  xlim(-7, 8) +
  theme(plot.margin = margin(l = 0.2)) +
  # top state rectangles
  annotate(
    "rect",
    xmin = -7, xmax = eq,
    ymin = ymax * 0.95, ymax = ymax * 1.16,
    fill = "#44729C",
    color = "black"
  ) +
  annotate(
    "rect",
    xmin = eq, xmax = 8,
    ymin = ymax * 0.95, ymax = ymax * 1.16,
    fill = "#b4c187",
    color = "black"
  ) +
  annotate(
    "text",
    x = (-7 + eq)/2,
    y = ymax * 1.07,
    label = "low-pigment",
    color = "white",
    fontface = "bold",
    size = 4
  ) +
  annotate(
    "text",
    x = (eq + 8) / 2,
    y = ymax * 1.07,
    label = "high-pigment",
    color = "white",
    fontface = "bold",
    size = 4
  )

# png("./figures/draft 2026-02-27/passage times.png", res = 300, height = 8, width = 6, units = "in") 

paul.all = ggarrange(global.ep.plot, ptimes, compare.to.knc, nrow = 3, ncol = 1, align = "hv")



png("./figures/Figure 2.png", res = 600, height = 173, width = 173, units = "mm") 

ggarrange(tues.all, paul.all, align = "hv")

dev.off()



### nutrients and kPAR
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
  mutate(doy =  mindoy + (dat0$Tstep - dat0$year) * (maxdoy - mindoy + 1))

# Load DDJ data and apply to dataframe
load("./results/DDJ results Paul ARIMA-corrected data.Rdata")

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




ggplot(dat0 %>% filter(Tstep >= 2013.11800 ), aes(x = doy, y = stdlevel, group = group, color = basin)) +
  geom_line(size = 1, alpha = 0.75) +
  geom_line(aes(x = doy, y = equilibria), size = 2, color = "black") +
  geom_hline(yintercept = thresh, linetype = "dashed") +
  scale_color_manual(values = c("high-pigment" = "#5a6b3a", "low-pigment" =   "#44729C")) +
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
  scale_color_manual(values = c("high-pigment" = "#5a6b3a", "low-pigment" =   "#44729C")) +
  facet_wrap(~year) +
  theme_classic() +
  labs(x = "DOY", y = "Chlorophyll (standard level)") +
  theme(legend.position = "none")


png("./figures/DEFENSE/time series standardized")




cL.all = read.csv("./scripts/Multivariate DLM/eigenvalues 2026-01-27.csv") %>% 
  filter(delta == 0.94) %>% 
  rename(year = Year)

ggplot(cL.all, aes(x = doy, y = eigvals, color = as.factor(year)))+
  geom_point()+
  geom_line()+
  facet_wrap(~year, ncol = 1, nrow = 5)+
  theme_bw()+
  labs(title = "log-trans, Delta = 0.94")+
  theme(axis.text = element_text(size = 12))+
  geom_hline(yintercept = 1)




cL.all = read.csv("./scripts/Multivariate DLM/eigenvalues 2026-01-27.csv") %>% 
  filter(delta == 0.90) %>% 
  rename(year = Year)

ggplot(cL.all, aes(x = doy, y = eigvals, color = as.factor(year)))+
  geom_point()+
  geom_line()+
  facet_wrap(~year, ncol = 1, nrow = 5)+
  theme_bw()+
  labs(title = "log-trans, Delta = 0.90")+
  theme(axis.text = element_text(size = 12))+
  geom_hline(yintercept = 1)



cL.all = read.csv("./scripts/Multivariate DLM/eigenvalues 2026-01-27 NO BGA.csv") %>% 
  filter(delta == 0.90) %>% 
  rename(year = Year)

ggplot(cL.all, aes(x = doy, y = eigvals, color = as.factor(year)))+
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

ggplot(cL.all, aes(x = doy, y = eigvals, color = as.factor(year)))+
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
  scale_color_manual(values = c("high-pigment" = "#5a6b3a", "low-pigment" =   "#44729C")) +
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
  # scale_color_manual(values = c("high-pigment" = "#5a6b3a", "low-pigment" = "#533113")) +
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
  #  scale_color_manual(values = c("high-pigment" = "#5a6b3a", "low-pigment" = "#533113")) +
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
  #scale_color_manual(values = c("high-pigment" = "#5a6b3a", "low-pigment" = "#533113")) +
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



png("./figures/DEFENSE/chl time series no ct threshold coloreds.png", width = 7.5, height = 5.5, res = 300, units = "in")


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
  scale_color_manual(values = c("high-pigment" = "#5a6b3a", "low-pigment" =   "#44729C")) +
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






png("./figures/DEFENSE/chl time series threshold coloreds.png", width = 7.5, height = 5.5, res = 300, units = "in")


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
  scale_color_manual(values = c("high-pigment" = "#5a6b3a", "low-pigment" = "#533113")) +
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





png("./figures/DEFENSE/chl time series threshold coloreds ct.png", width = 7.5, height = 5.5, res = 300, units = "in")


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
  scale_color_manual(values = c("high-pigment" = "#5a6b3a", "low-pigment" = "#533113")) +
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
  scale_color_manual(values = c("high-pigment" = "#5a6b3a", "low-pigment" = "#533113")) +
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
  scale_color_manual(values = c("high-pigment" = "#5a6b3a", "low-pigment" = "#533113")) +
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







##### how much did DOC vary?

doc.data = read.csv("./data/formatted data/simulation model inputs 2013-2015 2024 2025 v4.csv") %>% 
  filter(Lake == "T" & !is.na(Ztherm))

ggplot(doc.data, aes(x = as.factor(Year), y = DOC))+
  geom_boxplot()

