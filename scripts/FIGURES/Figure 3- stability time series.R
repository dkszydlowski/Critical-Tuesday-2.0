##### plot the Eigenvalues from the DLM testing for critical transitions for both Tuesday and Paul

library(tidyverse)

# read in the data
ct.T = read.csv("./results/Tuesday CT DLM.csv") %>% 
  filter(delta == 0.90) %>% 
  mutate(Lake = "T")


ct.L = read.csv("./results/Paul CT DLM.csv") %>% 
  filter(delta == 0.90) %>% 
  mutate(Lake = "L")

# get critical transitions where eigenvalues cross 1
crit.trans.1 = rbind(ct.T, ct.L) %>% 
  group_by(Year, Lake) %>%
  arrange(doy, .by_group = TRUE) %>%
  filter(eigvals >= 1 & lag(eigvals, default = 0) < 1) %>%
  ungroup() %>% 
  mutate(Lake = factor(crit.trans.1$Lake, levels = c("T", "L")))

# combine time series
all.ct.ts = rbind(ct.T, ct.L)

all.ct.ts$Lake <- factor(all.ct.ts$Lake, levels = c("T", "L"))

# nutrient addition days
nutrient.periods = data.frame(
  Year = c(2013, 2014, 2015, 2024, 2025),
  Lake = "T",   # only applies to Tuesday, not Paul
  xmin = c(154, 153, 152, 162, 154),
  xmax = c(238, 241, 240, 233, 198)
)

nutrient.periods$Lake = factor(nutrient.periods$Lake, levels = c("T", "L"))


png("./figures/Figure 3 stability.png", res = 600, height = 100, width = 173, units = "mm") 


ggplot(all.ct.ts, aes(x = doy, y = eigvals, color = Lake))+
  geom_rect(
    data = nutrient.periods,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
    fill = "grey",
    alpha = 0.4,
    inherit.aes = FALSE
  ) +
  geom_vline(
    data = crit.trans.1,
    aes(xintercept = doy),
    color = "#CC79A7",
    linewidth = 1) +
   geom_line(size = 0.9)+
  geom_hline(yintercept = 1)+
  theme_bw()+
  scale_color_manual(values = c("L" = "#44729C", "T" = "#755A42"))+
  facet_grid2(
    Lake ~ Year,
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
          "L"    = "#44729C"
        ),
        colour = NA
      ),
      text_y = elem_list_text(
        color = "white",
        size = 10
      )
    ),
    
    labeller = labeller(
      Lake = c(
        "T" = "Tuesday (experimental)",
        "L"    = "Paul (reference)"
      )
    )
  )+
  scale_x_continuous(
    breaks = c(121, 152, 182, 213),
    labels = c("May", "Jun", "Jul", "Aug")
  )+
  labs(x = "Date", y = "Time-varying AR coefficient")+
  theme(legend.position = "none")
  



dev.off()
