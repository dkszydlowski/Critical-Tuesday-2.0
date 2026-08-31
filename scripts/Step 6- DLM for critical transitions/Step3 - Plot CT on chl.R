
#### Plot critical transitions over daily chlorophyll time series ####
# this script is just used to inspect results

library(tidyverse)
library(ggpubr)


chosen_delta <- 0.90

# daily average morning chlorophyll function 
build_daily_mean <- function(hf_path, years) {
  raw <- read.csv(hf_path) %>%
    mutate(datetime = ymd_hms(datetime))
  
  daily_list <- list()
  for (yr in years) {
    sonde <- raw %>%
      filter(Year == yr) %>%
      rename(doy_frac = DoY) %>%
      mutate(doy = trunc(doy_frac), chl = Chl_HYLB) %>%
      filter(format(datetime, "%H") %in% c("06", "07", "08", "09")) %>%
      select(doy_frac, doy, chl)
    
    daily_list[[as.character(yr)]] <- sonde %>%
      group_by(doy) %>%
      summarise(chl_mean = mean(chl, na.rm = TRUE), .groups = "drop") %>%
      mutate(Year = yr)
  }
  bind_rows(daily_list)
}

# identify critical transitions from AR coefficient
find_transitions <- function(ct_results, delta_val) {
  ct_results %>%
    filter(delta == delta_val) %>%
    arrange(Year, doy) %>%
    group_by(Year) %>%
    mutate(
      eigvals_prev = lag(eigvals),
      transition = !is.na(eigvals_prev) & eigvals_prev < 1 & eigvals >= 1
    ) %>%
    ungroup() %>%
    filter(transition) %>%
    select(Year, doy, eigvals)
}

years <- c(2013, 2014, 2015, 2024, 2025)

##### Tuesday #####
tues_ct <- read.csv("./results/Tuesday CT DLM.csv")
tues_daily <- build_daily_mean(
  "./data/formatted data/HF data/Predicted Tuesday HYLB on Manual Scale log-trans NOISY ARIMA.csv",
  years
)
tues_transitions <- find_transitions(tues_ct, chosen_delta)

tues.ct.plot <- ggplot(tues_daily, aes(x = doy, y = chl_mean)) +
  geom_vline(
    data = tues_transitions,
    aes(xintercept = doy),
    color = "firebrick", linewidth = 0.8, alpha = 0.7
  ) +
  geom_line(color = '#117733', linewidth = 0.8) +
  geom_point(color = '#117733', size = 1.5) +
  geom_vline(xintercept = 162, linetype = "dashed", color = "grey30") +  # nutrient addition date
  facet_wrap(~ Year, ncol = 1, scales = "free_x") +
  theme_bw() +
  labs(x = "Day of year", y = "Chlorophyll (morning avg)", 
       title = "Tuesday Lake - critical transitions")

tues.ct.plot


##### Paul ######
# adjust paths below if Paul's files differ in naming
paul_ct <- read.csv("./results/Paul CT DLM.csv")
paul_daily <- build_daily_mean(
  "./data/formatted data/HF data/Predicted Paul HYLB on Manual Scale log-trans NOISY ARIMA.csv",
  years
)
paul_transitions <- find_transitions(paul_ct, chosen_delta)

paul.ct.plot <- ggplot(paul_daily, aes(x = doy, y = chl_mean)) +
  geom_vline(
    data = paul_transitions,
    aes(xintercept = doy),
    color = "firebrick", linewidth = 0.8, alpha = 0.7
  ) +
  geom_line(color = '#1F78B4', linewidth = 0.8) +
  geom_point(color = '#1F78B4', size = 1.5) +
  geom_vline(xintercept = 162, linetype = "dashed", color = "grey30") +
  facet_wrap(~ Year, ncol = 1, scales = "free_x") +
  theme_bw() +
  labs(x = "Day of year", y = "Chlorophyll (morning avg)", 
       title = "Paul Lake - critical transitions")

paul.ct.plot


# combined 
ggarrange(tues.ct.plot, paul.ct.plot, nrow = 1, ncol = 2)

