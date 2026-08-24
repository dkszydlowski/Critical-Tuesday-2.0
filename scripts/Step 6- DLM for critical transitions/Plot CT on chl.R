#### plot CT on top of raw chlorophyll used to see what chl was doing at time of CT

library(tidyverse)


#### Plot critical transitions over daily chlorophyll time series ####
library(tidyverse)
library(ggpubr)

# ============================================================
# ASSUMPTIONS - please check/adjust:
# 1. Paul's CT DLM results were saved analogously to Tuesday's,
#    e.g. "./results/Paul CT DLM.csv" -- adjust if different
# 2. Paul's raw HF data mirrors Tuesday's naming/structure,
#    e.g. "./data/formatted data/HF data/Predicted Paul HYLB on Manual Scale log-trans NOISY ARIMA.csv"
# 3. Using delta = 0.90 as the "best" discount factor for plotting,
#    matching your "plotting checks" section -- change chosen_delta below if you want a different one
# 4. A "critical transition" = eigenvalue crossing from < 1 to >= 1
#    (matches your geom_hline(yintercept = 1) annotation in the original script)
# ============================================================

chosen_delta <- 0.90

# ---- helper: build daily morning-average chl for one lake, all years ----
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

# ---- helper: identify critical transition days from DLM results ----
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

# ================= Tuesday =================
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


# ================= Paul =================
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


# ================= combined =================
ggarrange(tues.ct.plot, paul.ct.plot, nrow = 1, ncol = 2)




#### Time between nutrient addition onset and first critical transition ####
library(dplyr)

# ---- nutrient addition onset DOY per year, from your table ----
# DOY of additions: 154-238 (2013), 153-241 (2014), 152-240 (2015), 162-233 (2024), 154-198 (2025)
nutrient_onset <- data.frame(
  Year = c(2013, 2014, 2015, 2024, 2025),
  onset_doy = c(154, 153, 152, 162, 154)
)

# ---- first critical transition per year (using tues_transitions from the previous script) ----
first_transition <- tues_transitions %>%
  group_by(Year) %>%
  slice_min(doy, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  rename(first_transition_doy = doy)

# ---- combine and calculate days between onset and first transition ----
onset_to_transition <- nutrient_onset %>%
  left_join(first_transition, by = "Year") %>%
  mutate(
    days_to_transition = first_transition_doy - onset_doy
  )

onset_to_transition
