###### test passage times to see which are statistically different ######
# this code is supplemental and not necessary to the main analysis

library(tidyverse)
library(dplyr)
library(multcompView)
library(ggplot2)
library(FSA)
library(rcompanion) 

# plotting
library(ggtext)
library(viridis)



#=============================================================================================================================#
#### ANOVA AND TUKEY'S LETTERS ####


### read in Paul passage times ###
l25 = read.csv("./results/passage times/Paul ARIMA-corrected 2025 global 2026-06-18 THINNED.csv")
l24 = read.csv("./results/passage times/Paul ARIMA-corrected 2024 global 2026-06-18 THINNED.csv")
l15 = read.csv("./results/passage times/Paul ARIMA-corrected 2015 global 2026-06-18 THINNED.csv")
l14 = read.csv("./results/passage times/Paul ARIMA-corrected 2014 global 2026-06-18 THINNED.csv")
l13 = read.csv("./results/passage times/Paul ARIMA-corrected 2013 global 2026-06-18 THINNED.csv")

pL.all = rbind(l25, l24, l15, l14, l13) %>%
  mutate(hours = minutes/60, lake = "Paul",
         year  = c(rep(2025, nrow(l25)), rep(2024, nrow(l24)),
                   rep(2015, nrow(l15)), rep(2014, nrow(l14)),
                   rep(2013, nrow(l13))))

### read in Tuesday passage times ###
t25 = read.csv("./results/passage times/Tuesday ARIMA-corrected 2025 global 2026-06-18 THINNED.csv")
t24 = read.csv("./results/passage times/Tuesday ARIMA-corrected 2024 global 2026-06-18 THINNED.csv")
t15 = read.csv("./results/passage times/Tuesday ARIMA-corrected 2015 global 2026-06-18 THINNED.csv")
t14 = read.csv("./results/passage times/Tuesday ARIMA-corrected 2014 global 2026-06-18 THINNED.csv")
t13 = read.csv("./results/passage times/Tuesday ARIMA-corrected 2013 global 2026-06-18 THINNED.csv")

tL.all = rbind(t25, t24, t15, t14, t13) %>%
  mutate(hours = minutes/60, lake = "Tuesday",
         year  = c(rep(2025, nrow(t25)), rep(2024, nrow(t24)),
                   rep(2015, nrow(t15)), rep(2014, nrow(t14)),
                   rep(2013, nrow(t13))))

# combine into one
both.all = bind_rows(pL.all, tL.all) %>%
  mutate(
    year = as.factor(year),
    lake_year = interaction(lake, year, sep = "_"),
    days = hours/24)

n.both = both.all %>% 
  filter(minutes > 30) %>% 
  group_by(lake, year, basin) %>% 
  summarize(n.obs = n())

mean.both = both.all %>% 
  filter(minutes > 30) %>% 
  group_by(lake, year, basin) %>% 
  summarize(mean.pt.days = mean(days, na.rm = TRUE),
            mean.pt.hours = mean(hours, na.rm = TRUE),
            sd.pt.days = sd(days, na.rm = TRUE))


mod <- aov(days ~ lake * year * basin, data = both.all)

summary(mod)


library(broom)
ttests <- both.all %>%
  filter(minutes > 30) %>% 
  group_by(year, basin) %>%
  group_modify(~ tidy(t.test(days ~ lake, data = .x))) %>%
  ungroup()

ttests

library(emmeans)
emmeans(mod, pairwise ~ lake | year * basin)

mean.wide <- mean.both %>%
  ungroup() %>%
  select(lake, year, basin, mean.pt.days, sd.pt.days) %>%
  pivot_wider(
    names_from = lake,
    values_from = c(mean.pt.days, sd.pt.days)
  ) %>%
  mutate(
    diff.days = mean.pt.days_Paul - mean.pt.days_Tuesday
  )

mean.wide

# make interaction of lake, year, and basin
pt.groups <- both.all %>%
  filter(minutes > 30) %>%
  mutate(
    lake = as.factor(lake),
    year = as.factor(year),
    basin = as.factor(basin),
    lake_year_basin = interaction(lake, year, basin, sep = "_", drop = TRUE),
    logdays = log10(hours / 24)
  )

# run ANOVA and Tukey HSD
aov_mod <- aov(logdays ~ lake_year_basin, data = pt.groups)
tukey <- TukeyHSD(aov_mod)

# check residuals to make sure they are normal
shapiro.test(residuals(aov_mod))
qqnorm(residuals(aov_mod)); qqline(residuals(aov_mod)) # confirms we need to log-transform

cld <- multcompLetters4(aov_mod, tukey)
cld_df <- as.data.frame.list(cld$lake_year_basin)
cld_df$lake_year_basin <- rownames(cld_df)

test = dunnTest(logdays ~ lake_year, data = pt.groups, method = "bh")

# split lake_year_basin
letters_df <- pt.groups %>%
  group_by(lake, year, basin, lake_year_basin) %>%
  summarise(y_pos = max(days, na.rm = TRUE) * 1.6, .groups = "drop") %>%
  left_join(cld_df %>% select(lake_year_basin, Letters), by = "lake_year_basin") %>%
  select(lake, year, basin, Letters, y_pos)


write.csv(letters_df, "./results/passage times/tukey_letters 2026-07-30.csv", row.names = FALSE)


#### ADD LETTERS TO PLOT #####

library(ggnewscale)


### PAUL ###
L.letters = read.csv("./results/passage times/tukey_letters 2026-07-30.csv") %>% 
  filter(lake == "Paul") %>% 
  mutate(year = as.factor(year))


# ---- 1. figure out which letters actually appear across your groups ----
all_letters <- sort(unique(unlist(strsplit(L.letters$Letters, ""))))
# should give something like "a" "b" "c" "d" - confirm it's length 4
all_letters

# ---- 2. build a long presence/absence dataframe: one row per group x letter ----
letter_grid <- L.letters %>%
  tidyr::expand_grid(letter = all_letters) %>%
  rowwise() %>%
  mutate(present = grepl(letter, Letters, fixed = TRUE)) %>%
  ungroup() %>%
  left_join(data.frame(letter = all_letters, row_num = seq_along(all_letters)),
            by = "letter")

# ---- 3. define log-space boundaries for the strip, below your current data floor ----
# your current lower limit is 0.005 -- reserve space below that, e.g. down to 0.0005
strip_bottom <- log10(0.0005)
strip_top    <- log10(0.004)     # just under 0.005, so it doesn't touch the boxplots
n_rows       <- length(all_letters)
row_height   <- (strip_top - strip_bottom) / n_rows

letter_grid <- letter_grid %>%
  mutate(
    ymin = 10^(strip_bottom + (row_num - 1) * row_height),
    ymax = 10^(strip_bottom + row_num * row_height),
    fill_letter = ifelse(present, letter, NA)   # NA = unfilled/empty row
  )

# ---- 4. colors for the 4 letters (pick whatever you like) ----
letter_colors <- c(a = "#8c5c2b", b = "#5a6b3a", c = "#3A546B", d = "#361c07")


pL.all2 <- pL.all %>%
  mutate(
    basin_year = paste(basin, year, sep = "_")
  ) %>% 
  filter(minutes > 30)


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
    limits = c(0.0005, 175),   # extended down to fit the letter strip
    breaks = c(0.01, 0.1, 1, 10, 100),
    labels = c("0.01", "0.1", "1", "10", "100")) +
  labs(x = "", y = "") +
  theme_bw() +
  geom_text(
    data = annot.df,
    aes(x = as.factor(year), y = y, label = label),
    inherit.aes = FALSE, angle = 90, size = 3, hjust = 0,
    color = "black", fontface = "bold.italic"
  ) +
  geom_text(
    data = L.letters,
    aes(x = year, y = y_pos, label = Letters),
    inherit.aes = FALSE, size = 3
  ) +
  # ---- new fill scale so it doesn't clash with basin_year fill ----
new_scale_fill() +
  geom_rect(
    data = letter_grid,
    aes(
      xmin = as.numeric(as.factor(year)) - 0.4,
      xmax = as.numeric(as.factor(year)) + 0.4,
      ymin = ymin,
      ymax = ymax,
      fill = fill_letter
    ),
    inherit.aes = FALSE,
    color = "grey50",
    linewidth = 0.2
  ) +
  scale_fill_manual(values = letter_colors, na.value = "white", guide = "none") +
  theme(
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
    axis.title = element_text(size = 12),
    legend.position = "none",
    panel.grid = element_blank(),
    plot.margin = margin(r = 0, l = 0, t = 1, b = 0)) +
  theme(strip.text = element_blank(),
        strip.background = element_blank())

ptimes






# Kruskal-Wallis omnibus test  - non-parametric version
# but this tests for differences in medians, not means

kruskal.test(logdays ~ lake_year_basin, data = pt.groups)

# Dunn's post-hoc pairwise test ----
dunn_res <- dunnTest(logdays ~ lake_year_basin, data = pt.groups, method = "bh")  

dunn_res$res  

pvals <- dunn_res$res$P.adj
names(pvals) <- gsub(" - ", "-", dunn_res$res$Comparison)

# sanity check
head(names(pvals))

letters_kw <- multcompLetters(pvals, threshold = 0.05)

letters_kw_df <- data.frame(
  lake_year = names(letters_kw$Letters),
  Letters = letters_kw$Letters,
  row.names = NULL
)

letters_kw_df






















library(ggnewscale)
library(tidyr)
library(dplyr)
library(ggpubr)

# ---- helper: build a letter-presence grid for one lake/basin subset ----
build_letter_grid <- function(letters_df, strip_bottom = log10(0.0005), strip_top = log10(0.004)) {
  all_letters <- sort(unique(unlist(strsplit(letters_df$Letters, ""))))
  n_rows <- length(all_letters)
  row_height <- (strip_top - strip_bottom) / n_rows
  
  letters_df %>%
    tidyr::expand_grid(letter = all_letters) %>%
    rowwise() %>%
    mutate(present = grepl(letter, Letters, fixed = TRUE)) %>%
    ungroup() %>%
    left_join(data.frame(letter = all_letters, row_num = seq_along(all_letters)), by = "letter") %>%
    mutate(
      ymin = 10^(strip_bottom + (row_num - 1) * row_height),
      ymax = 10^(strip_bottom + row_num * row_height),
      fill_letter = ifelse(present, letter, NA)
    )
}


  
letter_colors <- c(a = "#5DC863FF", b = "#21908CFF", c = "#440154FF")

# color each letter to match
color_letters <- function(letters_str, colors = letter_colors) {
  chars <- strsplit(letters_str, "")[[1]]
  paste0(
    sapply(chars, function(ch) {
      col <- colors[[ch]]
      paste0("<span style='color:", col, "'>", ch, "</span>")
    }),
    collapse = ""
  )
}



# ---- read Tukey letters once, for both lakes ----
tukey_all <- read.csv("./results/passage times/tukey_letters 2026-07-30.csv") %>%
  mutate(year = as.factor(year))


### Tuesday 

t25 = read.csv("./results/passage times/Tuesday ARIMA-corrected 2025 global 2026-06-18 THINNED.csv")
t24 = read.csv("./results/passage times/Tuesday ARIMA-corrected 2024 global 2026-06-18 THINNED.csv")
t15 = read.csv("./results/passage times/Tuesday ARIMA-corrected 2015 global 2026-06-18 THINNED.csv")
t14 = read.csv("./results/passage times/Tuesday ARIMA-corrected 2014 global 2026-06-18 THINNED.csv")
t13 = read.csv("./results/passage times/Tuesday ARIMA-corrected 2013 global 2026-06-18 THINNED.csv")

pt.all = rbind(t25, t24, t15, t14, t13) %>% 
  mutate(hours = minutes/60) 

green_palette <- c("#CBD4AC", "#b4c187", "#80914b", "#5a6b3a", "#496231")
brown_palette <- c("#CFB491", "#9c7744", "#8c5c2b", "#533113", "#361c07")

L.letters.tues <- tukey_all %>% filter(lake == "Tuesday")
lb.grid.tues <- build_letter_grid(L.letters.tues %>% filter(basin == "left"))
rb.grid.tues <- build_letter_grid(L.letters.tues %>% filter(basin == "right"))

L.letters.tues <- L.letters.tues %>%
  rowwise() %>%
  mutate(Letters_colored = color_letters(Letters)) %>%
  ungroup()


### right basin (high-pigment) ###
rb.plot = ggplot(pt.all %>% filter(basin == "right"), aes(x = as.factor(year), y = (hours)/24, fill = factor(year))) +
  geom_boxplot(alpha = 0.8)+
  labs(y = "", x = "Year", title = "high-pigment") +
  scale_fill_manual(values = green_palette)+
  theme_classic()+
  scale_y_log10(breaks = c(0.1, 1, 10, 100), limits = c(0.0005, 175))+
  geom_richtext(
    data = L.letters.tues %>% filter(basin == "right"),
    aes(x = year, y = y_pos, label = Letters_colored),
    inherit.aes = FALSE, size = 3,
    fill = NA, label.color = NA,   
    fontface = "bold"
  )+
  new_scale_fill() +
  geom_rect(
    data = rb.grid.tues,
    aes(xmin = as.numeric(as.factor(year)) - 0.4, xmax = as.numeric(as.factor(year)) + 0.4,
        ymin = ymin, ymax = ymax, fill = fill_letter),
    inherit.aes = FALSE, color = "grey50", linewidth = 0.2
  ) +
  scale_fill_manual(values = letter_colors, na.value = "white", guide = "none") +
  theme(
    axis.text.y  = element_text(size = 12),
    axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
    axis.title = element_text(size = 12),
    strip.text = element_text(size = 12),
    legend.position = "none",
    plot.title = ggtext::element_textbox_simple(
      fill = "#5a6b3a",
      color = "white",
      face = "bold",
      size = 12,
      halign = 0.5,
      linetype = 1,
      box.color = "black",
      linewidth = 0.5,
      padding = margin(4, 4, 4, 4),
      margin = margin(b = 6)
    )
  ) +
  theme(plot.margin = margin(l = 0, r = 0, t = 0, b = 0))


### left basin (low-pigment) ###
lb.plot = ggplot(pt.all %>% filter(basin == "left"), aes(x = as.factor(year), y = (hours)/24, fill = factor(year))) +
  geom_boxplot(alpha = 0.8)+
  labs(y = "passage time (days)", x = "Year", title = "low-pigment") +
  scale_fill_manual(values = rev(brown_palette))+
  theme_classic()+
  scale_y_log10(breaks = c(0.1, 1, 10, 100), limits = c(0.0005, 175))+
  geom_richtext(
    data = L.letters.tues %>% filter(basin == "left"),
    aes(x = year, y = y_pos, label = Letters_colored),
    inherit.aes = FALSE, size = 3,
    fill = NA, label.color = NA,
    fontface = "bold"
  )+
  new_scale_fill() +
  geom_rect(
    data = lb.grid.tues,
    aes(xmin = as.numeric(as.factor(year)) - 0.4, xmax = as.numeric(as.factor(year)) + 0.4,
        ymin = ymin, ymax = ymax, fill = fill_letter),
    inherit.aes = FALSE, color = "grey50", linewidth = 0.2
  ) +
  scale_fill_manual(values = letter_colors, na.value = "white", guide = "none") +
  theme(
    axis.text.y  = element_text(size = 12),
    axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
    axis.title = element_text(size = 12),
    strip.text = element_text(size = 12),
    legend.position = "none",
    plot.title = ggtext::element_textbox_simple(
      fill = "#755A42",
      color = "white",
      face = "bold",
      size = 12,
      halign = 0.5,
      linetype = 1,
      box.color = "black",
      linewidth = 0.5,
      padding = margin(4, 4, 4, 4),
      margin = margin(b = 6)
    )
  ) +
  theme(plot.margin = margin(l = 0, r = 0, t = 0, b = 0))


# banner plot
banner = ggplot() +
  theme_void() +
  theme(plot.background = element_rect(fill = "#755A42", color = "black")) +
  annotate("text", x = 0.5, y = 0.5, label = "Tuesday Lake (experimental)",
           color = "white", fontface = "bold", size = 5) +
  xlim(0, 1) + ylim(0, 1)

pt.row = ggarrange(lb.plot, rb.plot, nrow = 1, ncol = 2)
tues.pt = ggarrange(banner, pt.row, nrow = 2, heights = c(0.08, 1))



### Paul

t25 = read.csv("./results/passage times/Paul ARIMA-corrected 2025 global 2026-06-18 THINNED.csv")
t24 = read.csv("./results/passage times/Paul ARIMA-corrected 2024 global 2026-06-18 THINNED.csv")
t15 = read.csv("./results/passage times/Paul ARIMA-corrected 2015 global 2026-06-18 THINNED.csv")
t14 = read.csv("./results/passage times/Paul ARIMA-corrected 2014 global 2026-06-18 THINNED.csv")
t13 = read.csv("./results/passage times/Paul ARIMA-corrected 2013 global 2026-06-18 THINNED.csv")

pt.all = rbind(t25, t24, t15, t14, t13) %>% 
  mutate(hours = minutes/60) 

green_palette <- c("#b4c187")
blue_palette <- c("#B4C5CF", "#44729C", "#2B5A8C", "#133353", "#071C36")

L.letters.paul <- tukey_all %>% filter(lake == "Paul")
lb.grid.paul <- build_letter_grid(L.letters.paul %>% filter(basin == "left"))
rb.grid.paul <- build_letter_grid(L.letters.paul %>% filter(basin == "right"))

# add in colors
L.letters.paul <- L.letters.paul %>%
  rowwise() %>%
  mutate(Letters_colored = color_letters(Letters)) %>%
  ungroup()

### right basin (high-pigment) ###
rb.plot = ggplot(pt.all %>% filter(basin == "right"), aes(x = as.factor(year), y = (hours)/24, fill = factor(year))) +
  geom_boxplot(alpha = 0.8)+
  labs(y = "", x = "Year", title = "high-pigment") +
  scale_fill_manual(values = rep(green_palette, 5))+
  theme_classic()+
  scale_y_log10(breaks = c(0.1, 1, 10, 100), limits = c(0.0005, 175))+
  geom_richtext(
    data = L.letters.paul %>% filter(basin == "right"),
    aes(x = year, y = y_pos, label = Letters_colored),
    inherit.aes = FALSE, size = 3,
    fill = NA, label.color = NA,
    fontface = "bold"
  )+
  new_scale_fill() +
  geom_rect(
    data = rb.grid.paul,
    aes(xmin = as.numeric(as.factor(year)) - 0.4, xmax = as.numeric(as.factor(year)) + 0.4,
        ymin = ymin, ymax = ymax, fill = fill_letter),
    inherit.aes = FALSE, color = "grey50", linewidth = 0.2
  ) +
  scale_fill_manual(values = letter_colors, na.value = "white", guide = "none") +
  theme(
    axis.text.y  = element_text(size = 12),
    axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
    axis.title = element_text(size = 12),
    strip.text = element_text(size = 12),
    legend.position = "none",
    plot.title = ggtext::element_textbox_simple(
      fill = "#b4c187",
      color = "white",
      face = "bold",
      size = 12,
      halign = 0.5,
      linetype = 1,
      box.color = "black",
      linewidth = 0.5,
      padding = margin(4, 4, 4, 4),
      margin = margin(b = 6)
    )
  ) +
  theme(plot.margin = margin(l = 0, r = 0, t = 0, b = 0))


### left basin (low-pigment) ###
lb.plot = ggplot(pt.all %>% filter(basin == "left"), aes(x = as.factor(year), y = (hours)/24, fill = factor(year))) +
  geom_boxplot(alpha = 0.8)+
  labs(y = "", x = "Year", title = "low-pigment") +
  scale_fill_manual(values = rev(blue_palette))+
  theme_classic()+
  scale_y_log10(breaks = c(0.1, 1, 10, 100), limits = c(0.0005, 175))+
  geom_richtext(
    data = L.letters.paul %>% filter(basin == "left"),
    aes(x = year, y = y_pos, label = Letters_colored),
    inherit.aes = FALSE, size = 3,
    fill = NA, label.color = NA,
    fontface = "bold"
  )+
  new_scale_fill() +
  geom_rect(
    data = lb.grid.paul,
    aes(xmin = as.numeric(as.factor(year)) - 0.4, xmax = as.numeric(as.factor(year)) + 0.4,
        ymin = ymin, ymax = ymax, fill = fill_letter),
    inherit.aes = FALSE, color = "grey50", linewidth = 0.2
  ) +
  scale_fill_manual(values = letter_colors, na.value = "white", guide = "none") +
  theme(
    axis.text.y  = element_text(size = 12),
    axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
    axis.title = element_text(size = 12),
    strip.text = element_text(size = 12),
    legend.position = "none",
    plot.title = ggtext::element_textbox_simple(
      fill = "#44729C",
      color = "white",
      face = "bold",
      size = 12,
      halign = 0.5,
      linetype = 1,
      box.color = "black",
      linewidth = 0.5,
      padding = margin(4, 4, 4, 4),
      margin = margin(b = 6)
    )
  ) +
  theme(plot.margin = margin(l = 0, r = 0, t = 0, b = 0))


# banner plot
banner = ggplot() +
  theme_void() +
  theme(plot.background = element_rect(fill = "#44729C", color = "black")) +
  annotate("text", x = 0.5, y = 0.5, label = "Paul Lake (reference)",
           color = "white", fontface = "bold", size = 5) +
  xlim(0, 1) + ylim(0, 1)

pt.row = ggarrange(lb.plot, rb.plot, nrow = 1, ncol = 2)
paul.pt = ggarrange(banner, pt.row, nrow = 2, heights = c(0.08, 1))


# combine Paul and Tuesday plots
png("./figures/Supplemental figures/Figure X PT differences.png", res = 600, height = 100, width = 250, units = "mm") 

ggarrange(tues.pt, paul.pt, nrow = 1, ncol = 2)

dev.off()
