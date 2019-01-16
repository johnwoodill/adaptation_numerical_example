library(tidyverse)
library(ggthemes)
library(cowplot)
library(numDeriv)

# Set bins for main analysis
bins <- 10

# Load data for main analysis with 10-bins
tavg_dat <- readRDS("data/tavg_dat_bins_10.rds")
dday10_30_dat <- readRDS("data/dday10_30_dat_bins_10.rds")
dday30_dat <- readRDS("data/dday30_dat_bins_10.rds")


# Tavg 10-bin plot
tavg_pdat <- gather(tavg_dat, key = crop_var, value = value, -tavg)
tavg_pdat$crop_var <- factor(tavg_pdat$crop_var, 
                             levels = c('corn_rev', 'cotton_rev', 'hay_rev', 'wheat_rev', 'soybean_rev',
                                        'p_corn_a', 'p_cotton_a', 'p_hay_a', 'p_wheat_a', 'p_soybean_a',
                                        'corn_acres', 'cotton_acres', 'hay_acres', 'wheat_acres', 'soybean_acres'),
                             labels = c("Corn Rev.", "Cotton Rev.", "Hay Rev", "Wheat Rev.", "Soybean Rev.",
                                        "P(Corn Acres)", "P(Cotton Acres)", "P(Hay Acres)", "P(Wheat Acres)", "P(Soybean Acres)",
                                        "Corn Acres", "Cotton Acres", "Hay Acres", "Wheat Acres", "Soybean Acres"))
ggplot(filter(tavg_pdat, crop_var %in% c("Corn Rev.", "Cotton Rev.", "Hay Rev", "Wheat Rev.", "Soybean Rev.",
                                        "P(Corn Acres)", "P(Cotton Acres)", "P(Hay Acres)", "P(Wheat Acres)", "P(Soybean Acres)")), aes(x = tavg, y = value, color = crop_var)) + 
  theme_tufte() +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  geom_line() +
  facet_wrap(~crop_var, scales = 'free', ncol = 5) +
  theme(legend.position = 'none') +
  ylab(NULL) +
  xlab("Average Temperature") +
  scale_x_continuous(breaks = seq(10, 30, by = 4)) +
  NULL
ggsave("figures/1-bins_tavg.pdf", width = 7, height = 4)

# Degree Day 10_30 1-bin plot
dday10_30_pdat <- gather(dday10_30_dat, key = crop_var, value = value, -dday10_30)
dday10_30_pdat$crop_var <- factor(dday10_30_pdat$crop_var, 
                              levels = c('corn_rev', 'cotton_rev', 'hay_rev', 'wheat_rev', 'soybean_rev',
                                        'p_corn_a', 'p_cotton_a', 'p_hay_a', 'p_wheat_a', 'p_soybean_a',
                                        'corn_acres', 'cotton_acres', 'hay_acres', 'wheat_acres', 'soybean_acres'),
                             labels = c("Corn Rev.", "Cotton Rev.", "Hay Rev", "Wheat Rev.", "Soybean Rev.",
                                        "P(Corn Acres)", "P(Cotton Acres)", "P(Hay Acres)", "P(Wheat Acres)", "P(Soybean Acres)",
                                        "Corn Acres", "Cotton Acres", "Hay Acres", "Wheat Acres", "Soybean Acres"))

ggplot(filter(dday10_30_pdat, crop_var %in% c("Corn Rev.", "Cotton Rev.", "Hay Rev", "Wheat Rev.", "Soybean Rev.",
                                        "P(Corn Acres)", "P(Cotton Acres)", "P(Hay Acres)", "P(Wheat Acres)", "P(Soybean Acres)")), aes(x = dday10_30, y = value, color = crop_var)) + 
  theme_tufte() +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  geom_line() +
  facet_wrap(~crop_var, scales = 'free', ncol = 5) +
  theme(legend.position = 'none',
        axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylab(NULL) +
  xlab("Degree Days (10-30C)") +
  scale_x_continuous(breaks = c(930, 1300, 1700, 2100, 2500, 2900)) +
  
  NULL

ggsave("figures/2-bins_dday10_30.pdf", width = 7, height = 4)

# Degree Day 30 10-bin Plot
dday30_pdat <- gather(dday30_dat, key = crop_var, value = value, -dday30)
dday30_pdat$crop_var <- factor(dday30_pdat$crop_var, 
                             levels = c('corn_rev', 'cotton_rev', 'hay_rev', 'wheat_rev', 'soybean_rev',
                                        'p_corn_a', 'p_cotton_a', 'p_hay_a', 'p_wheat_a', 'p_soybean_a',
                                        'corn_acres', 'cotton_acres', 'hay_acres', 'wheat_acres', 'soybean_acres'),
                             labels = c("Corn Rev.", "Cotton Rev.", "Hay Rev", "Wheat Rev.", "Soybean Rev.",
                                        "P(Corn Acres)", "P(Cotton Acres)", "P(Hay Acres)", "P(Wheat Acres)", "P(Soybean Acres)",
                                        "Corn Acres", "Cotton Acres", "Hay Acres", "Wheat Acres", "Soybean Acres"))

ggplot(filter(dday30_pdat, crop_var %in% c("Corn Rev.", "Cotton Rev.", "Hay Rev", "Wheat Rev.", "Soybean Rev.",
                                        "P(Corn Acres)", "P(Cotton Acres)", "P(Hay Acres)", "P(Wheat Acres)", "P(Soybean Acres)")), aes(x = dday30, y = value, color = crop_var)) + 
  theme_tufte() +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  geom_line() +
  facet_wrap(~crop_var, scales = 'free', ncol = 5) +
  theme(legend.position = 'none',
        axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylab(NULL) +
  xlab("Degree Days (30C)") +
  scale_x_continuous(breaks = c(3, 30, 56, 82, 109, 135, 165)) +
  NULL

ggsave("figures/3-bins_dday30.pdf", width = 7, height = 4)

# Linear interpolation within bins
tavg_corn_fn <- approxfun(tavg_dat$tavg, tavg_dat$corn_rev)
tavg_cotton_fn <- approxfun(tavg_dat$tavg, tavg_dat$cotton_rev)
tavg_hay_fn <- approxfun(tavg_dat$tavg, tavg_dat$hay_rev)
tavg_wheat_fn <- approxfun(tavg_dat$tavg, tavg_dat$wheat_rev)
tavg_soybean_fn <- approxfun(tavg_dat$tavg, tavg_dat$soybean_rev)

dday10_30_corn_fn <- approxfun(dday10_30_dat$dday10_30, dday10_30_dat$corn_rev)
dday10_30_cotton_fn <- approxfun(dday10_30_dat$dday10_30, dday10_30_dat$cotton_rev)
dday10_30_hay_fn <- approxfun(dday10_30_dat$dday10_30, dday10_30_dat$hay_rev)
dday10_30_wheat_fn <- approxfun(dday10_30_dat$dday10_30, dday10_30_dat$wheat_rev)
dday10_30_soybean_fn <- approxfun(dday10_30_dat$dday10_30, dday10_30_dat$soybean_rev)

dday30_corn_fn <- approxfun(dday30_dat$dday30, dday30_dat$corn_rev)
dday30_cotton_fn <- approxfun(dday30_dat$dday30, dday30_dat$cotton_rev)
dday30_hay_fn <- approxfun(dday30_dat$dday30, dday30_dat$hay_rev)
dday30_wheat_fn <- approxfun(dday30_dat$dday30, dday30_dat$wheat_rev)
dday30_soybean_fn <- approxfun(dday30_dat$dday30, dday30_dat$soybean_rev)

tavg_p_corn_fn <- approxfun(tavg_dat$tavg, tavg_dat$p_corn_a)
tavg_p_cotton_fn <- approxfun(tavg_dat$tavg, tavg_dat$p_cotton_a)
tavg_p_hay_fn <- approxfun(tavg_dat$tavg, tavg_dat$p_hay_a)
tavg_p_wheat_fn <- approxfun(tavg_dat$tavg, tavg_dat$p_wheat_a)
tavg_p_soybean_fn <- approxfun(tavg_dat$tavg, tavg_dat$p_soybean_a)

dday10_30_p_corn_fn <- approxfun(dday10_30_dat$dday10_30, dday10_30_dat$p_corn_a)
dday10_30_p_cotton_fn <- approxfun(dday10_30_dat$dday10_30, dday10_30_dat$p_cotton_a)
dday10_30_p_hay_fn <- approxfun(dday10_30_dat$dday10_30, dday10_30_dat$p_hay_a)
dday10_30_p_wheat_fn <- approxfun(dday10_30_dat$dday10_30, dday10_30_dat$p_wheat_a)
dday10_30_p_soybean_fn <- approxfun(dday10_30_dat$dday10_30, dday10_30_dat$p_soybean_a)

dday30_p_corn_fn <- approxfun(dday30_dat$dday30, dday30_dat$p_corn_a)
dday30_p_cotton_fn <- approxfun(dday30_dat$dday30, dday30_dat$p_cotton_a)
dday30_p_hay_fn <- approxfun(dday30_dat$dday30, dday30_dat$p_hay_a)
dday30_p_wheat_fn <- approxfun(dday30_dat$dday30, dday30_dat$p_wheat_a)
dday30_p_soybean_fn <- approxfun(dday30_dat$dday30, dday30_dat$p_soybean_a)

# Build data.frame to plot
int_pdat <- data.frame(temp = rep(c("Average Temp. (C)", "Degree Day (10-30C)", "Degree Day (30C)"), each = bins*5),
                       crop = rep(c("Corn", "Cotton", "Hay", "Wheat", "Soybean"), each = bins),
                       x = c(rep(tavg_dat$tavg, 5), rep(dday10_30_dat$dday10_30, 5), rep(dday30_dat$dday30, 5)),
                       y = c(tavg_corn_fn(tavg_dat$tavg), tavg_cotton_fn(tavg_dat$tavg), tavg_hay_fn(tavg_dat$tavg), tavg_wheat_fn(tavg_dat$tavg), tavg_soybean_fn(tavg_dat$tavg),
                             dday10_30_corn_fn(dday10_30_dat$dday10_30), dday10_30_cotton_fn(dday10_30_dat$dday10_30), dday10_30_hay_fn(dday10_30_dat$dday10_30), dday10_30_wheat_fn(dday10_30_dat$dday10_30), dday10_30_soybean_fn(dday10_30_dat$dday10_30),
                             dday30_corn_fn(dday30_dat$dday30), dday30_cotton_fn(dday30_dat$dday30), dday30_hay_fn(dday30_dat$dday30), dday30_wheat_fn(dday30_dat$dday30), dday30_soybean_fn(dday30_dat$dday30)))

# Combine all temperature plots into one for appendix
ggplot(int_pdat, aes(x=x, y=y, color = crop)) + 
  theme_tufte() +    
  geom_line() + 
  ylab("Revenue/Acre") +
  xlab(NULL) +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  facet_wrap(temp~crop) +
  theme(legend.position = 'none') +
  facet_wrap(temp~crop, scales = 'free', ncol = 5)
ggsave("figures/a1-interp_rev_plot.pdf", height = 6, width = 8)

#---------------------------------------------
# Total Effect
# Average Temperatures

# Get temperature and cut end-points
tavg <- tavg_dat$tavg
tavg[1] <- tavg_dat$tavg[1] + 0.1
tavg[length(tavg_dat$tavg)] <- tavg[length(tavg_dat$tavg)] - 0.1

# Aggregate effects across uniform increases in temperature
tavg_base <- sum(tavg_dat$corn_rev*tavg_dat$corn_acres) +
        sum(tavg_dat$cotton_rev*tavg_dat$cotton_acres) +
        sum(tavg_dat$hay_rev*tavg_dat$hay_acres) +
        sum(tavg_dat$wheat_rev*tavg_dat$wheat_acres) +
        sum(tavg_dat$soybean_rev*tavg_dat$soybean_acres)
  
tavg_temp1 <- tavg_base + sum((tavg_corn_fn(tavg_dat$tavg + 1)*tavg_dat$corn_acres - tavg_corn_fn(tavg_dat$tavg)*tavg_dat$corn_acres), na.rm = TRUE) +
  sum((tavg_cotton_fn(tavg_dat$tavg + 1)*tavg_dat$cotton_acres - tavg_cotton_fn(tavg_dat$tavg)*tavg_dat$cotton_acres), na.rm = TRUE) +
  sum((tavg_hay_fn(tavg_dat$tavg + 1)*tavg_dat$hay_acres - tavg_hay_fn(tavg_dat$tavg)*tavg_dat$hay_acres), na.rm = TRUE) +
  sum((tavg_wheat_fn(tavg_dat$tavg + 1)*tavg_dat$wheat_acres - tavg_wheat_fn(tavg_dat$tavg)*tavg_dat$wheat_acres), na.rm = TRUE) +
  sum((tavg_soybean_fn(tavg_dat$tavg + 1)*tavg_dat$soybean_acres - tavg_soybean_fn(tavg_dat$tavg)*tavg_dat$soybean_acres), na.rm = TRUE)
  
tavg_temp2 <- tavg_base + sum((tavg_corn_fn(tavg_dat$tavg + 2)*tavg_dat$corn_acres - tavg_corn_fn(tavg_dat$tavg)*tavg_dat$corn_acres), na.rm = TRUE) +
  sum((tavg_cotton_fn(tavg_dat$tavg + 2)*tavg_dat$cotton_acres - tavg_cotton_fn(tavg_dat$tavg)*tavg_dat$cotton_acres), na.rm = TRUE) +
  sum((tavg_hay_fn(tavg_dat$tavg + 2)*tavg_dat$hay_acres - tavg_hay_fn(tavg_dat$tavg)*tavg_dat$hay_acres), na.rm = TRUE) +
  sum((tavg_wheat_fn(tavg_dat$tavg + 2)*tavg_dat$wheat_acres - tavg_wheat_fn(tavg_dat$tavg)*tavg_dat$wheat_acres), na.rm = TRUE) +
  sum((tavg_soybean_fn(tavg_dat$tavg + 2)*tavg_dat$soybean_acres - tavg_soybean_fn(tavg_dat$tavg)*tavg_dat$soybean_acres), na.rm = TRUE)
  
tavg_temp3 <- tavg_base + sum((tavg_corn_fn(tavg_dat$tavg + 3)*tavg_dat$corn_acres - tavg_corn_fn(tavg_dat$tavg)*tavg_dat$corn_acres), na.rm = TRUE) +
  sum((tavg_cotton_fn(tavg_dat$tavg + 3)*tavg_dat$cotton_acres - tavg_cotton_fn(tavg_dat$tavg)*tavg_dat$cotton_acres), na.rm = TRUE) +
  sum((tavg_hay_fn(tavg_dat$tavg + 3)*tavg_dat$hay_acres - tavg_hay_fn(tavg_dat$tavg)*tavg_dat$hay_acres), na.rm = TRUE) +
  sum((tavg_wheat_fn(tavg_dat$tavg + 3)*tavg_dat$wheat_acres - tavg_wheat_fn(tavg_dat$tavg)*tavg_dat$wheat_acres), na.rm = TRUE) +
  sum((tavg_soybean_fn(tavg_dat$tavg + 3)*tavg_dat$soybean_acres - tavg_soybean_fn(tavg_dat$tavg)*tavg_dat$soybean_acres), na.rm = TRUE)
  
tavg_temp4 <- tavg_base + sum((tavg_corn_fn(tavg_dat$tavg + 4)*tavg_dat$corn_acres - tavg_corn_fn(tavg_dat$tavg)*tavg_dat$corn_acres), na.rm = TRUE) +
  sum((tavg_cotton_fn(tavg_dat$tavg + 4)*tavg_dat$cotton_acres - tavg_cotton_fn(tavg_dat$tavg)*tavg_dat$cotton_acres), na.rm = TRUE) +
  sum((tavg_hay_fn(tavg_dat$tavg + 4)*tavg_dat$hay_acres - tavg_hay_fn(tavg_dat$tavg)*tavg_dat$hay_acres), na.rm = TRUE) +
  sum((tavg_wheat_fn(tavg_dat$tavg + 4)*tavg_dat$wheat_acres - tavg_wheat_fn(tavg_dat$tavg)*tavg_dat$wheat_acres), na.rm = TRUE) +
  sum((tavg_soybean_fn(tavg_dat$tavg + 4)*tavg_dat$soybean_acres - tavg_soybean_fn(tavg_dat$tavg)*tavg_dat$soybean_acres), na.rm = TRUE)
  
tavg_temp5 <- tavg_base + sum((tavg_corn_fn(tavg_dat$tavg + 5)*tavg_dat$corn_acres - tavg_corn_fn(tavg_dat$tavg)*tavg_dat$corn_acres), na.rm = TRUE) +
  sum((tavg_cotton_fn(tavg_dat$tavg + 5)*tavg_dat$cotton_acres - tavg_cotton_fn(tavg_dat$tavg)*tavg_dat$cotton_acres), na.rm = TRUE) +
  sum((tavg_hay_fn(tavg_dat$tavg + 5)*tavg_dat$hay_acres - tavg_hay_fn(tavg_dat$tavg)*tavg_dat$hay_acres), na.rm = TRUE) +
  sum((tavg_wheat_fn(tavg_dat$tavg + 5)*tavg_dat$wheat_acres - tavg_wheat_fn(tavg_dat$tavg)*tavg_dat$wheat_acres), na.rm = TRUE) +
  sum((tavg_soybean_fn(tavg_dat$tavg + 5)*tavg_dat$soybean_acres - tavg_soybean_fn(tavg_dat$tavg)*tavg_dat$soybean_acres), na.rm = TRUE)

# Build data.frame of results
adapt1_tavg <- data.frame(temp = "Average Temperature (C)",
                          model = "Total Effect",
                     rev = c(tavg_base, tavg_temp1, tavg_temp2, tavg_temp3, tavg_temp4, tavg_temp5))
adapt1_tavg$change = (adapt1_tavg$rev - first(adapt1_tavg$rev))/first(adapt1_tavg$rev)*100

# Checks
adapt1_tavg
plot(x=0:5, adapt1_tavg$change)
lines(x=0:5, adapt1_tavg$change)


# Degree Days 10-30

# Cut end-points
dday10_30 <- dday10_30_dat$dday10_30
dday10_30[1] <- dday10_30_dat$dday10_30[1] + 0.1
dday10_30[length(dday10_30_dat$dday10_30)] <- dday10_30[length(dday10_30_dat$dday10_30)] - 0.1

# Aggregate result with increases in uniform temperatures
dday10_30_base <- sum(dday10_30_dat$corn_rev*dday10_30_dat$corn_acres) +
        sum(dday10_30_dat$cotton_rev*dday10_30_dat$cotton_acres) +
        sum(dday10_30_dat$hay_rev*dday10_30_dat$hay_acres) +
        sum(dday10_30_dat$wheat_rev*dday10_30_dat$wheat_acres) +
        sum(dday10_30_dat$soybean_rev*dday10_30_dat$soybean_acres)
  
dday10_30_temp1 <- dday10_30_base + sum((dday10_30_corn_fn(dday10_30_dat$dday10_30 + 140)*dday10_30_dat$corn_acres - dday10_30_corn_fn(dday10_30_dat$dday10_30)*dday10_30_dat$corn_acres), na.rm = TRUE) +
  sum((dday10_30_cotton_fn(dday10_30_dat$dday10_30 + 140)*dday10_30_dat$cotton_acres - dday10_30_cotton_fn(dday10_30_dat$dday10_30)*dday10_30_dat$cotton_acres), na.rm = TRUE) +
  sum((dday10_30_hay_fn(dday10_30_dat$dday10_30 + 140)*dday10_30_dat$hay_acres - dday10_30_hay_fn(dday10_30_dat$dday10_30)*dday10_30_dat$hay_acres), na.rm = TRUE) +
  sum((dday10_30_wheat_fn(dday10_30_dat$dday10_30 + 140)*dday10_30_dat$wheat_acres - dday10_30_wheat_fn(dday10_30_dat$dday10_30)*dday10_30_dat$wheat_acres), na.rm = TRUE) +
  sum((dday10_30_soybean_fn(dday10_30_dat$dday10_30 + 140)*dday10_30_dat$soybean_acres - dday10_30_soybean_fn(dday10_30_dat$dday10_30)*dday10_30_dat$soybean_acres), na.rm = TRUE)
  
dday10_30_temp2 <- dday10_30_base + sum((dday10_30_corn_fn(dday10_30_dat$dday10_30 + 280)*dday10_30_dat$corn_acres - dday10_30_corn_fn(dday10_30_dat$dday10_30)*dday10_30_dat$corn_acres), na.rm = TRUE) +
  sum((dday10_30_cotton_fn(dday10_30_dat$dday10_30 + 280)*dday10_30_dat$cotton_acres - dday10_30_cotton_fn(dday10_30_dat$dday10_30)*dday10_30_dat$cotton_acres), na.rm = TRUE) +
  sum((dday10_30_hay_fn(dday10_30_dat$dday10_30 + 280)*dday10_30_dat$hay_acres - dday10_30_hay_fn(dday10_30_dat$dday10_30)*dday10_30_dat$hay_acres), na.rm = TRUE) +
  sum((dday10_30_wheat_fn(dday10_30_dat$dday10_30 + 280)*dday10_30_dat$wheat_acres - dday10_30_wheat_fn(dday10_30_dat$dday10_30)*dday10_30_dat$wheat_acres), na.rm = TRUE) +
  sum((dday10_30_soybean_fn(dday10_30_dat$dday10_30 + 280)*dday10_30_dat$soybean_acres - dday10_30_soybean_fn(dday10_30_dat$dday10_30)*dday10_30_dat$soybean_acres), na.rm = TRUE)
  
dday10_30_temp3 <- dday10_30_base + sum((dday10_30_corn_fn(dday10_30_dat$dday10_30 + 420)*dday10_30_dat$corn_acres - dday10_30_corn_fn(dday10_30_dat$dday10_30)*dday10_30_dat$corn_acres), na.rm = TRUE) +
  sum((dday10_30_cotton_fn(dday10_30_dat$dday10_30 + 420)*dday10_30_dat$cotton_acres - dday10_30_cotton_fn(dday10_30_dat$dday10_30)*dday10_30_dat$cotton_acres), na.rm = TRUE) +
  sum((dday10_30_hay_fn(dday10_30_dat$dday10_30 + 420)*dday10_30_dat$hay_acres - dday10_30_hay_fn(dday10_30_dat$dday10_30)*dday10_30_dat$hay_acres), na.rm = TRUE) +
  sum((dday10_30_wheat_fn(dday10_30_dat$dday10_30 + 420)*dday10_30_dat$wheat_acres - dday10_30_wheat_fn(dday10_30_dat$dday10_30)*dday10_30_dat$wheat_acres), na.rm = TRUE) +
  sum((dday10_30_soybean_fn(dday10_30_dat$dday10_30 + 420)*dday10_30_dat$soybean_acres - dday10_30_soybean_fn(dday10_30_dat$dday10_30)*dday10_30_dat$soybean_acres), na.rm = TRUE)
  
dday10_30_temp4 <- dday10_30_base + sum((dday10_30_corn_fn(dday10_30_dat$dday10_30 + 560)*dday10_30_dat$corn_acres - dday10_30_corn_fn(dday10_30_dat$dday10_30)*dday10_30_dat$corn_acres), na.rm = TRUE) +
  sum((dday10_30_cotton_fn(dday10_30_dat$dday10_30 + 560)*dday10_30_dat$cotton_acres - dday10_30_cotton_fn(dday10_30_dat$dday10_30)*dday10_30_dat$cotton_acres), na.rm = TRUE) +
  sum((dday10_30_hay_fn(dday10_30_dat$dday10_30 + 560)*dday10_30_dat$hay_acres - dday10_30_hay_fn(dday10_30_dat$dday10_30)*dday10_30_dat$hay_acres), na.rm = TRUE) +
  sum((dday10_30_wheat_fn(dday10_30_dat$dday10_30 + 560)*dday10_30_dat$wheat_acres - dday10_30_wheat_fn(dday10_30_dat$dday10_30)*dday10_30_dat$wheat_acres), na.rm = TRUE) +
  sum((dday10_30_soybean_fn(dday10_30_dat$dday10_30 + 560)*dday10_30_dat$soybean_acres - dday10_30_soybean_fn(dday10_30_dat$dday10_30)*dday10_30_dat$soybean_acres), na.rm = TRUE)
  
dday10_30_temp5 <- dday10_30_base + sum((dday10_30_corn_fn(dday10_30_dat$dday10_30 + 700)*dday10_30_dat$corn_acres - dday10_30_corn_fn(dday10_30_dat$dday10_30)*dday10_30_dat$corn_acres), na.rm = TRUE) +
  sum((dday10_30_cotton_fn(dday10_30_dat$dday10_30 + 700)*dday10_30_dat$cotton_acres - dday10_30_cotton_fn(dday10_30_dat$dday10_30)*dday10_30_dat$cotton_acres), na.rm = TRUE) +
  sum((dday10_30_hay_fn(dday10_30_dat$dday10_30 + 700)*dday10_30_dat$hay_acres - dday10_30_hay_fn(dday10_30_dat$dday10_30)*dday10_30_dat$hay_acres), na.rm = TRUE) +
  sum((dday10_30_wheat_fn(dday10_30_dat$dday10_30 + 700)*dday10_30_dat$wheat_acres - dday10_30_wheat_fn(dday10_30_dat$dday10_30)*dday10_30_dat$wheat_acres), na.rm = TRUE) +
  sum((dday10_30_soybean_fn(dday10_30_dat$dday10_30 + 700)*dday10_30_dat$soybean_acres - dday10_30_soybean_fn(dday10_30_dat$dday10_30)*dday10_30_dat$soybean_acres), na.rm = TRUE)
  

# Build data.frame of results
adapt1_dday10_30 <- data.frame(temp = "Degree Day (10-30C)",
                          model = "Total Effect",
                     rev = c(dday10_30_base, dday10_30_temp1, dday10_30_temp2, dday10_30_temp3, dday10_30_temp4, dday10_30_temp5))
adapt1_dday10_30$change = (adapt1_dday10_30$rev - first(adapt1_dday10_30$rev))/first(adapt1_dday10_30$rev)*100

# Check
adapt1_dday10_30
plot(x=0:5, adapt1_dday10_30$change)
lines(x=0:5, adapt1_dday10_30$change)

# Degree Days 30

# Cut end-points
dday30 <- dday30_dat$dday30
dday30[1] <- dday30_dat$dday30[1] + 0.1
dday30[length(dday30_dat$dday30)] <- dday30[length(dday30_dat$dday30)] - 0.1

# Aggregate based on uniform increases in temperatures
dday30_base <- sum(dday30_dat$corn_rev*dday30_dat$corn_acres) +
        sum(dday30_dat$cotton_rev*dday30_dat$cotton_acres) +
        sum(dday30_dat$hay_rev*dday30_dat$hay_acres) +
        sum(dday30_dat$wheat_rev*dday30_dat$wheat_acres) +
        sum(dday30_dat$soybean_rev*dday30_dat$soybean_acres)
  
dday30_temp1 <- dday30_base + sum((dday30_corn_fn(dday30_dat$dday30 + 15)*dday30_dat$corn_acres - dday30_corn_fn(dday30_dat$dday30)*dday30_dat$corn_acres), na.rm = TRUE) +
  sum((dday30_cotton_fn(dday30_dat$dday30 + 15)*dday30_dat$cotton_acres - dday30_cotton_fn(dday30_dat$dday30)*dday30_dat$cotton_acres), na.rm = TRUE) +
  sum((dday30_hay_fn(dday30_dat$dday30 + 15)*dday30_dat$hay_acres - dday30_hay_fn(dday30_dat$dday30)*dday30_dat$hay_acres), na.rm = TRUE) +
  sum((dday30_wheat_fn(dday30_dat$dday30 + 15)*dday30_dat$wheat_acres - dday30_wheat_fn(dday30_dat$dday30)*dday30_dat$wheat_acres), na.rm = TRUE) +
  sum((dday30_soybean_fn(dday30_dat$dday30 + 15)*dday30_dat$soybean_acres - dday30_soybean_fn(dday30_dat$dday30)*dday30_dat$soybean_acres), na.rm = TRUE)
  
dday30_temp2 <- dday30_base + sum((dday30_corn_fn(dday30_dat$dday30 + 30)*dday30_dat$corn_acres - dday30_corn_fn(dday30_dat$dday30)*dday30_dat$corn_acres), na.rm = TRUE) +
  sum((dday30_cotton_fn(dday30_dat$dday30 + 30)*dday30_dat$cotton_acres - dday30_cotton_fn(dday30_dat$dday30)*dday30_dat$cotton_acres), na.rm = TRUE) +
  sum((dday30_hay_fn(dday30_dat$dday30 + 30)*dday30_dat$hay_acres - dday30_hay_fn(dday30_dat$dday30)*dday30_dat$hay_acres), na.rm = TRUE) +
  sum((dday30_wheat_fn(dday30_dat$dday30 + 30)*dday30_dat$wheat_acres - dday30_wheat_fn(dday30_dat$dday30)*dday30_dat$wheat_acres), na.rm = TRUE) +
  sum((dday30_soybean_fn(dday30_dat$dday30 + 30)*dday30_dat$soybean_acres - dday30_soybean_fn(dday30_dat$dday30)*dday30_dat$soybean_acres), na.rm = TRUE)
  
dday30_temp3 <- dday30_base + sum((dday30_corn_fn(dday30_dat$dday30 + 45)*dday30_dat$corn_acres - dday30_corn_fn(dday30_dat$dday30)*dday30_dat$corn_acres), na.rm = TRUE) +
  sum((dday30_cotton_fn(dday30_dat$dday30 + 45)*dday30_dat$cotton_acres - dday30_cotton_fn(dday30_dat$dday30)*dday30_dat$cotton_acres), na.rm = TRUE) +
  sum((dday30_hay_fn(dday30_dat$dday30 + 45)*dday30_dat$hay_acres - dday30_hay_fn(dday30_dat$dday30)*dday30_dat$hay_acres), na.rm = TRUE) +
  sum((dday30_wheat_fn(dday30_dat$dday30 + 45)*dday30_dat$wheat_acres - dday30_wheat_fn(dday30_dat$dday30)*dday30_dat$wheat_acres), na.rm = TRUE) +
  sum((dday30_soybean_fn(dday30_dat$dday30 + 45)*dday30_dat$soybean_acres - dday30_soybean_fn(dday30_dat$dday30)*dday30_dat$soybean_acres), na.rm = TRUE)
  
dday30_temp4 <- dday30_base + sum((dday30_corn_fn(dday30_dat$dday30 + 60)*dday30_dat$corn_acres - dday30_corn_fn(dday30_dat$dday30)*dday30_dat$corn_acres), na.rm = TRUE) +
  sum((dday30_cotton_fn(dday30_dat$dday30 + 60)*dday30_dat$cotton_acres - dday30_cotton_fn(dday30_dat$dday30)*dday30_dat$cotton_acres), na.rm = TRUE) +
  sum((dday30_hay_fn(dday30_dat$dday30 + 60)*dday30_dat$hay_acres - dday30_hay_fn(dday30_dat$dday30)*dday30_dat$hay_acres), na.rm = TRUE) +
  sum((dday30_wheat_fn(dday30_dat$dday30 + 60)*dday30_dat$wheat_acres - dday30_wheat_fn(dday30_dat$dday30)*dday30_dat$wheat_acres), na.rm = TRUE) +
  sum((dday30_soybean_fn(dday30_dat$dday30 + 60)*dday30_dat$soybean_acres - dday30_soybean_fn(dday30_dat$dday30)*dday30_dat$soybean_acres), na.rm = TRUE)
  
dday30_temp5 <- dday30_base + sum((dday30_corn_fn(dday30_dat$dday30 + 75)*dday30_dat$corn_acres - dday30_corn_fn(dday30_dat$dday30)*dday30_dat$corn_acres), na.rm = TRUE) +
  sum((dday30_cotton_fn(dday30_dat$dday30 + 75)*dday30_dat$cotton_acres - dday30_cotton_fn(dday30_dat$dday30)*dday30_dat$cotton_acres), na.rm = TRUE) +
  sum((dday30_hay_fn(dday30_dat$dday30 + 75)*dday30_dat$hay_acres - dday30_hay_fn(dday30_dat$dday30)*dday30_dat$hay_acres), na.rm = TRUE) +
  sum((dday30_wheat_fn(dday30_dat$dday30 + 75)*dday30_dat$wheat_acres - dday30_wheat_fn(dday30_dat$dday30)*dday30_dat$wheat_acres), na.rm = TRUE) +
  sum((dday30_soybean_fn(dday30_dat$dday30 + 75)*dday30_dat$soybean_acres - dday30_soybean_fn(dday30_dat$dday30)*dday30_dat$soybean_acres), na.rm = TRUE)
  
# Build data.frame of results
adapt1_dday30 <- data.frame(temp = "Degree Day (30C)",
                          model = "Total Effect",
                     rev = c(dday30_base, dday30_temp1, dday30_temp2, dday30_temp3, dday30_temp4, dday30_temp5))
adapt1_dday30$change = (adapt1_dday30$rev - first(adapt1_dday30$rev))/first(adapt1_dday30$rev)*100

# Checks
adapt1_dday30
plot(x=0:5, adapt1_dday30$change)
lines(x=0:5, adapt1_dday30$change)

#-------------------------------------
# Adaptation w/o Crop-Switching

# Average Temperatures
tavg <- tavg_dat$tavg
tavg[1] <- tavg_dat$tavg[1] + 0.1
tavg[length(tavg_dat$tavg)] <- tavg[length(tavg_dat$tavg)] - 0.1

tavg_base <- sum(tavg_dat$corn_rev*tavg_dat$corn_acres) +
        sum(tavg_dat$cotton_rev*tavg_dat$cotton_acres) +
        sum(tavg_dat$hay_rev*tavg_dat$hay_acres) +
        sum(tavg_dat$wheat_rev*tavg_dat$wheat_acres) +
        sum(tavg_dat$soybean_rev*tavg_dat$soybean_acres)
  
tavg_temp1 <- tavg_base + (sum(grad(tavg_corn_fn, tavg)*tavg_dat$corn_acres) + 
          sum(grad(tavg_cotton_fn, tavg)*1*tavg_dat$cotton_acres) +
          sum(grad(tavg_hay_fn, tavg)*1*tavg_dat$hay_acres) +
          sum(grad(tavg_wheat_fn, tavg)*1*tavg_dat$wheat_acres) +
          sum(grad(tavg_soybean_fn, tavg)*1*tavg_dat$soybean_acres))

tavg_temp2 <- tavg_base + (sum((2*grad(tavg_corn_fn, tavg))*tavg_dat$corn_acres) + 
          sum((2*grad(tavg_cotton_fn, tavg))*tavg_dat$cotton_acres) +
          sum((2*grad(tavg_hay_fn, tavg))*tavg_dat$hay_acres) +
          sum((2*grad(tavg_wheat_fn, tavg))*tavg_dat$wheat_acres) +
          sum((2*grad(tavg_soybean_fn, tavg))*tavg_dat$soybean_acres))

tavg_temp3 <- tavg_base + (sum((3*grad(tavg_corn_fn, tavg))*tavg_dat$corn_acres) + 
          sum((3*grad(tavg_cotton_fn, tavg))*tavg_dat$cotton_acres) +
          sum((3*grad(tavg_hay_fn, tavg))*tavg_dat$hay_acres) +
          sum((3*grad(tavg_wheat_fn, tavg))*tavg_dat$wheat_acres) +
          sum((3*grad(tavg_soybean_fn, tavg))*tavg_dat$soybean_acres))

tavg_temp4 <- tavg_base + (sum((4*grad(tavg_corn_fn, tavg))*tavg_dat$corn_acres) + 
          sum((4*grad(tavg_cotton_fn, tavg))*tavg_dat$cotton_acres) +
          sum((4*grad(tavg_hay_fn, tavg))*tavg_dat$hay_acres) +
          sum((4*grad(tavg_wheat_fn, tavg))*tavg_dat$wheat_acres) +
          sum((4*grad(tavg_soybean_fn, tavg))*tavg_dat$soybean_acres))

tavg_temp5 <- tavg_base + (sum((5*grad(tavg_corn_fn, tavg))*tavg_dat$corn_acres) + 
          sum((5*grad(tavg_cotton_fn, tavg))*tavg_dat$cotton_acres) +
          sum((5*grad(tavg_hay_fn, tavg))*tavg_dat$hay_acres) +
          sum((5*grad(tavg_wheat_fn, tavg))*tavg_dat$wheat_acres) +
          sum((5*grad(tavg_soybean_fn, tavg))*tavg_dat$soybean_acres))

adapt2_tavg <- data.frame(temp = "Average Temperature (C)",
                          model = "Adaptation w/o Crop-switching",
                     rev = c(tavg_base, tavg_temp1, tavg_temp2, tavg_temp3, tavg_temp4, tavg_temp5))
adapt2_tavg$change = (adapt2_tavg$rev - first(adapt2_tavg$rev))/first(adapt2_tavg$rev)*100
adapt2_tavg
plot(x=0:5, adapt2_tavg$change)
lines(x=0:5, adapt2_tavg$change)


# Degree Days 10-30
dday10_30 <- dday10_30_dat$dday10_30
dday10_30[1] <- dday10_30_dat$dday10_30[1] + 6
dday10_30[length(dday10_30_dat$dday10_30)] <- dday10_30[length(dday10_30_dat$dday10_30)] - 70

dday10_30_base <- sum(dday10_30_dat$corn_rev*dday10_30_dat$corn_acres) +
        sum(dday10_30_dat$cotton_rev*dday10_30_dat$cotton_acres) +
        sum(dday10_30_dat$hay_rev*dday10_30_dat$hay_acres) +
        sum(dday10_30_dat$wheat_rev*dday10_30_dat$wheat_acres) +
        sum(dday10_30_dat$soybean_rev*dday10_30_dat$soybean_acres)
  
dday10_30_temp1 <- dday10_30_base + (sum(140*grad(dday10_30_corn_fn, dday10_30)*dday10_30_dat$corn_acres) + 
          sum(140*grad(dday10_30_cotton_fn, dday10_30)*dday10_30_dat$cotton_acres) +
          sum(140*grad(dday10_30_hay_fn, dday10_30)*dday10_30_dat$hay_acres) +
          sum(140*grad(dday10_30_wheat_fn, dday10_30)*dday10_30_dat$wheat_acres) +
          sum(140*grad(dday10_30_soybean_fn, dday10_30)*dday10_30_dat$soybean_acres))

dday10_30_temp2 <- dday10_30_base + (sum((280*grad(dday10_30_corn_fn, dday10_30))*dday10_30_dat$corn_acres) + 
          sum((280*grad(dday10_30_cotton_fn, dday10_30))*dday10_30_dat$cotton_acres) +
          sum((280*grad(dday10_30_hay_fn, dday10_30))*dday10_30_dat$hay_acres) +
          sum((280*grad(dday10_30_wheat_fn, dday10_30))*dday10_30_dat$wheat_acres) +
          sum((280*grad(dday10_30_soybean_fn, dday10_30))*dday10_30_dat$soybean_acres))

dday10_30_temp3 <- dday10_30_base + (sum((420*grad(dday10_30_corn_fn, dday10_30))*dday10_30_dat$corn_acres) + 
          sum((420*grad(dday10_30_cotton_fn, dday10_30))*dday10_30_dat$cotton_acres) +
          sum((420*grad(dday10_30_hay_fn, dday10_30))*dday10_30_dat$hay_acres) +
          sum((420*grad(dday10_30_wheat_fn, dday10_30))*dday10_30_dat$wheat_acres) +
          sum((420*grad(dday10_30_soybean_fn, dday10_30))*dday10_30_dat$soybean_acres))

dday10_30_temp4 <- dday10_30_base + (sum((560*grad(dday10_30_corn_fn, dday10_30))*dday10_30_dat$corn_acres) + 
          sum((560*grad(dday10_30_cotton_fn, dday10_30))*dday10_30_dat$cotton_acres) +
          sum((560*grad(dday10_30_hay_fn, dday10_30))*dday10_30_dat$hay_acres) +
          sum((560*grad(dday10_30_wheat_fn, dday10_30))*dday10_30_dat$wheat_acres) +
          sum((560*grad(dday10_30_soybean_fn, dday10_30))*dday10_30_dat$soybean_acres))

dday10_30_temp5 <- dday10_30_base + (sum((700*grad(dday10_30_corn_fn, dday10_30))*dday10_30_dat$corn_acres) + 
          sum((700*grad(dday10_30_cotton_fn, dday10_30))*dday10_30_dat$cotton_acres) +
          sum((700*grad(dday10_30_hay_fn, dday10_30))*dday10_30_dat$hay_acres) +
          sum((700*grad(dday10_30_wheat_fn, dday10_30))*dday10_30_dat$wheat_acres) +
          sum((700*grad(dday10_30_soybean_fn, dday10_30))*dday10_30_dat$soybean_acres))

adapt2_dday10_30 <- data.frame(temp = "Degree Day (10-30C)",
                          model = "Adaptation w/o Crop-switching",
                     rev = c(dday10_30_base, dday10_30_temp1, dday10_30_temp2, dday10_30_temp3, dday10_30_temp4, dday10_30_temp5))
adapt2_dday10_30$change = 100*((adapt2_dday10_30$rev - first(adapt2_dday10_30$rev))/first(adapt2_dday10_30$rev))
adapt2_dday10_30
plot(x=0:5, adapt2_dday10_30$change)
lines(x=0:5, adapt2_dday10_30$change)

# Degree Days 30
dday30 <- dday30_dat$dday30
dday30[1] <- dday30_dat$dday30[1] + 0.1
dday30[length(dday30_dat$dday30)] <- dday30[length(dday30_dat$dday30)] - 0.1

dday30_base <- sum(dday30_dat$corn_rev*dday30_dat$corn_acres) +
        sum(dday30_dat$cotton_rev*dday30_dat$cotton_acres) +
        sum(dday30_dat$hay_rev*dday30_dat$hay_acres) +
        sum(dday30_dat$wheat_rev*dday30_dat$wheat_acres) +
        sum(dday30_dat$soybean_rev*dday30_dat$soybean_acres)
  
dday30_temp1 <- dday30_base + (sum(15*grad(dday30_corn_fn, dday30)*dday30_dat$corn_acres) + 
          sum(15*grad(dday30_cotton_fn, dday30)*dday30_dat$cotton_acres) +
          sum(15*grad(dday30_hay_fn, dday30)*dday30_dat$hay_acres) +
          sum(15*grad(dday30_wheat_fn, dday30)*dday30_dat$wheat_acres) +
          sum(15*grad(dday30_soybean_fn, dday30)*dday30_dat$soybean_acres))

dday30_temp2 <- dday30_base + (sum((30*grad(dday30_corn_fn, dday30))*dday30_dat$corn_acres) + 
          sum((30*grad(dday30_cotton_fn, dday30))*dday30_dat$cotton_acres) +
          sum((30*grad(dday30_hay_fn, dday30))*dday30_dat$hay_acres) +
          sum((30*grad(dday30_wheat_fn, dday30))*dday30_dat$wheat_acres) +
          sum((30*grad(dday30_soybean_fn, dday30))*dday30_dat$soybean_acres))

dday30_temp3 <- dday30_base + (sum((45*grad(dday30_corn_fn, dday30))*dday30_dat$corn_acres) + 
          sum((45*grad(dday30_cotton_fn, dday30))*dday30_dat$cotton_acres) +
          sum((45*grad(dday30_hay_fn, dday30))*dday30_dat$hay_acres) +
          sum((45*grad(dday30_wheat_fn, dday30))*dday30_dat$wheat_acres) +
          sum((45*grad(dday30_soybean_fn, dday30))*dday30_dat$soybean_acres))

dday30_temp4 <- dday30_base + (sum((60*grad(dday30_corn_fn, dday30))*dday30_dat$corn_acres) + 
          sum((60*grad(dday30_cotton_fn, dday30))*dday30_dat$cotton_acres) +
          sum((60*grad(dday30_hay_fn, dday30))*dday30_dat$hay_acres) +
          sum((60*grad(dday30_wheat_fn, dday30))*dday30_dat$wheat_acres) +
          sum((60*grad(dday30_soybean_fn, dday30))*dday30_dat$soybean_acres))

dday30_temp5 <- dday30_base + (sum((75*grad(dday30_corn_fn, dday30))*dday30_dat$corn_acres) + 
          sum((75*grad(dday30_cotton_fn, dday30))*dday30_dat$cotton_acres) +
          sum((75*grad(dday30_hay_fn, dday30))*dday30_dat$hay_acres) +
          sum((75*grad(dday30_wheat_fn, dday30))*dday30_dat$wheat_acres) +
          sum((75*grad(dday30_soybean_fn, dday30))*dday30_dat$soybean_acres))

adapt2_dday30 <- data.frame(temp = "Degree Day (30C)",
                          model = "Adaptation w/o Crop-switching",
                     rev = c(dday30_base, dday30_temp1, dday30_temp2, dday30_temp3, dday30_temp4, dday30_temp5))
adapt2_dday30$change = 100*(adapt2_dday30$rev - first(adapt2_dday30$rev))/first(adapt2_dday30$rev)
adapt2_dday30
plot(x=0:5, adapt2_dday30$change)
lines(x=0:5, adapt2_dday30$change)


# Adaptation with Crop-switching
#-------------------------------------

# Average Temperatures
tavg <- tavg_dat$tavg
tavg[1] <- tavg_dat$tavg[1] + 0.1
tavg[length(tavg_dat$tavg)] <- tavg[length(tavg_dat$tavg)] - 0.1

tavg_base <- sum(tavg_dat$corn_rev*tavg_dat$corn_acres) +
        sum(tavg_dat$cotton_rev*tavg_dat$cotton_acres) +
        sum(tavg_dat$hay_rev*tavg_dat$hay_acres) +
        sum(tavg_dat$wheat_rev*tavg_dat$wheat_acres) +
        sum(tavg_dat$soybean_rev*tavg_dat$soybean_acres)
  
tavg_temp1 <- tavg_base + (sum(grad(tavg_corn_fn, tavg)*(grad(tavg_p_corn_fn, tavg)*tavg_dat$corn_acres + tavg_dat$corn_acres))) + 
          (sum(grad(tavg_cotton_fn, tavg)*(grad(tavg_p_cotton_fn, tavg)*tavg_dat$cotton_acres + tavg_dat$cotton_acres))) + 
  (sum(grad(tavg_hay_fn, tavg)*(grad(tavg_p_hay_fn, tavg)*tavg_dat$hay_acres + tavg_dat$hay_acres))) + 
  (sum(grad(tavg_wheat_fn, tavg)*(grad(tavg_p_wheat_fn, tavg)*tavg_dat$wheat_acres + tavg_dat$wheat_acres))) + 
  (sum(grad(tavg_soybean_fn, tavg)*(grad(tavg_p_soybean_fn, tavg)*tavg_dat$soybean_acres + tavg_dat$soybean_acres))) 

tavg_temp2 <- tavg_base + (sum(2*grad(tavg_corn_fn, tavg)*(2*grad(tavg_p_corn_fn, tavg)*tavg_dat$corn_acres + tavg_dat$corn_acres))) + 
          (sum(2*grad(tavg_cotton_fn, tavg)*(2*grad(tavg_p_cotton_fn, tavg)*tavg_dat$cotton_acres + tavg_dat$cotton_acres))) + 
  (sum(2*grad(tavg_hay_fn, tavg)*(2*grad(tavg_p_hay_fn, tavg)*tavg_dat$hay_acres + tavg_dat$hay_acres))) + 
  (sum(2*grad(tavg_wheat_fn, tavg)*(2*grad(tavg_p_wheat_fn, tavg)*tavg_dat$wheat_acres + tavg_dat$wheat_acres))) + 
  (sum(2*grad(tavg_soybean_fn, tavg)*(2*grad(tavg_p_soybean_fn, tavg)*tavg_dat$soybean_acres + tavg_dat$soybean_acres))) 

tavg_temp3 <- tavg_base + (sum(3*grad(tavg_corn_fn, tavg)*(3*grad(tavg_p_corn_fn, tavg)*tavg_dat$corn_acres + tavg_dat$corn_acres))) + 
          (sum(3*grad(tavg_cotton_fn, tavg)*(3*grad(tavg_p_cotton_fn, tavg)*tavg_dat$cotton_acres + tavg_dat$cotton_acres))) + 
  (sum(3*grad(tavg_hay_fn, tavg)*(3*grad(tavg_p_hay_fn, tavg)*tavg_dat$hay_acres + tavg_dat$hay_acres))) + 
  (sum(3*grad(tavg_wheat_fn, tavg)*(3*grad(tavg_p_wheat_fn, tavg)*tavg_dat$wheat_acres + tavg_dat$wheat_acres))) + 
  (sum(3*grad(tavg_soybean_fn, tavg)*(3*grad(tavg_p_soybean_fn, tavg)*tavg_dat$soybean_acres + tavg_dat$soybean_acres))) 

tavg_temp4 <- tavg_base + (sum(4*grad(tavg_corn_fn, tavg)*(4*grad(tavg_p_corn_fn, tavg)*tavg_dat$corn_acres + tavg_dat$corn_acres))) + 
          (sum(4*grad(tavg_cotton_fn, tavg)*(grad(tavg_p_cotton_fn, tavg)*tavg_dat$cotton_acres + tavg_dat$cotton_acres))) + 
  (sum(4*grad(tavg_hay_fn, tavg)*(4*grad(tavg_p_hay_fn, tavg)*tavg_dat$hay_acres + tavg_dat$hay_acres))) + 
  (sum(4*grad(tavg_wheat_fn, tavg)*(4*grad(tavg_p_wheat_fn, tavg)*tavg_dat$wheat_acres + tavg_dat$wheat_acres))) + 
  (sum(4*grad(tavg_soybean_fn, tavg)*(4*grad(tavg_p_soybean_fn, tavg)*tavg_dat$soybean_acres + tavg_dat$soybean_acres))) 

tavg_temp5 <- tavg_base + (sum(5*grad(tavg_corn_fn, tavg)*(5*grad(tavg_p_corn_fn, tavg)*tavg_dat$corn_acres + tavg_dat$corn_acres))) + 
          (sum(5*grad(tavg_cotton_fn, tavg)*(5*grad(tavg_p_cotton_fn, tavg)*tavg_dat$cotton_acres + tavg_dat$cotton_acres))) + 
  (sum(5*grad(tavg_hay_fn, tavg)*(5*grad(tavg_p_hay_fn, tavg)*tavg_dat$hay_acres + tavg_dat$hay_acres))) + 
  (sum(5*grad(tavg_wheat_fn, tavg)*(5*grad(tavg_p_wheat_fn, tavg)*tavg_dat$wheat_acres + tavg_dat$wheat_acres))) + 
  (sum(5*grad(tavg_soybean_fn, tavg)*(5*grad(tavg_p_soybean_fn, tavg)*tavg_dat$soybean_acres + tavg_dat$soybean_acres))) 


adapt3_tavg <- data.frame(temp = "Average Temperature (C)",
                          model = "Adaptation w/ Crop-switching",
                     rev = c(tavg_base, tavg_temp1, tavg_temp2, tavg_temp3, tavg_temp4, tavg_temp5))
adapt3_tavg$change = (adapt3_tavg$rev - first(adapt3_tavg$rev))/first(adapt3_tavg$rev)*100
adapt3_tavg
plot(x=0:5, adapt3_tavg$change)
lines(x=0:5, adapt3_tavg$change)



# Degree Days 10-30C
dday10_30 <- dday10_30_dat$dday10_30
dday10_30[1] <- dday10_30_dat$dday10_30[1] + 6
dday10_30[length(dday10_30_dat$dday10_30)] <- dday10_30[length(dday10_30_dat$dday10_30)] - 6

dday10_30_base <- sum(dday10_30_dat$corn_rev*dday10_30_dat$corn_acres) +
        sum(dday10_30_dat$cotton_rev*dday10_30_dat$cotton_acres) +
        sum(dday10_30_dat$hay_rev*dday10_30_dat$hay_acres) +
        sum(dday10_30_dat$wheat_rev*dday10_30_dat$wheat_acres) +
        sum(dday10_30_dat$soybean_rev*dday10_30_dat$soybean_acres)
  
dday10_30_temp1 <- dday10_30_base + (sum(140*grad(dday10_30_corn_fn, dday10_30)*(140*grad(dday10_30_p_corn_fn, dday10_30)*dday10_30_dat$corn_acres + dday10_30_dat$corn_acres))) + 
          (sum(140*grad(dday10_30_cotton_fn, dday10_30)*(140*grad(dday10_30_p_cotton_fn, dday10_30)*dday10_30_dat$cotton_acres + dday10_30_dat$cotton_acres))) + 
  (sum(140*grad(dday10_30_hay_fn, dday10_30)*(140*grad(dday10_30_p_hay_fn, dday10_30)*dday10_30_dat$hay_acres + dday10_30_dat$hay_acres))) + 
  (sum(140*grad(dday10_30_wheat_fn, dday10_30)*(140*grad(dday10_30_p_wheat_fn, dday10_30)*dday10_30_dat$wheat_acres + dday10_30_dat$wheat_acres))) + 
  (sum(140*grad(dday10_30_soybean_fn, dday10_30)*(140*grad(dday10_30_p_soybean_fn, dday10_30)*dday10_30_dat$soybean_acres + dday10_30_dat$soybean_acres))) 

dday10_30_temp2 <- dday10_30_base + (sum(280*grad(dday10_30_corn_fn, dday10_30)*(280*grad(dday10_30_p_corn_fn, dday10_30)*dday10_30_dat$corn_acres + dday10_30_dat$corn_acres))) + 
          (sum(280*grad(dday10_30_cotton_fn, dday10_30)*(280*grad(dday10_30_p_cotton_fn, dday10_30)*dday10_30_dat$cotton_acres + dday10_30_dat$cotton_acres))) + 
  (sum(280*grad(dday10_30_hay_fn, dday10_30)*(280*grad(dday10_30_p_hay_fn, dday10_30)*dday10_30_dat$hay_acres + dday10_30_dat$hay_acres))) + 
  (sum(280*grad(dday10_30_wheat_fn, dday10_30)*(280*grad(dday10_30_p_wheat_fn, dday10_30)*dday10_30_dat$wheat_acres + dday10_30_dat$wheat_acres))) + 
  (sum(280*grad(dday10_30_soybean_fn, dday10_30)*(280*grad(dday10_30_p_soybean_fn, dday10_30)*dday10_30_dat$soybean_acres + dday10_30_dat$soybean_acres))) 

dday10_30_temp3 <- dday10_30_base + (sum(420*grad(dday10_30_corn_fn, dday10_30)*(420*grad(dday10_30_p_corn_fn, dday10_30)*dday10_30_dat$corn_acres + dday10_30_dat$corn_acres))) + 
          (sum(420*grad(dday10_30_cotton_fn, dday10_30)*(420*grad(dday10_30_p_cotton_fn, dday10_30)*dday10_30_dat$cotton_acres + dday10_30_dat$cotton_acres))) + 
  (sum(420*grad(dday10_30_hay_fn, dday10_30)*(420*grad(dday10_30_p_hay_fn, dday10_30)*dday10_30_dat$hay_acres + dday10_30_dat$hay_acres))) + 
  (sum(420*grad(dday10_30_wheat_fn, dday10_30)*(420*grad(dday10_30_p_wheat_fn, dday10_30)*dday10_30_dat$wheat_acres + dday10_30_dat$wheat_acres))) + 
  (sum(420*grad(dday10_30_soybean_fn, dday10_30)*(420*grad(dday10_30_p_soybean_fn, dday10_30)*dday10_30_dat$soybean_acres + dday10_30_dat$soybean_acres))) 

dday10_30_temp4 <- dday10_30_base + (sum(560*grad(dday10_30_corn_fn, dday10_30)*(560*grad(dday10_30_p_corn_fn, dday10_30)*dday10_30_dat$corn_acres + dday10_30_dat$corn_acres))) + 
          (sum(560*grad(dday10_30_cotton_fn, dday10_30)*(grad(dday10_30_p_cotton_fn, dday10_30)*dday10_30_dat$cotton_acres + dday10_30_dat$cotton_acres))) + 
  (sum(560*grad(dday10_30_hay_fn, dday10_30)*(560*grad(dday10_30_p_hay_fn, dday10_30)*dday10_30_dat$hay_acres + dday10_30_dat$hay_acres))) + 
  (sum(560*grad(dday10_30_wheat_fn, dday10_30)*(560*grad(dday10_30_p_wheat_fn, dday10_30)*dday10_30_dat$wheat_acres + dday10_30_dat$wheat_acres))) + 
  (sum(560*grad(dday10_30_soybean_fn, dday10_30)*(560*grad(dday10_30_p_soybean_fn, dday10_30)*dday10_30_dat$soybean_acres + dday10_30_dat$soybean_acres))) 

dday10_30_temp5 <- dday10_30_base + (sum(700*grad(dday10_30_corn_fn, dday10_30)*(700*grad(dday10_30_p_corn_fn, dday10_30)*dday10_30_dat$corn_acres + dday10_30_dat$corn_acres))) + 
          (sum(700*grad(dday10_30_cotton_fn, dday10_30)*(700*grad(dday10_30_p_cotton_fn, dday10_30)*dday10_30_dat$cotton_acres + dday10_30_dat$cotton_acres))) + 
  (sum(700*grad(dday10_30_hay_fn, dday10_30)*(700*grad(dday10_30_p_hay_fn, dday10_30)*dday10_30_dat$hay_acres + dday10_30_dat$hay_acres))) + 
  (sum(700*grad(dday10_30_wheat_fn, dday10_30)*(700*grad(dday10_30_p_wheat_fn, dday10_30)*dday10_30_dat$wheat_acres + dday10_30_dat$wheat_acres))) + 
  (sum(700*grad(dday10_30_soybean_fn, dday10_30)*(700*grad(dday10_30_p_soybean_fn, dday10_30)*dday10_30_dat$soybean_acres + dday10_30_dat$soybean_acres))) 


adapt3_dday10_30 <- data.frame(temp = "Degree Day (10-30C)",
                          model = "Adaptation w/ Crop-switching",
                     rev = c(dday10_30_base, dday10_30_temp1, dday10_30_temp2, dday10_30_temp3, dday10_30_temp4, dday10_30_temp5))
adapt3_dday10_30$change = (adapt3_dday10_30$rev - first(adapt3_dday10_30$rev))/first(adapt3_dday10_30$rev)*100
adapt3_dday10_30
plot(x=0:5, adapt3_dday10_30$change)
lines(x=0:5, adapt3_dday10_30$change)

# Degree Days 10-30C
dday30 <- dday30_dat$dday30
dday30[1] <- dday30_dat$dday30[1] + 0.1
dday30[length(dday30_dat$dday30)] <- dday30[length(dday30_dat$dday30)] - 0.1

dday30_base <- sum(dday30_dat$corn_rev*dday30_dat$corn_acres) +
        sum(dday30_dat$cotton_rev*dday30_dat$cotton_acres) +
        sum(dday30_dat$hay_rev*dday30_dat$hay_acres) +
        sum(dday30_dat$wheat_rev*dday30_dat$wheat_acres) +
        sum(dday30_dat$soybean_rev*dday30_dat$soybean_acres)
  
dday30_temp1 <- dday30_base + (sum(15*grad(dday30_corn_fn, dday30)*(15*grad(dday30_p_corn_fn, dday30)*dday30_dat$corn_acres + dday30_dat$corn_acres))) + 
          (sum(15*grad(dday30_cotton_fn, dday30)*(15*grad(dday30_p_cotton_fn, dday30)*dday30_dat$cotton_acres + dday30_dat$cotton_acres))) + 
  (sum(15*grad(dday30_hay_fn, dday30)*(15*grad(dday30_p_hay_fn, dday30)*dday30_dat$hay_acres + dday30_dat$hay_acres))) + 
  (sum(15*grad(dday30_wheat_fn, dday30)*(15*grad(dday30_p_wheat_fn, dday30)*dday30_dat$wheat_acres + dday30_dat$wheat_acres))) + 
  (sum(15*grad(dday30_soybean_fn, dday30)*(15*grad(dday30_p_soybean_fn, dday30)*dday30_dat$soybean_acres + dday30_dat$soybean_acres))) 

dday30_temp2 <- dday30_base + (sum(30*grad(dday30_corn_fn, dday30)*(30*grad(dday30_p_corn_fn, dday30)*dday30_dat$corn_acres + dday30_dat$corn_acres))) + 
          (sum(30*grad(dday30_cotton_fn, dday30)*(30*grad(dday30_p_cotton_fn, dday30)*dday30_dat$cotton_acres + dday30_dat$cotton_acres))) + 
  (sum(30*grad(dday30_hay_fn, dday30)*(30*grad(dday30_p_hay_fn, dday30)*dday30_dat$hay_acres + dday30_dat$hay_acres))) + 
  (sum(30*grad(dday30_wheat_fn, dday30)*(30*grad(dday30_p_wheat_fn, dday30)*dday30_dat$wheat_acres + dday30_dat$wheat_acres))) + 
  (sum(30*grad(dday30_soybean_fn, dday30)*(30*grad(dday30_p_soybean_fn, dday30)*dday30_dat$soybean_acres + dday30_dat$soybean_acres))) 

dday30_temp3 <- dday30_base + (sum(45*grad(dday30_corn_fn, dday30)*(45*grad(dday30_p_corn_fn, dday30)*dday30_dat$corn_acres + dday30_dat$corn_acres))) + 
          (sum(45*grad(dday30_cotton_fn, dday30)*(45*grad(dday30_p_cotton_fn, dday30)*dday30_dat$cotton_acres + dday30_dat$cotton_acres))) + 
  (sum(45*grad(dday30_hay_fn, dday30)*(45*grad(dday30_p_hay_fn, dday30)*dday30_dat$hay_acres + dday30_dat$hay_acres))) + 
  (sum(45*grad(dday30_wheat_fn, dday30)*(45*grad(dday30_p_wheat_fn, dday30)*dday30_dat$wheat_acres + dday30_dat$wheat_acres))) + 
  (sum(45*grad(dday30_soybean_fn, dday30)*(45*grad(dday30_p_soybean_fn, dday30)*dday30_dat$soybean_acres + dday30_dat$soybean_acres))) 

dday30_temp4 <- dday30_base + (sum(60*grad(dday30_corn_fn, dday30)*(60*grad(dday30_p_corn_fn, dday30)*dday30_dat$corn_acres + dday30_dat$corn_acres))) + 
          (sum(60*grad(dday30_cotton_fn, dday30)*(grad(dday30_p_cotton_fn, dday30)*dday30_dat$cotton_acres + dday30_dat$cotton_acres))) + 
  (sum(60*grad(dday30_hay_fn, dday30)*(60*grad(dday30_p_hay_fn, dday30)*dday30_dat$hay_acres + dday30_dat$hay_acres))) + 
  (sum(60*grad(dday30_wheat_fn, dday30)*(60*grad(dday30_p_wheat_fn, dday30)*dday30_dat$wheat_acres + dday30_dat$wheat_acres))) + 
  (sum(60*grad(dday30_soybean_fn, dday30)*(60*grad(dday30_p_soybean_fn, dday30)*dday30_dat$soybean_acres + dday30_dat$soybean_acres))) 

dday30_temp5 <- dday30_base + (sum(75*grad(dday30_corn_fn, dday30)*(75*grad(dday30_p_corn_fn, dday30)*dday30_dat$corn_acres + dday30_dat$corn_acres))) + 
          (sum(75*grad(dday30_cotton_fn, dday30)*(75*grad(dday30_p_cotton_fn, dday30)*dday30_dat$cotton_acres + dday30_dat$cotton_acres))) + 
  (sum(75*grad(dday30_hay_fn, dday30)*(75*grad(dday30_p_hay_fn, dday30)*dday30_dat$hay_acres + dday30_dat$hay_acres))) + 
  (sum(75*grad(dday30_wheat_fn, dday30)*(75*grad(dday30_p_wheat_fn, dday30)*dday30_dat$wheat_acres + dday30_dat$wheat_acres))) + 
  (sum(75*grad(dday30_soybean_fn, dday30)*(75*grad(dday30_p_soybean_fn, dday30)*dday30_dat$soybean_acres + dday30_dat$soybean_acres))) 


adapt3_dday30 <- data.frame(temp = "Degree Day (30C)",
                          model = "Adaptation w/ Crop-switching",
                     rev = c(dday30_base, dday30_temp1, dday30_temp2, dday30_temp3, dday30_temp4, dday30_temp5))
adapt3_dday30$change = (adapt3_dday30$rev - first(adapt3_dday30$rev))/first(adapt3_dday30$rev)*100
adapt3_dday30
plot(x=0:5, adapt3_dday30$change)
lines(x=0:5, adapt3_dday30$change)

pdat <- rbind(adapt1_tavg, adapt1_dday10_30, adapt1_dday30, 
              adapt2_tavg, adapt2_dday10_30, adapt2_dday30,
              adapt3_tavg, adapt3_dday10_30, adapt3_dday30)
pdat$c <- seq(0,5)


# Adaptation with Crop-switching
#-------------------------------------

# Average Temperatures
tavg <- tavg_dat$tavg
tavg[1] <- tavg_dat$tavg[1] + 0.1
tavg[length(tavg_dat$tavg)] <- tavg[length(tavg_dat$tavg)] - 0.1

tavg_base <- sum(tavg_dat$corn_rev*tavg_dat$corn_acres) +
        sum(tavg_dat$cotton_rev*tavg_dat$cotton_acres) +
        sum(tavg_dat$hay_rev*tavg_dat$hay_acres) +
        sum(tavg_dat$wheat_rev*tavg_dat$wheat_acres) +
        sum(tavg_dat$soybean_rev*tavg_dat$soybean_acres)
  
tavg_temp1 <- tavg_base + sum(((tavg_corn_fn(tavg + 1)*(tavg_p_corn_fn(tavg + 1)*tavg_dat$corn_acres))) - ((tavg_corn_fn(tavg)*(tavg_p_corn_fn(tavg)*tavg_dat$corn_acres))), na.rm = TRUE) +
                          sum(((tavg_cotton_fn(tavg + 1)*(tavg_p_cotton_fn(tavg + 1)*tavg_dat$cotton_acres))) - ((tavg_cotton_fn(tavg)*(tavg_p_cotton_fn(tavg)*tavg_dat$cotton_acres))), na.rm = TRUE) +
                          sum(((tavg_hay_fn(tavg + 1)*(tavg_p_hay_fn(tavg + 1)*tavg_dat$hay_acres))) - ((tavg_hay_fn(tavg)*(tavg_p_hay_fn(tavg)*tavg_dat$hay_acres))), na.rm = TRUE) +
                          sum(((tavg_wheat_fn(tavg + 1)*(tavg_p_wheat_fn(tavg + 1)*tavg_dat$wheat_acres))) - ((tavg_wheat_fn(tavg)*(tavg_p_wheat_fn(tavg)*tavg_dat$wheat_acres))), na.rm = TRUE) +
                          sum(((tavg_soybean_fn(tavg + 1)*(tavg_p_soybean_fn(tavg + 1)*tavg_dat$soybean_acres))) - ((tavg_soybean_fn(tavg)*(tavg_p_soybean_fn(tavg)*tavg_dat$soybean_acres))), na.rm = TRUE) 

tavg_temp2 <- tavg_base + sum(((tavg_corn_fn(tavg + 2)*(tavg_p_corn_fn(tavg + 2)*tavg_dat$corn_acres))) - ((tavg_corn_fn(tavg)*(tavg_p_corn_fn(tavg)*tavg_dat$corn_acres))), na.rm = TRUE) +
                          sum(((tavg_cotton_fn(tavg + 2)*(tavg_p_cotton_fn(tavg + 2)*tavg_dat$cotton_acres))) - ((tavg_cotton_fn(tavg)*(tavg_p_cotton_fn(tavg)*tavg_dat$cotton_acres))), na.rm = TRUE) +
                          sum(((tavg_hay_fn(tavg + 2)*(tavg_p_hay_fn(tavg + 2)*tavg_dat$hay_acres))) - ((tavg_hay_fn(tavg)*(tavg_p_hay_fn(tavg)*tavg_dat$hay_acres))), na.rm = TRUE) +
                          sum(((tavg_wheat_fn(tavg + 2)*(tavg_p_wheat_fn(tavg + 2)*tavg_dat$wheat_acres))) - ((tavg_wheat_fn(tavg)*(tavg_p_wheat_fn(tavg)*tavg_dat$wheat_acres))), na.rm = TRUE) +
                          sum(((tavg_soybean_fn(tavg + 2)*(tavg_p_soybean_fn(tavg + 2)*tavg_dat$soybean_acres))) - ((tavg_soybean_fn(tavg)*(tavg_p_soybean_fn(tavg)*tavg_dat$soybean_acres))), na.rm = TRUE) 

tavg_temp3 <- tavg_base + sum(((tavg_corn_fn(tavg + 3)*(tavg_p_corn_fn(tavg + 3)*tavg_dat$corn_acres))) - ((tavg_corn_fn(tavg)*(tavg_p_corn_fn(tavg)*tavg_dat$corn_acres))), na.rm = TRUE) +
                          sum(((tavg_cotton_fn(tavg + 3)*(tavg_p_cotton_fn(tavg + 3)*tavg_dat$cotton_acres))) - ((tavg_cotton_fn(tavg)*(tavg_p_cotton_fn(tavg)*tavg_dat$cotton_acres))), na.rm = TRUE) +
                          sum(((tavg_hay_fn(tavg + 3)*(tavg_p_hay_fn(tavg + 3)*tavg_dat$hay_acres))) - ((tavg_hay_fn(tavg)*(tavg_p_hay_fn(tavg)*tavg_dat$hay_acres))), na.rm = TRUE) +
                          sum(((tavg_wheat_fn(tavg + 3)*(tavg_p_wheat_fn(tavg + 3)*tavg_dat$wheat_acres))) - ((tavg_wheat_fn(tavg)*(tavg_p_wheat_fn(tavg)*tavg_dat$wheat_acres))), na.rm = TRUE) +
                          sum(((tavg_soybean_fn(tavg + 3)*(tavg_p_soybean_fn(tavg + 3)*tavg_dat$soybean_acres))) - ((tavg_soybean_fn(tavg)*(tavg_p_soybean_fn(tavg)*tavg_dat$soybean_acres))), na.rm = TRUE)

                          
tavg_temp4 <- tavg_base + sum(((tavg_corn_fn(tavg + 4)*(tavg_p_corn_fn(tavg + 4)*tavg_dat$corn_acres))) - ((tavg_corn_fn(tavg)*(tavg_p_corn_fn(tavg)*tavg_dat$corn_acres))), na.rm = TRUE) +
                          sum(((tavg_cotton_fn(tavg + 4)*(tavg_p_cotton_fn(tavg + 4)*tavg_dat$cotton_acres))) - ((tavg_cotton_fn(tavg)*(tavg_p_cotton_fn(tavg)*tavg_dat$cotton_acres))), na.rm = TRUE) +
                          sum(((tavg_hay_fn(tavg + 4)*(tavg_p_hay_fn(tavg + 4)*tavg_dat$hay_acres))) - ((tavg_hay_fn(tavg)*(tavg_p_hay_fn(tavg)*tavg_dat$hay_acres))), na.rm = TRUE) +
                          sum(((tavg_wheat_fn(tavg + 4)*(tavg_p_wheat_fn(tavg + 4)*tavg_dat$wheat_acres))) - ((tavg_wheat_fn(tavg)*(tavg_p_wheat_fn(tavg)*tavg_dat$wheat_acres))), na.rm = TRUE) +
                          sum(((tavg_soybean_fn(tavg + 4)*(tavg_p_soybean_fn(tavg + 4)*tavg_dat$soybean_acres))) - ((tavg_soybean_fn(tavg)*(tavg_p_soybean_fn(tavg)*tavg_dat$soybean_acres))), na.rm = TRUE)

                          
tavg_temp5 <- tavg_base + sum(((tavg_corn_fn(tavg + 5)*(tavg_p_corn_fn(tavg + 5)*tavg_dat$corn_acres))) - ((tavg_corn_fn(tavg)*(tavg_p_corn_fn(tavg)*tavg_dat$corn_acres))), na.rm = TRUE) +
                          sum(((tavg_cotton_fn(tavg + 5)*(tavg_p_cotton_fn(tavg + 5)*tavg_dat$cotton_acres))) - ((tavg_cotton_fn(tavg)*(tavg_p_cotton_fn(tavg)*tavg_dat$cotton_acres))), na.rm = TRUE) +
                          sum(((tavg_hay_fn(tavg + 5)*(tavg_p_hay_fn(tavg + 5)*tavg_dat$hay_acres))) - ((tavg_hay_fn(tavg)*(tavg_p_hay_fn(tavg)*tavg_dat$hay_acres))), na.rm = TRUE) +
                          sum(((tavg_wheat_fn(tavg + 5)*(tavg_p_wheat_fn(tavg + 5)*tavg_dat$wheat_acres))) - ((tavg_wheat_fn(tavg)*(tavg_p_wheat_fn(tavg)*tavg_dat$wheat_acres))), na.rm = TRUE) +
                          sum(((tavg_soybean_fn(tavg + 5)*(tavg_p_soybean_fn(tavg + 5)*tavg_dat$soybean_acres))) - ((tavg_soybean_fn(tavg)*(tavg_p_soybean_fn(tavg)*tavg_dat$soybean_acres))), na.rm = TRUE)

adapt4_tavg <- data.frame(temp = "Average Temperature (C)",
                          model = "Total Effect w/ Crop-switching",
                     rev = c(tavg_base, tavg_temp1, tavg_temp2, tavg_temp3, tavg_temp4, tavg_temp5))
adapt4_tavg$change = (adapt4_tavg$rev - first(adapt4_tavg$rev))/first(adapt4_tavg$rev)*100
adapt4_tavg
plot(x=0:5, adapt4_tavg$change)
lines(x=0:5, adapt4_tavg$change)

# Degree Days 10-30C
dday10_30 <- dday10_30_dat$dday10_30
dday10_30[1] <- dday10_30_dat$dday10_30[1] + 0.1
dday10_30[length(dday10_30_dat$dday10_30)] <- dday10_30[length(dday10_30_dat$dday10_30)] - 0.1

dday10_30_base <- sum(dday10_30_dat$corn_rev*dday10_30_dat$corn_acres) +
        sum(dday10_30_dat$cotton_rev*dday10_30_dat$cotton_acres) +
        sum(dday10_30_dat$hay_rev*dday10_30_dat$hay_acres) +
        sum(dday10_30_dat$wheat_rev*dday10_30_dat$wheat_acres) +
        sum(dday10_30_dat$soybean_rev*dday10_30_dat$soybean_acres)
  
dday10_30_temp1 <- dday10_30_base + sum(((dday10_30_corn_fn(dday10_30 + 140)*(dday10_30_p_corn_fn(dday10_30 + 140)*dday10_30_dat$corn_acres))) - ((dday10_30_corn_fn(dday10_30)*(dday10_30_p_corn_fn(dday10_30)*dday10_30_dat$corn_acres))), na.rm = TRUE) +
                          sum(((dday10_30_cotton_fn(dday10_30 + 140)*(dday10_30_p_cotton_fn(dday10_30 + 140)*dday10_30_dat$cotton_acres))) - ((dday10_30_cotton_fn(dday10_30)*(dday10_30_p_cotton_fn(dday10_30)*dday10_30_dat$cotton_acres))), na.rm = TRUE) +
                          sum(((dday10_30_hay_fn(dday10_30 + 140)*(dday10_30_p_hay_fn(dday10_30 + 140)*dday10_30_dat$hay_acres))) - ((dday10_30_hay_fn(dday10_30)*(dday10_30_p_hay_fn(dday10_30)*dday10_30_dat$hay_acres))), na.rm = TRUE) +
                          sum(((dday10_30_wheat_fn(dday10_30 + 140)*(dday10_30_p_wheat_fn(dday10_30 + 140)*dday10_30_dat$wheat_acres))) - ((dday10_30_wheat_fn(dday10_30)*(dday10_30_p_wheat_fn(dday10_30)*dday10_30_dat$wheat_acres))), na.rm = TRUE) +
                          sum(((dday10_30_soybean_fn(dday10_30 + 140)*(dday10_30_p_soybean_fn(dday10_30 + 140)*dday10_30_dat$soybean_acres))) - ((dday10_30_soybean_fn(dday10_30)*(dday10_30_p_soybean_fn(dday10_30)*dday10_30_dat$soybean_acres))), na.rm = TRUE)

dday10_30_temp2 <- dday10_30_base + sum(((dday10_30_corn_fn(dday10_30 + 280)*(dday10_30_p_corn_fn(dday10_30 + 280)*dday10_30_dat$corn_acres))) - ((dday10_30_corn_fn(dday10_30)*(dday10_30_p_corn_fn(dday10_30)*dday10_30_dat$corn_acres))), na.rm = TRUE) +
                          sum(((dday10_30_cotton_fn(dday10_30 + 280)*(dday10_30_p_cotton_fn(dday10_30 + 280)*dday10_30_dat$cotton_acres))) - ((dday10_30_cotton_fn(dday10_30)*(dday10_30_p_cotton_fn(dday10_30)*dday10_30_dat$cotton_acres))), na.rm = TRUE) +
                          sum(((dday10_30_hay_fn(dday10_30 + 280)*(dday10_30_p_hay_fn(dday10_30 + 280)*dday10_30_dat$hay_acres))) - ((dday10_30_hay_fn(dday10_30)*(dday10_30_p_hay_fn(dday10_30)*dday10_30_dat$hay_acres))), na.rm = TRUE) +
                          sum(((dday10_30_wheat_fn(dday10_30 + 280)*(dday10_30_p_wheat_fn(dday10_30 + 280)*dday10_30_dat$wheat_acres))) - ((dday10_30_wheat_fn(dday10_30)*(dday10_30_p_wheat_fn(dday10_30)*dday10_30_dat$wheat_acres))), na.rm = TRUE) +
                          sum(((dday10_30_soybean_fn(dday10_30 + 280)*(dday10_30_p_soybean_fn(dday10_30 + 280)*dday10_30_dat$soybean_acres))) - ((dday10_30_soybean_fn(dday10_30)*(dday10_30_p_soybean_fn(dday10_30)*dday10_30_dat$soybean_acres))), na.rm = TRUE)

dday10_30_temp3 <- dday10_30_base + sum(((dday10_30_corn_fn(dday10_30 +420)*(dday10_30_p_corn_fn(dday10_30 +420)*dday10_30_dat$corn_acres))) - ((dday10_30_corn_fn(dday10_30)*(dday10_30_p_corn_fn(dday10_30)*dday10_30_dat$corn_acres))), na.rm = TRUE) +
                          sum(((dday10_30_cotton_fn(dday10_30 +420)*(dday10_30_p_cotton_fn(dday10_30 +420)*dday10_30_dat$cotton_acres))) - ((dday10_30_cotton_fn(dday10_30)*(dday10_30_p_cotton_fn(dday10_30)*dday10_30_dat$cotton_acres))), na.rm = TRUE) +
                          sum(((dday10_30_hay_fn(dday10_30 +420)*(dday10_30_p_hay_fn(dday10_30 +420)*dday10_30_dat$hay_acres))) - ((dday10_30_hay_fn(dday10_30)*(dday10_30_p_hay_fn(dday10_30)*dday10_30_dat$hay_acres))), na.rm = TRUE) +
                          sum(((dday10_30_wheat_fn(dday10_30 +420)*(dday10_30_p_wheat_fn(dday10_30 +420)*dday10_30_dat$wheat_acres))) - ((dday10_30_wheat_fn(dday10_30)*(dday10_30_p_wheat_fn(dday10_30)*dday10_30_dat$wheat_acres))), na.rm = TRUE) +
                          sum(((dday10_30_soybean_fn(dday10_30 +420)*(dday10_30_p_soybean_fn(dday10_30 +420)*dday10_30_dat$soybean_acres))) - ((dday10_30_soybean_fn(dday10_30)*(dday10_30_p_soybean_fn(dday10_30)*dday10_30_dat$soybean_acres))), na.rm = TRUE)

                          
dday10_30_temp4 <- dday10_30_base + sum(((dday10_30_corn_fn(dday10_30 + 560)*(dday10_30_p_corn_fn(dday10_30 + 560)*dday10_30_dat$corn_acres))) - ((dday10_30_corn_fn(dday10_30)*(dday10_30_p_corn_fn(dday10_30)*dday10_30_dat$corn_acres))), na.rm = TRUE) +
                          sum(((dday10_30_cotton_fn(dday10_30 + 560)*(dday10_30_p_cotton_fn(dday10_30 + 560)*dday10_30_dat$cotton_acres))) - ((dday10_30_cotton_fn(dday10_30)*(dday10_30_p_cotton_fn(dday10_30)*dday10_30_dat$cotton_acres))), na.rm = TRUE) +
                          sum(((dday10_30_hay_fn(dday10_30 + 560)*(dday10_30_p_hay_fn(dday10_30 + 560)*dday10_30_dat$hay_acres))) - ((dday10_30_hay_fn(dday10_30)*(dday10_30_p_hay_fn(dday10_30)*dday10_30_dat$hay_acres))), na.rm = TRUE) +
                          sum(((dday10_30_wheat_fn(dday10_30 + 560)*(dday10_30_p_wheat_fn(dday10_30 + 560)*dday10_30_dat$wheat_acres))) - ((dday10_30_wheat_fn(dday10_30)*(dday10_30_p_wheat_fn(dday10_30)*dday10_30_dat$wheat_acres))), na.rm = TRUE) +
                          sum(((dday10_30_soybean_fn(dday10_30 + 560)*(dday10_30_p_soybean_fn(dday10_30 + 560)*dday10_30_dat$soybean_acres))) - ((dday10_30_soybean_fn(dday10_30)*(dday10_30_p_soybean_fn(dday10_30)*dday10_30_dat$soybean_acres))), na.rm = TRUE)

                          
dday10_30_temp5 <- dday10_30_base + sum(((dday10_30_corn_fn(dday10_30 + + 700)*(dday10_30_p_corn_fn(dday10_30 + 700)*dday10_30_dat$corn_acres))) - ((dday10_30_corn_fn(dday10_30)*(dday10_30_p_corn_fn(dday10_30)*dday10_30_dat$corn_acres))), na.rm = TRUE) +
                          sum(((dday10_30_cotton_fn(dday10_30 + 700)*(dday10_30_p_cotton_fn(dday10_30 + 700)*dday10_30_dat$cotton_acres))) - ((dday10_30_cotton_fn(dday10_30)*(dday10_30_p_cotton_fn(dday10_30)*dday10_30_dat$cotton_acres))), na.rm = TRUE) +
                          sum(((dday10_30_hay_fn(dday10_30 + 700)*(dday10_30_p_hay_fn(dday10_30 + 700)*dday10_30_dat$hay_acres))) - ((dday10_30_hay_fn(dday10_30)*(dday10_30_p_hay_fn(dday10_30)*dday10_30_dat$hay_acres))), na.rm = TRUE) +
                          sum(((dday10_30_wheat_fn(dday10_30 + 700)*(dday10_30_p_wheat_fn(dday10_30 + 700)*dday10_30_dat$wheat_acres))) - ((dday10_30_wheat_fn(dday10_30)*(dday10_30_p_wheat_fn(dday10_30)*dday10_30_dat$wheat_acres))), na.rm = TRUE) +
                          sum(((dday10_30_soybean_fn(dday10_30 + 700)*(dday10_30_p_soybean_fn(dday10_30 + 700)*dday10_30_dat$soybean_acres))) - ((dday10_30_soybean_fn(dday10_30)*(dday10_30_p_soybean_fn(dday10_30)*dday10_30_dat$soybean_acres))), na.rm = TRUE)

adapt4_dday10_30 <- data.frame(temp = "Degree Day (10-30C)",
                          model = "Total Effect w/ Crop-switching",
                     rev = c(dday10_30_base, dday10_30_temp1, dday10_30_temp2, dday10_30_temp3, dday10_30_temp4, dday10_30_temp5))
adapt4_dday10_30$change = (adapt4_dday10_30$rev - first(adapt4_dday10_30$rev))/first(adapt4_dday10_30$rev)*100
adapt4_dday10_30
plot(x=0:5, adapt4_dday10_30$change)
lines(x=0:5, adapt4_dday10_30$change)

# Degree Day 30C
dday30 <- dday30_dat$dday30
dday30[1] <- dday30_dat$dday30[1] + 0.1
dday30[length(dday30_dat$dday30)] <- dday30[length(dday30_dat$dday30)] - 0.1

dday30_base <- sum(dday30_dat$corn_rev*dday30_dat$corn_acres) +
        sum(dday30_dat$cotton_rev*dday30_dat$cotton_acres) +
        sum(dday30_dat$hay_rev*dday30_dat$hay_acres) +
        sum(dday30_dat$wheat_rev*dday30_dat$wheat_acres) +
        sum(dday30_dat$soybean_rev*dday30_dat$soybean_acres)
  
dday30_temp1 <- dday30_base + sum(((dday30_corn_fn(dday30 + 15)*(dday30_p_corn_fn(dday30 + 15)*dday30_dat$corn_acres))) - ((dday30_corn_fn(dday30)*(dday30_p_corn_fn(dday30)*dday30_dat$corn_acres))), na.rm = TRUE) +
                          sum(((dday30_cotton_fn(dday30 + 15)*(dday30_p_cotton_fn(dday30 + 15)*dday30_dat$cotton_acres))) - ((dday30_cotton_fn(dday30)*(dday30_p_cotton_fn(dday30)*dday30_dat$cotton_acres))), na.rm = TRUE) +
                          sum(((dday30_hay_fn(dday30 + 15)*(dday30_p_hay_fn(dday30 + 15)*dday30_dat$hay_acres))) - ((dday30_hay_fn(dday30)*(dday30_p_hay_fn(dday30)*dday30_dat$hay_acres))), na.rm = TRUE) +
                          sum(((dday30_wheat_fn(dday30 + 15)*(dday30_p_wheat_fn(dday30 + 15)*dday30_dat$wheat_acres))) - ((dday30_wheat_fn(dday30)*(dday30_p_wheat_fn(dday30)*dday30_dat$wheat_acres))), na.rm = TRUE) +
                          sum(((dday30_soybean_fn(dday30 + 15)*(dday30_p_soybean_fn(dday30 + 15)*dday30_dat$soybean_acres))) - ((dday30_soybean_fn(dday30)*(dday30_p_soybean_fn(dday30)*dday30_dat$soybean_acres))), na.rm = TRUE)

dday30_temp2 <- dday30_base + sum(((dday30_corn_fn(dday30 + 30)*(dday30_p_corn_fn(dday30 + 30)*dday30_dat$corn_acres))) - ((dday30_corn_fn(dday30)*(dday30_p_corn_fn(dday30)*dday30_dat$corn_acres))), na.rm = TRUE) +
                          sum(((dday30_cotton_fn(dday30 + 30)*(dday30_p_cotton_fn(dday30 + 30)*dday30_dat$cotton_acres))) - ((dday30_cotton_fn(dday30)*(dday30_p_cotton_fn(dday30)*dday30_dat$cotton_acres))), na.rm = TRUE) +
                          sum(((dday30_hay_fn(dday30 + 30)*(dday30_p_hay_fn(dday30 + 30)*dday30_dat$hay_acres))) - ((dday30_hay_fn(dday30)*(dday30_p_hay_fn(dday30)*dday30_dat$hay_acres))), na.rm = TRUE) +
                          sum(((dday30_wheat_fn(dday30 + 30)*(dday30_p_wheat_fn(dday30 + 30)*dday30_dat$wheat_acres))) - ((dday30_wheat_fn(dday30)*(dday30_p_wheat_fn(dday30)*dday30_dat$wheat_acres))), na.rm = TRUE) +
                          sum(((dday30_soybean_fn(dday30 + 30)*(dday30_p_soybean_fn(dday30 + 30)*dday30_dat$soybean_acres))) - ((dday30_soybean_fn(dday30)*(dday30_p_soybean_fn(dday30)*dday30_dat$soybean_acres))), na.rm = TRUE)

dday30_temp3 <- dday30_base + sum(((dday30_corn_fn(dday30 + 45)*(dday30_p_corn_fn(dday30 + 45)*dday30_dat$corn_acres))) - ((dday30_corn_fn(dday30)*(dday30_p_corn_fn(dday30)*dday30_dat$corn_acres))), na.rm = TRUE) +
                          sum(((dday30_cotton_fn(dday30 + 45)*(dday30_p_cotton_fn(dday30 + 45)*dday30_dat$cotton_acres))) - ((dday30_cotton_fn(dday30)*(dday30_p_cotton_fn(dday30)*dday30_dat$cotton_acres))), na.rm = TRUE) +
                          sum(((dday30_hay_fn(dday30 + 45)*(dday30_p_hay_fn(dday30 + 45)*dday30_dat$hay_acres))) - ((dday30_hay_fn(dday30)*(dday30_p_hay_fn(dday30)*dday30_dat$hay_acres))), na.rm = TRUE) +
                          sum(((dday30_wheat_fn(dday30 + 45)*(dday30_p_wheat_fn(dday30 + 45)*dday30_dat$wheat_acres))) - ((dday30_wheat_fn(dday30)*(dday30_p_wheat_fn(dday30)*dday30_dat$wheat_acres))), na.rm = TRUE) +
                          sum(((dday30_soybean_fn(dday30 + 45)*(dday30_p_soybean_fn(dday30 + 45)*dday30_dat$soybean_acres))) - ((dday30_soybean_fn(dday30)*(dday30_p_soybean_fn(dday30)*dday30_dat$soybean_acres))), na.rm = TRUE)

                          
dday30_temp4 <- dday30_base + sum(((dday30_corn_fn(dday30 + 60)*(dday30_p_corn_fn(dday30 + 60)*dday30_dat$corn_acres))) - ((dday30_corn_fn(dday30)*(dday30_p_corn_fn(dday30)*dday30_dat$corn_acres))), na.rm = TRUE) +
                          sum(((dday30_cotton_fn(dday30 + 60)*(dday30_p_cotton_fn(dday30 + 60)*dday30_dat$cotton_acres))) - ((dday30_cotton_fn(dday30)*(dday30_p_cotton_fn(dday30)*dday30_dat$cotton_acres))), na.rm = TRUE) +
                          sum(((dday30_hay_fn(dday30 + 60)*(dday30_p_hay_fn(dday30 + 60)*dday30_dat$hay_acres))) - ((dday30_hay_fn(dday30)*(dday30_p_hay_fn(dday30)*dday30_dat$hay_acres))), na.rm = TRUE) +
                          sum(((dday30_wheat_fn(dday30 + 60)*(dday30_p_wheat_fn(dday30 + 60)*dday30_dat$wheat_acres))) - ((dday30_wheat_fn(dday30)*(dday30_p_wheat_fn(dday30)*dday30_dat$wheat_acres))), na.rm = TRUE) +
                          sum(((dday30_soybean_fn(dday30 + 60)*(dday30_p_soybean_fn(dday30 + 60)*dday30_dat$soybean_acres))) - ((dday30_soybean_fn(dday30)*(dday30_p_soybean_fn(dday30)*dday30_dat$soybean_acres))), na.rm = TRUE)

                          
dday30_temp5 <- dday30_base + sum(((dday30_corn_fn(dday30 + 75)*(dday30_p_corn_fn(dday30 + 75)*dday30_dat$corn_acres))) - ((dday30_corn_fn(dday30)*(dday30_p_corn_fn(dday30)*dday30_dat$corn_acres))), na.rm = TRUE) +
                          sum(((dday30_cotton_fn(dday30 + 75)*(dday30_p_cotton_fn(dday30 + 75)*dday30_dat$cotton_acres))) - ((dday30_cotton_fn(dday30)*(dday30_p_cotton_fn(dday30)*dday30_dat$cotton_acres))), na.rm = TRUE) +
                          sum(((dday30_hay_fn(dday30 + 75)*(dday30_p_hay_fn(dday30 + 75)*dday30_dat$hay_acres))) - ((dday30_hay_fn(dday30)*(dday30_p_hay_fn(dday30)*dday30_dat$hay_acres))), na.rm = TRUE) +
                          sum(((dday30_wheat_fn(dday30 + 75)*(dday30_p_wheat_fn(dday30 + 75)*dday30_dat$wheat_acres))) - ((dday30_wheat_fn(dday30)*(dday30_p_wheat_fn(dday30)*dday30_dat$wheat_acres))), na.rm = TRUE) +
                          sum(((dday30_soybean_fn(dday30 + 75)*(dday30_p_soybean_fn(dday30 + 75)*dday30_dat$soybean_acres))) - ((dday30_soybean_fn(dday30)*(dday30_p_soybean_fn(dday30)*dday30_dat$soybean_acres))), na.rm = TRUE)

adapt4_dday30 <- data.frame(temp = "Degree Day (30C)",
                          model = "Total Effect w/ Crop-switching",
                     rev = c(dday30_base, dday30_temp1, dday30_temp2, dday30_temp3, dday30_temp4, dday30_temp5))
adapt4_dday30$change = (adapt4_dday30$rev - first(adapt4_dday30$rev))/first(adapt4_dday30$rev)*100
adapt4_dday30
plot(x=0:5, adapt4_dday30$change)
lines(x=0:5, adapt4_dday30$change)

# Build data.frame for plot
pdat <- rbind(adapt1_tavg, adapt1_dday10_30, adapt1_dday30, 
              adapt2_tavg, adapt2_dday10_30, adapt2_dday30,
              # adapt3_tavg, adapt3_dday10_30, adapt3_dday30,  # Do not display ME w/adapt
              adapt4_tavg, adapt4_dday10_30, adapt4_dday30)
pdat$c <- seq(0,5)

# Average temperature main plot
atext2 <- data.frame(temp = "Average Temperature (C)",
                    x = c(3.5, 2.7, 3.7),
                    y = c(-14, -24, -5),
                    model= c(
                             #"Marginal Effect \n w/ Adaptation", 
                             "Marginal Effect \n w/o Adaptation", 
                             "NL w/o Adaptation", 
                             "NL w/ Adaptation"))

ggplot(filter(pdat, temp == "Average Temperature (C)"), aes(x=c, y=change, color=model, group = model)) + 
  theme_tufte(base_size = 11) +    
  geom_line() +
  # geom_line(aes(color=model)) + 
  # geom_label(data = filter(pdat, c == 5), label = "Adaptation w/o") +
  geom_text(data = atext2, aes(x=x, y=y, label=model, color=model, group=model), size = 2.8) +
  # annotate("text", x = 4, y = -5, label = "Adaptation w/ \n Crop-switching", color = "blue", size = 3) +
  # annotate("text", x = 4, y = -18, label = "Adaptation w/o \n Crop-switching", color = "darkgreen", size = 3) +
  # annotate("text", x = 16, y = 29, label = "Corn", color = "red", size = 4) +
  # annotate("text", x = 21, y = 29, label = "Cotton", color = "green", size = 4) +
  xlab('Change in Temperature (C)') +
  ylab("Percentage Change from Baseline (+0C)") + 
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  scale_x_continuous(breaks = 0:5, labels = c("+0C", "+1C", "+2C", "+3C", "+4C", "+5C")) +
  scale_y_continuous(labels=function(x) paste0(x,"%")) +
  facet_wrap(~temp) +
  scale_colour_manual(values=c(
                               #"blue1", # ME w/ Adaptation line
                               "#377eb8", # ME w/o Adaptation line
                               #"blue1", # ME w Adaptation text 
                               "#377eb8", # ME w/O Adaptation text 
                               "#4daf4a", # NL w/ Aadptation text 
                               "#e41a1c", # NL w/o Adaptation text 
                               "#e41a1c", 
                               "#4daf4a")) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey", alpha = 0.5) +
  theme(legend.position = 'none')
ggsave("figures/4-tavg_main_plot.pdf", width = 6, height = 4)


atext <- data.frame(temp = "Average Temperature (C)",
                    x = c(4, 2.7, 3.7),
                    y = c(-18, -25, -7),
                    model= c(
                             #"Marginal Effect \n w/ Adaptation", 
                             "Marginal Effect \n w/o Adaptation", 
                             "NL w/o Adaptation", 
                             "NL w/ Adaptation"))
pdat <- filter(pdat, model != "Adaptation w/ Crop-switching")
ggplot(pdat, aes(x=c, y=change, color=model, group = model)) + 
  theme_tufte(base_size = 11) +    
  geom_line() +
  # geom_line(aes(color=model)) + 
  # geom_label(data = filter(pdat, c == 5), label = "Adaptation w/o") +
  geom_text(data = atext, aes(x=x, y=y, label=model, color=model, group=model), size = 2.8) +
  # annotate("text", x = 4, y = -5, label = "Adaptation w/ \n Crop-switching", color = "blue", size = 3) +
  # annotate("text", x = 4, y = -18, label = "Adaptation w/o \n Crop-switching", color = "darkgreen", size = 3) +
  # annotate("text", x = 16, y = 29, label = "Corn", color = "red", size = 4) +
  # annotate("text", x = 21, y = 29, label = "Cotton", color = "green", size = 4) +
  xlab('Change in Temperature (C)') +
  ylab("Percentage Change from Baseline (+0C)") + 
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  scale_x_continuous(breaks = 0:5, labels = c("+0C", "+1C", "+2C", "+3C", "+4C", "+5C")) +
  scale_y_continuous(labels=function(x) paste0(x,"%")) +
  facet_wrap(~temp) +
  scale_colour_manual(values=c(
                               #"blue1", # ME w/ Adaptation line
                               "#377eb8", # ME w/o Adaptation line
                               #"blue1", # ME w Adaptation text 
                               "#377eb8", # ME w/O Adaptation text 
                               "#4daf4a", # NL w/ Aadptation text 
                               "#e41a1c", # NL w/o Adaptation text 
                               "#e41a1c", 
                               "#4daf4a")) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey", alpha = 0.5) +
  theme(legend.position = 'none')
ggsave("figures/5-main_plot.pdf", width = 6, height = 4.5)





