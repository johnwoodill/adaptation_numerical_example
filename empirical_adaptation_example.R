library(tidyverse)
library(ggthemes)
library(cowplot)
library(numDeriv)

slope <- function(x1, y1, x2, y2){
  m = (y2 - y1)/(x2 - x1)
  return(m)
}

mdat <- as.data.frame(readRDS("data/full_ag_data.rds"))
dat <- filter(mdat, abs(long) <= 100)

remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

dat <- dat %>% 
  group_by(fips) %>% 
  summarise(corn_yield = mean(corn_yield, na.rm = TRUE),
            corn_mprice = mean(corn_rprice, na.rm = TRUE),
            corn_a = mean(corn_grain_a, na.rm = TRUE),
            cotton_yield = mean(cotton_yield, na.rm = TRUE),
            cotton_mprice = mean(cotton_rprice, na.rm = TRUE),
            cotton_a = mean(cotton_a, na.rm = TRUE),
            hay_yield = mean(hay_yield, na.rm = TRUE),
            hay_mprice = mean(hay_rprice, na.rm = TRUE),
            hay_a = mean(hay_a, na.rm = TRUE),
            soybean_yield = mean(soybean_yield, na.rm = TRUE),
            soybean_mprice = mean(soybean_rprice, na.rm = TRUE),
            soybean_a = mean(soybean_a, na.rm = TRUE),
            wheat_yield = mean(wheat_yield, na.rm = TRUE),
            wheat_mprice = mean(wheat_rprice, na.rm = TRUE),
            wheat_a = mean(wheat_a, na.rm = TRUE),
            corn_mrev = mean(corn_mrev, na.rm = TRUE),
            cotton_mrev = mean(cotton_mrev, na.rm = TRUE),
            hay_mrev = mean(hay_mrev, na.rm = TRUE),
            wheat_mrev = mean(wheat_mrev, na.rm = TRUE),
            soybean_mrev = mean(soybean_mrev, na.rm = TRUE),
            tavg = mean(tavg, na.rm = TRUE),
            dday10_30 = mean(dday10_30, na.rm = TRUE),
            dday30 = mean(dday30, na.rm = TRUE),
            prec = mean(prec, na.rm = TRUE),
            corn_a = mean(corn_a, na.rm = TRUE),
            p_corn_a = mean(p_corn_a, na.rm = TRUE),
            p_cotton_a = mean(p_cotton_a, na.rm = TRUE),
            p_hay_a = mean(p_hay_a, na.rm = TRUE),
            p_wheat_a = mean(p_wheat_a, na.rm = TRUE),
            p_soybean_a = mean(p_soybean_a, na.rm = TRUE))

dat$corn_rev <- dat$corn_yield*mean(mdat$corn_rprice, na.rm = TRUE)
dat$cotton_rev <- dat$cotton_yield*mean(mdat$cotton_rprice, na.rm = TRUE)
dat$hay_rev <- dat$hay_yield*mean(mdat$hay_rprice, na.rm = TRUE)
dat$wheat_rev <- dat$wheat_yield*mean(mdat$wheat_rprice, na.rm = TRUE)
dat$soybean_rev <- dat$soybean_yield*mean(mdat$soybean_rprice, na.rm = TRUE)

dat$corn_a <- remove_outliers(dat$corn_a)
dat$cotton_a <- remove_outliers(dat$cotton_a)
dat$hay_a <- remove_outliers(dat$hay_a)
dat$wheat_a <- remove_outliers(dat$wheat_a)
dat$soybean_a <- remove_outliers(dat$soybean_a)

# Cut into bins
dat$ftavg <- cut(dat$tavg, 10, labels = 1:10)
dat$fdday10_30 <- cut(dat$dday10_30, 10, labels = 1:10)
dat$fdday30 <- cut(dat$dday30, 10, labels = 1:10)

tavg_dat <- dat %>% 
  group_by(ftavg) %>% 
  summarise(corn_rev = mean(corn_rev, na.rm = TRUE),
            cotton_rev = mean(cotton_rev, na.rm = TRUE),
            hay_rev = mean(hay_rev, na.rm = TRUE),
            wheat_rev = mean(wheat_rev, na.rm = TRUE),
            soybean_rev = mean(soybean_rev, na.rm = TRUE),
            tavg = mean(tavg, na.rm = TRUE),
            p_corn_a = mean(p_corn_a, na.rm = TRUE),
            p_cotton_a = mean(p_cotton_a, na.rm = TRUE),
            p_hay_a = mean(p_hay_a, na.rm = TRUE),
            p_wheat_a = mean(p_wheat_a, na.rm = TRUE),
            p_soybean_a = mean(p_soybean_a, na.rm = TRUE),
            corn_acres = sum(corn_a, na.rm = TRUE),
            cotton_acres = sum(cotton_a, na.rm = TRUE),
            hay_acres = sum(hay_a, na.rm = TRUE),
            wheat_acres = sum(wheat_a, na.rm = TRUE),
            soybean_acres = sum(soybean_a, na.rm = TRUE)) %>% 
  mutate(ftavg = NULL) %>% 
  arrange(tavg) %>% 
  ungroup()


dday10_30_dat <- dat %>% 
    group_by(fdday10_30) %>% 
    summarise(corn_rev = mean(corn_rev, na.rm = TRUE),
            cotton_rev = mean(cotton_rev, na.rm = TRUE),
            hay_rev = mean(hay_rev, na.rm = TRUE),
            wheat_rev = mean(wheat_rev, na.rm = TRUE),
            soybean_rev = mean(soybean_rev, na.rm = TRUE),
            dday10_30 = mean(dday10_30, na.rm = TRUE),
            p_corn_a = mean(p_corn_a, na.rm = TRUE),
            p_cotton_a = mean(p_cotton_a, na.rm = TRUE),
            p_hay_a = mean(p_hay_a, na.rm = TRUE),
            p_wheat_a = mean(p_wheat_a, na.rm = TRUE),
            p_soybean_a = mean(p_soybean_a, na.rm = TRUE),
            corn_acres = sum(corn_a, na.rm = TRUE),
            cotton_acres = sum(cotton_a, na.rm = TRUE),
            hay_acres = sum(hay_a, na.rm = TRUE),
            wheat_acres = sum(wheat_a, na.rm = TRUE),
            soybean_acres = sum(soybean_a, na.rm = TRUE)) %>% 
  mutate(fdday10_30 = NULL) %>% 
  arrange(dday10_30) %>% 
  ungroup()

dday30_dat <- dat %>% 
    group_by(fdday30) %>% 
    summarise(corn_rev = mean(corn_rev, na.rm = TRUE),
            cotton_rev = mean(cotton_rev, na.rm = TRUE),
            hay_rev = mean(hay_rev, na.rm = TRUE),
            wheat_rev = mean(wheat_rev, na.rm = TRUE),
            soybean_rev = mean(soybean_rev, na.rm = TRUE),
            dday30 = mean(dday30, na.rm = TRUE),
            p_corn_a = mean(p_corn_a, na.rm = TRUE),
            p_cotton_a = mean(p_cotton_a, na.rm = TRUE),
            p_hay_a = mean(p_hay_a, na.rm = TRUE),
            p_wheat_a = mean(p_wheat_a, na.rm = TRUE),
            p_soybean_a = mean(p_soybean_a, na.rm = TRUE),
            corn_acres = sum(corn_a, na.rm = TRUE),
            cotton_acres = sum(cotton_a, na.rm = TRUE),
            hay_acres = sum(hay_a, na.rm = TRUE),
            wheat_acres = sum(wheat_a, na.rm = TRUE),
            soybean_acres = sum(soybean_a, na.rm = TRUE)) %>% 
  mutate(fdday30 = NULL) %>% 
  arrange(dday30) %>% 
  ungroup()

# Tavg plot
tavg_pdat <- gather(tavg_dat, key = crop_var, value = value, -tavg)
tavg_pdat$crop_var <- factor(tavg_pdat$crop_var, 
                             levels = c('corn_rev', 'cotton_rev', 'hay_rev', 'wheat_rev', 'soybean_rev',
                                        'p_corn_a', 'p_cotton_a', 'p_hay_a', 'p_wheat_a', 'p_soybean_a',
                                        'corn_acres', 'cotton_acres', 'hay_acres', 'wheat_acres', 'soybean_acres'),
                             labels = c("Corn Rev.", "Cotton Rev.", "Hay Rev", "Wheat Rev.", "Soybean Rev.",
                                        "P(Corn Acres)", "P(Cotton Acres)", "P(Hay Acres)", "P(Wheat Acres)", "P(Soybean Acres)",
                                        "Corn Acres", "Cotton Acres", "Hay Acres", "Wheat Acres", "Soybean Acres"))
ggplot(tavg_pdat, aes(x = tavg, y = value, color = crop_var)) + 
  theme_tufte() +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  geom_line() +
  facet_wrap(~crop_var, scales = 'free', ncol = 5) +
  theme(legend.position = 'none') +
  # ylab("Proportion of Acres              Revenue/Acre") +
  ylab(NULL) +
  xlab("Average Temperature") +
  scale_x_continuous(breaks = seq(10, 30, by = 4)) +
  NULL
ggsave("figures/bins_tavg.pdf", width = 6, height = 4)

# Degree Day 10_30 plot
dday10_30_pdat <- gather(dday10_30_dat, key = crop_var, value = value, -dday10_30)
dday10_30_pdat$crop_var <- factor(dday10_30_pdat$crop_var, 
                              levels = c('corn_rev', 'cotton_rev', 'hay_rev', 'wheat_rev', 'soybean_rev',
                                        'p_corn_a', 'p_cotton_a', 'p_hay_a', 'p_wheat_a', 'p_soybean_a',
                                        'corn_acres', 'cotton_acres', 'hay_acres', 'wheat_acres', 'soybean_acres'),
                             labels = c("Corn Rev.", "Cotton Rev.", "Hay Rev", "Wheat Rev.", "Soybean Rev.",
                                        "P(Corn Acres)", "P(Cotton Acres)", "P(Hay Acres)", "P(Wheat Acres)", "P(Soybean Acres)",
                                        "Corn Acres", "Cotton Acres", "Hay Acres", "Wheat Acres", "Soybean Acres"))

ggplot(dday10_30_pdat, aes(x = dday10_30, y = value, color = crop_var)) + 
  theme_tufte() +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  geom_line() +
  facet_wrap(~crop_var, scales = 'free', ncol = 5) +
  theme(legend.position = 'none',
        axis.text.x = element_text(angle = 90, hjust = 1)) +
  # ylab("Proportion of Acres              Revenue/Acre") +
  ylab(NULL) +
  xlab("Degree Days (10-30C)") +
  scale_x_continuous(breaks = c(930, 1300, 1700, 2100, 2500, 2900)) +
  
  NULL

ggsave("figures/bins_dday10_30.pdf", width = 6, height = 4)

# Degree Day 30 Plot
dday30_pdat <- gather(dday30_dat, key = crop_var, value = value, -dday30)
dday30_pdat$crop_var <- factor(dday30_pdat$crop_var, 
                             levels = c('corn_rev', 'cotton_rev', 'hay_rev', 'wheat_rev', 'soybean_rev',
                                        'p_corn_a', 'p_cotton_a', 'p_hay_a', 'p_wheat_a', 'p_soybean_a',
                                        'corn_acres', 'cotton_acres', 'hay_acres', 'wheat_acres', 'soybean_acres'),
                             labels = c("Corn Rev.", "Cotton Rev.", "Hay Rev", "Wheat Rev.", "Soybean Rev.",
                                        "P(Corn Acres)", "P(Cotton Acres)", "P(Hay Acres)", "P(Wheat Acres)", "P(Soybean Acres)",
                                        "Corn Acres", "Cotton Acres", "Hay Acres", "Wheat Acres", "Soybean Acres"))

ggplot(dday30_pdat, aes(x = dday30, y = value, color = crop_var)) + 
  theme_tufte() +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  geom_line() +
  facet_wrap(~crop_var, scales = 'free', ncol = 5) +
  theme(legend.position = 'none',
        axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylab(NULL) +
  # ylab("Proportion of Acres              Revenue/Acre") +
  xlab("Degree Days (30C)") +
  scale_x_continuous(breaks = c(3, 30, 56, 82, 109, 135, 165)) +
  NULL

ggsave("figures/bins_dday30.pdf", width = 6, height = 4)

# Build functions
# tavg_corn_fn <- approxfun(locpoly(tavg_dat$tavg, tavg_dat$corn_rev, bandwidth = 5))
# tavg_cotton_fn <- approxfun(locpoly(tavg_dat$tavg, tavg_dat$cotton_rev, bandwidth = 5))
# tavg_hay_fn <- approxfun(locpoly(tavg_dat$tavg, tavg_dat$hay_rev, bandwidth = 5))
# tavg_wheat_fn <- approxfun(locpoly(tavg_dat$tavg, tavg_dat$wheat_rev, bandwidth = 5))
# tavg_soybean_fn <- approxfun(locpoly(tavg_dat$tavg, tavg_dat$soybean_rev, bandwidth = 5))
# 
# dday10_30_corn_fn <- approxfun(locpoly(dday10_30_dat$dday10_30, dday10_30_dat$corn_rev, bandwidth = 5))
# dday10_30_cotton_fn <- approxfun(locpoly(dday10_30_dat$dday10_30, dday10_30_dat$cotton_rev, bandwidth = 5))
# dday10_30_hay_fn <- approxfun(locpoly(dday10_30_dat$dday10_30, dday10_30_dat$hay_rev, bandwidth = 5))
# dday10_30_wheat_fn <- approxfun(locpoly(dday10_30_dat$dday10_30, dday10_30_dat$wheat_rev, bandwidth = 5))
# dday10_30_soybean_fn <- approxfun(locpoly(dday10_30_dat$dday10_30, dday10_30_dat$soybean_rev, bandwidth = 5))
# 
# dday30_corn_fn <- approxfun(locpoly(dday30_dat$dday30, dday30_dat$corn_rev, bandwidth = 5))
# dday30_cotton_fn <- approxfun(locpoly(dday30_dat$dday30, dday30_dat$cotton_rev, bandwidth = 5))
# dday30_hay_fn <- approxfun(locpoly(dday30_dat$dday30, dday30_dat$hay_rev, bandwidth = 5))
# dday30_wheat_fn <- approxfun(locpoly(dday30_dat$dday30, dday30_dat$wheat_rev, bandwidth = 5))
# dday30_soybean_fn <- approxfun(locpoly(dday30_dat$dday30, dday30_dat$soybean_rev, bandwidth = 5))

# Linear int
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



int_pdat <- data.frame(temp = rep(c("Average Temp. (C)", "Degree Day (10-30C)", "Degree Day (30C)"), each = 50),
                       crop = rep(c("Corn", "Cotton", "Hay", "Wheat", "Soybean"), each = 10),
                       x = c(rep(tavg_dat$tavg, 5), rep(dday10_30_dat$dday10_30, 5), rep(dday30_dat$dday30, 5)),
                       y = c(tavg_corn_fn(tavg_dat$tavg), tavg_cotton_fn(tavg_dat$tavg), tavg_hay_fn(tavg_dat$tavg), tavg_wheat_fn(tavg_dat$tavg), tavg_soybean_fn(tavg_dat$tavg),
                             dday10_30_corn_fn(dday10_30_dat$dday10_30), dday10_30_cotton_fn(dday10_30_dat$dday10_30), dday10_30_hay_fn(dday10_30_dat$dday10_30), dday10_30_wheat_fn(dday10_30_dat$dday10_30), dday10_30_soybean_fn(dday10_30_dat$dday10_30),
                             dday30_corn_fn(dday30_dat$dday30), dday30_cotton_fn(dday30_dat$dday30), dday30_hay_fn(dday30_dat$dday30), dday30_wheat_fn(dday30_dat$dday30), dday30_soybean_fn(dday30_dat$dday30)))


ggplot(int_pdat, aes(x=x, y=y, color = crop)) + 
  theme_tufte() +    
  geom_line() + 
  ylab("Revenue/Acre") +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  facet_wrap(temp~crop) +
  theme(legend.position = 'none') +
  facet_wrap(temp~crop, scales = 'free', ncol = 5)
ggsave("figures/interp_rev_plot.pdf", height = 6, width = 8)

#---------------------------------------------
# TOTAL EFFECT
# Average Temperatures
tavg <- tavg_dat$tavg
tavg[1] <- tavg_dat$tavg[1] + 0.1
tavg[length(tavg_dat$tavg)] <- tavg[length(tavg_dat$tavg)] - 0.1

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
  
adapt1_tavg <- data.frame(temp = "Average Temperature (C)",
                          model = "Total Effect",
                     rev = c(tavg_base, tavg_temp1, tavg_temp2, tavg_temp3, tavg_temp4, tavg_temp5))
adapt1_tavg$change = (adapt1_tavg$rev - first(adapt1_tavg$rev))/first(adapt1_tavg$rev)*100
adapt1_tavg
plot(x=0:5, adapt1_tavg$change)
lines(x=0:5, adapt1_tavg$change)


# Degree Days 10-30
dday10_30 <- dday10_30_dat$dday10_30
dday10_30[1] <- dday10_30_dat$dday10_30[1] + 0.1
dday10_30[length(dday10_30_dat$dday10_30)] <- dday10_30[length(dday10_30_dat$dday10_30)] - 0.1

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
  
adapt1_dday10_30 <- data.frame(temp = "Degree Day (10-30C)",
                          model = "Total Effect",
                     rev = c(dday10_30_base, dday10_30_temp1, dday10_30_temp2, dday10_30_temp3, dday10_30_temp4, dday10_30_temp5))
adapt1_dday10_30$change = (adapt1_dday10_30$rev - first(adapt1_dday10_30$rev))/first(adapt1_dday10_30$rev)*100
adapt1_dday10_30
plot(x=0:5, adapt1_dday10_30$change)
lines(x=0:5, adapt1_dday10_30$change)




# Degree Days 30
dday30 <- dday30_dat$dday30
dday30[1] <- dday30_dat$dday30[1] + 0.1
dday30[length(dday30_dat$dday30)] <- dday30[length(dday30_dat$dday30)] - 0.1

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
  
adapt1_dday30 <- data.frame(temp = "Degree Day (30C)",
                          model = "Total Effect",
                     rev = c(dday30_base, dday30_temp1, dday30_temp2, dday30_temp3, dday30_temp4, dday30_temp5))
adapt1_dday30$change = (adapt1_dday30$rev - first(adapt1_dday30$rev))/first(adapt1_dday30$rev)*100
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

pdat <- rbind(adapt1_tavg, adapt1_dday10_30, adapt1_dday30, 
              adapt2_tavg, adapt2_dday10_30, adapt2_dday30,
              adapt3_tavg, adapt3_dday10_30, adapt3_dday30,
              adapt4_tavg, adapt4_dday10_30, adapt4_dday30)
pdat$c <- seq(0,5)

atext <- data.frame(temp = "Average Temperature (C)",
                    x = c(4, 2.7, 3.7),
                    y = c(-18, -25, -7),
                    model= c(
                             #"Marginal Effect \n w/ Adaptation", 
                             "Marginal Effect \n w/o Adaptation", 
                             "NL w/o Adaptation", 
                             "NL w/ Adaptation"))
atext

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
ggsave("figures/main_plot.pdf", width = 6, height = 4.5)
# ggsave("figures/main_plot.svg", width = 6, height = 4.5)





#---------------------
# p1 <- ggplot(dplot, aes(x = x, y = y/1000000, color = crop)) + 
#   theme_tufte() +    
#   geom_line() + 
#   #xlab('Average Temperature (C)') +
#   xlab(NULL) + 
#   ylab("Value of Activity \n ($1 million)") + 
#   annotate("text", x = 12, y = 14, label = "Wheat", color = "blue", size = 4) +
#   annotate("text", x = 16, y = 29, label = "Corn", color = "red", size = 4) +
#   annotate("text", x = 21, y = 29, label = "Cotton", color = "green", size = 4) +
#   annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
#   annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
#   theme(legend.position = 'top') + theme(
#                          #legend.position = c(0.05, 0.9),
#                          #legend.title = element_blank(),
#                          #legend.background = element_rect(
#                          #        size=0.5, linetype="solid", 
#                          #\       colour = "grey"),
#                          legend.position = "none",
#                          axis.text.x = element_blank(),
#                          #axis.text.y = element_blank(),
#                          axis.ticks.x = element_blank(),
#                          #axis.ticks.y = element_blank(),
#                          #panel.border = element_rect(fill = NA),
#                          plot.margin = unit(c(0, 0, 3, 0), "cm")) 
# p1
# dat$acres = rowSums(dat[, c("wheat_a", "corn_a", "cotton_a")], na.rm = TRUE)
# p2 <- ggplot(dat, aes(tavg)) + 
#   ylim(0, 0.18) +
#   theme_tufte() +
#   geom_density(aes(weight = acres/sum(acres)), color = "grey") +
#   annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
#   annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
#   ylab("Density") +
#   scale_x_continuous(breaks = 0:30) +
#   xlab("Average Temperature(C)")
# p2
# ggdraw() + draw_plot(p1) + draw_plot(p2, 0, height = .30, width = 1.01)
# ggsave("figures/empirical_graph.pdf", width = 6, height = 4)
# 
