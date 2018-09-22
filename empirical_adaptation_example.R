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

dat$corn_rev <- dat$corn_yield*mean(dat$corn_mprice, na.rm = TRUE)
dat$cotton_rev <- dat$cotton_yield*mean(dat$cotton_mprice, na.rm = TRUE)
dat$hay_rev <- dat$hay_yield*mean(dat$hay_mprice, na.rm = TRUE)
dat$wheat_rev <- dat$wheat_yield*mean(dat$wheat_mprice, na.rm = TRUE)
dat$soybean_rev <- dat$soybean_yield*mean(dat$soybean_mprice, na.rm = TRUE)

# Cut into bins
dat$ftavg <- cut(dat$tavg, 30, labels = 1:30)
dat$fdday10_30 <- cut(dat$dday10_30, 30, labels = 1:30)
dat$fdday30 <- cut(dat$dday30, 30, labels = 1:30)

tavg_dat <- dat %>% 
  group_by(ftavg) %>% 
  summarise(corn_rev = mean(corn_rev, na.rm = TRUE),
            cotton_rev = mean(cotton_rev, na.rm = TRUE),
            hay_rev = mean(hay_rev, na.rm = TRUE),
            wheat_rev = mean(wheat_rev, na.rm = TRUE),
            soybean_rev = mean(soybean_rev, na.rm = TRUE),
            tavg = mean(tavg),
            p_corn_a = mean(p_corn_a),
            p_cotton_a = mean(p_cotton_a),
            p_hay_a = mean(p_hay_a),
            p_wheat_a = mean(p_wheat_a),
            p_soybean_a = mean(p_soybean_a),
            corn_acres = sum(corn_a),
            cotton_acres = sum(cotton_a),
            hay_acres = sum(hay_a),
            wheat_acres = sum(wheat_a),
            soybean_acres = sum(soybean_a)) %>% 
  mutate(ftavg = NULL) %>% 
  arrange(tavg) %>% 
  ungroup()


locpoly(tavg_dat$tavg, tavg_dat$corn_rev, bandwidth = 5)$y
plot(locpoly(tavg_dat$tavg, tavg_dat$cotton_rev, bandwidth = 5))
plot(locpoly(tavg_dat$tavg, tavg_dat$hay_rev, bandwidth = 5))
plot(locpoly(tavg_dat$tavg, tavg_dat$wheat_rev, bandwidth = 5))
plot(locpoly(tavg_dat$tavg, tavg_dat$soybean_rev, bandwidth = 5))


plot(locpoly(tavg_dat$tavg, tavg_dat$p_corn_a, bandwidth = 5))
# tavg_dat$corn_rev <- approx(tavg_dat$corn_rev, xout = tavg_dat$tavg)$y
# tavg_dat$cotton_rev <- approx(tavg_dat$cotton_rev, xout = tavg_dat$tavg)$y
# tavg_dat$hay_rev <- approx(tavg_dat$hay_rev, xout = tavg_dat$tavg)$y
# tavg_dat$wheat_rev <- approx(tavg_dat$wheat_rev, xout = tavg_dat$tavg)$y
# tavg_dat$soybean_rev <- approx(tavg_dat$soybean_rev, xout = tavg_dat$tavg)$y
# 
# tavg_dat$p_corn_a <- approx(tavg_dat$p_corn_a, xout = tavg_dat$tavg)$y
# tavg_dat$p_cotton_a <- approx(tavg_dat$p_cotton_a, xout = tavg_dat$tavg)$y
# tavg_dat$p_hay_a <- approx(tavg_dat$p_hay_a, xout = tavg_dat$tavg)$y
# tavg_dat$p_wheat_a <- approx(tavg_dat$p_wheat_a, xout = tavg_dat$tavg)$y
# tavg_dat$p_soybean_a <- approx(tavg_dat$p_soybean_a, xout = tavg_dat$tavg)$y

dday10_30_dat <- dat %>% 
    group_by(fdday10_30) %>% 
    summarise(corn_rev = mean(corn_rev, na.rm = TRUE),
            cotton_rev = mean(cotton_rev, na.rm = TRUE),
            hay_rev = mean(hay_rev, na.rm = TRUE),
            wheat_rev = mean(wheat_rev, na.rm = TRUE),
            soybean_rev = mean(soybean_rev, na.rm = TRUE),
            dday10_30 = mean(dday10_30),
            p_corn_a = mean(p_corn_a),
            p_cotton_a = mean(p_cotton_a),
            p_hay_a = mean(p_hay_a),
            p_wheat_a = mean(p_wheat_a),
            p_soybean_a = mean(p_soybean_a),
            corn_acres = sum(corn_a),
            cotton_acres = sum(cotton_a),
            hay_acres = sum(hay_a),
            wheat_acres = sum(wheat_a),
            soybean_acres = sum(soybean_a)) %>% 
  mutate(fdday10_30 = NULL) %>% 
  arrange(dday10_30) %>% 
  ungroup()

# dday10_30_dat$corn_rev <- approx(dday10_30_dat$corn_rev, x = dday10_30_dat$dday10_30, xout = dday10_30_dat$dday10_30)$y
# dday10_30_dat$cotton_rev <- approx(dday10_30_dat$cotton_rev, x = dday10_30_dat$dday10_30,xout = dday10_30_dat$dday10_30)$y
# dday10_30_dat$hay_rev <- approx(dday10_30_dat$hay_rev, x = dday10_30_dat$dday10_30,xout = dday10_30_dat$dday10_30)$y
# dday10_30_dat$wheat_rev <- approx(dday10_30_dat$wheat_rev, x = dday10_30_dat$dday10_30,xout = dday10_30_dat$dday10_30)$y
# dday10_30_dat$soybean_rev <- approx(dday10_30_dat$soybean_rev, x = dday10_30_dat$dday10_30,xout = dday10_30_dat$dday10_30)$y
# 
# dday10_30_dat$p_corn_a <- approx(dday10_30_dat$p_corn_a, x = dday10_30_dat$dday10_30, xout = dday10_30_dat$dday10_30)$y
# dday10_30_dat$p_cotton_a <- approx(dday10_30_dat$p_cotton_a, x = dday10_30_dat$dday10_30,xout = dday10_30_dat$dday10_30)$y
# dday10_30_dat$p_hay_a <- approx(dday10_30_dat$p_hay_a, x = dday10_30_dat$dday10_30,xout = dday10_30_dat$dday10_30)$y
# dday10_30_dat$p_wheat_a <- approx(dday10_30_dat$p_wheat_a, x = dday10_30_dat$dday10_30,xout = dday10_30_dat$dday10_30)$y
# dday10_30_dat$p_soybean_a <- approx(dday10_30_dat$p_soybean_a, x = dday10_30_dat$dday10_30,xout = dday10_30_dat$dday10_30)$y


dday30_dat <- dat %>% 
    group_by(fdday30) %>% 
    summarise(corn_rev = mean(corn_rev, na.rm = TRUE),
            cotton_rev = mean(cotton_rev, na.rm = TRUE),
            hay_rev = mean(hay_rev, na.rm = TRUE),
            wheat_rev = mean(wheat_rev, na.rm = TRUE),
            soybean_rev = mean(soybean_rev, na.rm = TRUE),
            dday30 = mean(dday30),
            p_corn_a = mean(p_corn_a),
            p_cotton_a = mean(p_cotton_a),
            p_hay_a = mean(p_hay_a),
            p_wheat_a = mean(p_wheat_a),
            p_soybean_a = mean(p_soybean_a),
            corn_acres = sum(corn_a),
            cotton_acres = sum(cotton_a),
            hay_acres = sum(hay_a),
            wheat_acres = sum(wheat_a),
            soybean_acres = sum(soybean_a)) %>% 
  mutate(fdday30 = NULL) %>% 
  arrange(dday30) %>% 
  ungroup()

# dday30_dat$corn_rev <- approx(dday30_dat$corn_rev, xout = dday30_dat$dday30)$y
# dday30_dat$cotton_rev <- approx(dday30_dat$cotton_rev, xout = dday30_dat$dday30)$y
# dday30_dat$hay_rev <- approx(dday30_dat$hay_rev, xout = dday30_dat$dday30)$y
# dday30_dat$wheat_rev <- approx(dday30_dat$wheat_rev, xout = dday30_dat$dday30)$y
# dday30_dat$soybean_rev <- approx(dday30_dat$soybean_rev, xout = dday30_dat$dday30)$y
# 
# dday30_dat$p_corn_a <- approx(dday30_dat$p_corn_a, xout = dday30_dat$dday30)$y
# dday30_dat$p_cotton_a <- approx(dday30_dat$p_cotton_a, xout = dday30_dat$dday30)$y
# dday30_dat$p_hay_a <- approx(dday30_dat$p_hay_a, xout = dday30_dat$dday30)$y
# dday30_dat$p_wheat_a <- approx(dday30_dat$p_wheat_a, xout = dday30_dat$dday30)$y
# dday30_dat$p_soybean_a <- approx(dday30_dat$p_soybean_a, xout = dday30_dat$dday30)$y

# Tavg plot
tavg_pdat <- select(tavg_dat, tavg, corn_rev, cotton_rev, hay_rev, wheat_rev, soybean_rev,
                    p_corn_a, p_cotton_a, p_hay_a, p_wheat_a, p_soybean_a)
tavg_pdat <- gather(tavg_dat, key = crop_var, value = value, -tavg)
tavg_pdat$crop_var <- factor(tavg_pdat$crop_var, 
                             levels = c('corn_rev', 'cotton_rev', 'hay_rev', 'wheat_rev', 'soybean_rev',
                                        'p_corn_a', 'p_cotton_a', 'p_hay_a', 'p_wheat_a', 'p_soybean_a'),
                             labels = c("Corn Rev.", "Cotton Rev.", "Hay Rev", "Wheat Rev.", "Soybean Rev.",
                                        "Corn Acres", "Cotton Acres", "Hay Acres", "Wheat Acres", "Soybean Acres"))
ggplot(tavg_pdat, aes(x = tavg, y = value, color = crop_var)) + 
  theme_tufte() +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  geom_line() +
  facet_wrap(~crop_var, scales = 'free', ncol = 5) +
  theme(legend.position = 'none') +
  ylab("Proportion of Acres              log(Revenue/Acre)") +
  xlab("Average Temperature") +
  scale_x_continuous(breaks = seq(10, 30, by = 4)) +
  NULL
ggsave("figures/bins_tavg.pdf", width = 6, height = 4)

# Degree Day 10_30 plot
dday10_30_pdat <- select(dday10_30_dat, dday10_30, corn_rev, cotton_rev, hay_rev, wheat_rev, soybean_rev,
                    p_corn_a, p_cotton_a, p_hay_a, p_wheat_a, p_soybean_a)
dday10_30_pdat <- gather(dday10_30_dat, key = crop_var, value = value, -dday10_30)
dday10_30_pdat$crop_var <- factor(dday10_30_pdat$crop_var, 
                             levels = c('corn_rev', 'cotton_rev', 'hay_rev', 'wheat_rev', 'soybean_rev',
                                        'p_corn_a', 'p_cotton_a', 'p_hay_a', 'p_wheat_a', 'p_soybean_a'),
                             labels = c("Corn Rev.", "Cotton Rev.", "Hay Rev", "Wheat Rev.", "Soybean Rev.",
                                        "Corn Acres", "Cotton Acres", "Hay Acres", "Wheat Acres", "Soybean Acres"))

ggplot(dday10_30_pdat, aes(x = dday10_30, y = value, color = crop_var)) + 
  theme_tufte() +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  geom_line() +
  facet_wrap(~crop_var, scales = 'free', ncol = 5) +
  theme(legend.position = 'none',
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("Proportion of Acres              log(Revenue/Acre)") +
  xlab("Degree Days (10-30C)") +
  scale_x_continuous(breaks = c(930, 1300, 1700, 2100, 2500, 2900)) +
  
  NULL
ggsave("figures/bins_dday10_30.pdf", width = 6, height = 4)

dday30_pdat <- gather(dday30_dat, key = crop_var, value = value, -dday30)
dday30_pdat$crop_var <- factor(dday30_pdat$crop_var, 
                             levels = c('corn_rev', 'cotton_rev', 'hay_rev', 'wheat_rev', 'soybean_rev',
                                        'p_corn_a', 'p_cotton_a', 'p_hay_a', 'p_wheat_a', 'p_soybean_a'),
                             labels = c("Corn Rev.", "Cotton Rev.", "Hay Rev", "Wheat Rev.", "Soybean Rev.",
                                        "Corn Acres", "Cotton Acres", "Hay Acres", "Wheat Acres", "Soybean Acres"))

ggplot(dday30_pdat, aes(x = dday30, y = value, color = crop_var)) + 
  theme_tufte() +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  geom_line() +
  facet_wrap(~crop_var, scales = 'free', ncol = 5) +
  theme(legend.position = 'none',
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("Proportion of Acres              log(Revenue/Acre)") +
  xlab("Degree Days (30C)") +
  scale_x_continuous(breaks = c(3, 30, 56, 82, 109, 135, 165)) +
  NULL

ggsave("figures/bins_dday30.pdf", width = 6, height = 4)

# Build tavg functions
tavg_corn_fn <- approxfun(locpoly(tavg_dat$tavg, tavg_dat$corn_rev, bandwidth = 5))
tavg_cotton_fn <- approxfun(locpoly(tavg_dat$tavg, tavg_dat$cotton_rev, bandwidth = 5))
tavg_hay_fn <- approxfun(locpoly(tavg_dat$tavg, tavg_dat$hay_rev, bandwidth = 5))
tavg_wheat_fn <- approxfun(locpoly(tavg_dat$tavg, tavg_dat$wheat_rev, bandwidth = 5))
tavg_soybean_fn <- approxfun(locpoly(tavg_dat$tavg, tavg_dat$soybean_rev, bandwidth = 5))

tavg <- tavg_dat$tavg
tavg[1] <- tavg_dat$tavg[1]+0.1; tavg[length(tavg_dat$tavg)] <- tavg[length(tavg_dat$tavg)] - 0.1

temp1 <- grad(tavg_corn_fn, tavg)*1*tavg_dat$corn_acres + 
          grad(tavg_cotton_fn, tavg)*1*tavg_dat$cotton_acres +
          grad(tavg_hay_fn, tavg)*1*tavg_dat$hay_acres +
          grad(tavg_wheat_fn, tavg)*1*tavg_dat$wheat_acres +
          grad(tavg_soybean_fn, tavg)*1*tavg_dat$soybean_acres

temp2 <- grad(tavg_corn_fn, tavg)*2*tavg_dat$corn_acres + 
          grad(tavg_cotton_fn, tavg)*2*tavg_dat$cotton_acres +
          grad(tavg_hay_fn, tavg)*2*tavg_dat$hay_acres +
          grad(tavg_wheat_fn, tavg)*2*tavg_dat$wheat_acres +
          grad(tavg_soybean_fn, tavg)*2*tavg_dat$soybean_acres

(sum(temp2) - sum(temp1))/sum(temp1)*100

corn1 <- sum(grad(tavg_corn_fn, tavg)*1*tavg_dat$corn_acres)
corn2 <- sum(grad(tavg_corn_fn, tavg)*2*tavg_dat$corn_acres)
corn3 <- sum(grad(tavg_corn_fn, tavg)*3*tavg_dat$corn_acres)
corn4 <- sum(grad(tavg_corn_fn, tavg)*4*tavg_dat$corn_acres)
corn5 <- sum(grad(tavg_corn_fn, tavg)*5*tavg_dat$corn_acres)

100*(corn2 - corn1)/corn1

#------------------------

corn_tavg_slopes <- slope(x1=tavg_dat$tavg, y1=tavg_dat$corn_rev, 
                     x2=lead(tavg_dat$tavg), y2=lead(tavg_dat$corn_rev))

corn_dday10_30_slopes <- slope(x1=dday10_30_dat$dday10_30, y1=dday10_30_dat$corn_rev, 
                     x2=lead(dday10_30_dat$dday10_30), y2=lead(dday10_30_dat$corn_rev))

corn_dday30_slopes <- slope(x1=dday30_dat$dday30, y1=dday30_dat$corn_rev, 
                     x2=lead(dday30_dat$dday30), y2=lead(dday30_dat$corn_rev))

corn_tavg_slopes <- rollmean(corn_tavg_slopes, 2, align = "left", na.rm = TRUE)
corn_dday10_30_slopes <- rollmean(corn_tavg_slopes, 2, align = "left")
corn_dday30_slopes <- rollmean(corn_tavg_slopes, 2, align = "left")


indat <- c()

corn1 <- sum(corn_tavg_slopes*1*tavg_dat$corn_acres[1:29], na.rm = TRUE)
corn2 <- sum(corn_tavg_slopes*2*tavg_dat$corn_acres[1:29], na.rm = TRUE)
corn3 <- sum(corn_tavg_slopes*3*tavg_dat$corn_acres[1:29], na.rm = TRUE)
corn4 <- sum(corn_tavg_slopes*4*tavg_dat$corn_acres[1:29], na.rm = TRUE)
corn5 <- sum(corn_tavg_slopes*5*tavg_dat$corn_acres[1:29], na.rm = TRUE)

(corn3 - corn1)/corn1

for (i in 1:5){
  
}

p1 <- ggplot(dplot, aes(x = x, y = y/1000000, color = crop)) + 
  theme_tufte() +    
  geom_line() + 
  #xlab('Average Temperature (C)') +
  xlab(NULL) + 
  ylab("Value of Activity \n ($1 million)") + 
  annotate("text", x = 12, y = 14, label = "Wheat", color = "blue", size = 4) +
  annotate("text", x = 16, y = 29, label = "Corn", color = "red", size = 4) +
  annotate("text", x = 21, y = 29, label = "Cotton", color = "green", size = 4) +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  theme(legend.position = 'top') + theme(
                         #legend.position = c(0.05, 0.9),
                         #legend.title = element_blank(),
                         #legend.background = element_rect(
                         #        size=0.5, linetype="solid", 
                         #\       colour = "grey"),
                         legend.position = "none",
                         axis.text.x = element_blank(),
                         #axis.text.y = element_blank(),
                         axis.ticks.x = element_blank(),
                         #axis.ticks.y = element_blank(),
                         #panel.border = element_rect(fill = NA),
                         plot.margin = unit(c(0, 0, 3, 0), "cm")) 
p1
dat$acres = rowSums(dat[, c("wheat_a", "corn_a", "cotton_a")], na.rm = TRUE)
p2 <- ggplot(dat, aes(tavg)) + 
  ylim(0, 0.18) +
  theme_tufte() +
  geom_density(aes(weight = acres/sum(acres)), color = "grey") +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  ylab("Density") +
  scale_x_continuous(breaks = 0:30) +
  xlab("Average Temperature(C)")
p2
ggdraw() + draw_plot(p1) + draw_plot(p2, 0, height = .30, width = 1.01)
ggsave("figures/empirical_graph.pdf", width = 6, height = 4)

