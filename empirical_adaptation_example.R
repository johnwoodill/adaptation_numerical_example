library(tidyverse)
library(cowplot)

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

1) take average yield over 10 years.
2) take average price over 10 years.
3) find average total acreage in all five crops over 10 years.

# dat <- filter(dat, corn_mrev > 0 & !is.na(corn_mrev) & !is.infinite(corn_mrev))
# dat <- filter(dat, cotton_mrev > 0 & !is.na(cotton_mrev) & !is.infinite(cotton_mrev))
# dat <- filter(dat, hay_mrev > 0 & !is.na(hay_mrev) & !is.infinite(hay_mrev))
# dat <- filter(dat, wheat_mrev > 0 & !is.na(wheat_mrev) & !is.infinite(wheat_mrev))
# dat <- filter(dat, soybean_mrev > 0 & !is.na(soybean_mrev) & !is.infinite(soybean_mrev))
# 
# dat <- dat %>% filter(corn_mrev < quantile(dat$corn_mrev, 0.95) & corn_mrev > quantile(dat$corn_mrev, 0.05))
# dat <- dat %>% filter(cotton_mrev < quantile(dat$cotton_mrev, 0.95) & cotton_mrev > quantile(dat$cotton_mrev, 0.05))
# dat <- dat %>% filter(hay_mrev < quantile(dat$hay_mrev, 0.95) & hay_mrev > quantile(dat$hay_mrev, 0.05))
# dat <- dat %>% filter(wheat_mrev < quantile(dat$wheat_mrev, 0.95) & wheat_mrev > quantile(dat$wheat_mrev, 0.05))
# dat <- dat %>% filter(soybean_mrev < quantile(dat$soybean_mrev, 0.95) & soybean_mrev > quantile(dat$soybean_mrev, 0.05))


dat$five <- dat$year -dat$year %% 5

dat <- dat %>% 
  group_by(ten, fips) %>% 
  summarise(corn_yield = mean(corn_yield),
            corn_mprice = mean(corn_rprice),
            corn_a = mean(corn_grain_a),
            cotton_yield = mean(cotton_yield),
            cotton_mprice = mean(cotton_rprice),
            cotton_a = mean(cotton_a),
            hay_yield = mean(hay_yield),
            hay_mprice = mean(hay_rprice),
            hay_a = mean(hay_a),
            soybean_yield = mean(soybean_yield),
            soybean_mprice = mean(soybean_rprice),
            soybean_a = mean(soybean_a),
            wheat_yield = mean(wheat_yield),
            wheat_mprice = mean(wheat_rprice),
            wheat_a = mean(wheat_a),
            corn_mrev = mean(corn_mrev),
            cotton_mrev = mean(cotton_mrev),
            hay_mrev = mean(hay_mrev),
            wheat_mrev = mean(wheat_mrev),
            soybean_mrev = mean(soybean_mrev),
            tavg = mean(tavg))

corn_dens <- density(dat$tavg, weights = dat$corn_yield*dat$corn_a/sum(dat$corn_yield*dat$corn_a))
cotton_dens <- density(dat$tavg, weights = dat$cotton_yield*dat$cotton_a/sum(dat$cotton_yield*dat$cotton_a))
hay_dens <- density(dat$tavg, weights = dat$hay_yield*dat$hay_a/sum(dat$hay_yield*dat$hay_a))
soybean_dens <- density(dat$tavg, weights = dat$soybean_yield*dat$soybean_a/sum(dat$soybean_yield*dat$soybean_a))
wheat_dens <- density(dat$tavg, weights = dat$wheat_yield*dat$wheat_a/sum(dat$wheat_yield*dat$wheat_a))


dplot <- data.frame(crop = rep(c("Corn", "Cotton", "Hay", "Wheat", "Soybean"), each = 512),
                    x = c(corn_dens$x, cotton_dens$x, hay_dens$x, wheat_dens$x, soybean_dens$x),
                    y = c(corn_dens$y*sum(dat$corn_mrev*dat$corn_yield), cotton_dens$y*sum(dat$cotton_mrev*dat$cotton_yield), 
                          hay_dens$y*sum(dat$hay_mrev*dat$hay_yield), wheat_dens$y*sum(dat$wheat_mrev*dat$wheat_yield), 
                          soybean_dens$y*sum(dat$soybean_mrev*dat$soybean_yield)))


p1 <- ggplot(dplot, aes(x = x, y = y, color = crop)) + 
  geom_line() + xlab('Average Temperature (C)') +
  ylab("Value of Activity \n (yield * avg price)") + 
  theme(legend.position = 'top')
p1
p2 <- ggplot(dat, aes(tavg)) + 
  geom_density() + 
  xlab("Average Temperature(C) - phi(c)")

plot_grid(p1, p2, ncol = 1)


plot(corn_dens)
lines(cotton_dens)
lines(hay_dens)
lines(soybean_dens)
lines(wheat_dens)

plot(wheat_dens)
lines(soybean_dens)
lines(hay_dens)


#---------------------------------------------------------
# dat$corn_mrev <- remove_outliers(dat$corn_mrev)
# dat$cotton_mrev <- remove_outliers(dat$cotton_mrev)
# dat$hay_mrev <- remove_outliers(dat$hay_mrev)
# dat$wheat_mrev <- remove_outliers(dat$wheat_mrev)
# dat$soybean_mrev <- remove_outliers(dat$soybean_mrev)
# 
# # dat <- filter(dat, tavg > 10 & tavg < 25)
# dat <- filter(dat, corn_mrev > 0 & !is.na(corn_mrev) & !is.infinite(corn_mrev))
# dat <- filter(dat, cotton_mrev > 0 & !is.na(cotton_mrev) & !is.infinite(cotton_mrev))
# dat <- filter(dat, hay_mrev > 0 & !is.na(hay_mrev) & !is.infinite(hay_mrev))
# dat <- filter(dat, wheat_mrev > 0 & !is.na(wheat_mrev) & !is.infinite(wheat_mrev))
# dat <- filter(dat, soybean_mrev > 0 & !is.na(soybean_mrev) & !is.infinite(soybean_mrev))

dat$corn_mrev <- dat$corn_grain_p

dat$corn_mrev <- dat$corn_mrev*dat$corn_a
dat$coton_mrev <- dat$coton_mrev*dat$coton_a
dat$hay_mrev <- dat$hay_mrev*dat$hay_a
dat$wheat_mrev <- dat$wheat_mrev*dat$wheat_a
dat$soybean_mrev <- dat$soybean_mrev*dat$soybean_a

# dat <- dat %>% filter(corn_mrev < quantile(dat$corn_mrev, 0.95) & corn_mrev > quantile(dat$corn_mrev, 0.05))
# dat <- dat %>% filter(cotton_mrev < quantile(dat$cotton_mrev, 0.95) & cotton_mrev > quantile(dat$cotton_mrev, 0.05))
# dat <- dat %>% filter(hay_mrev < quantile(dat$hay_mrev, 0.95) & hay_mrev > quantile(dat$hay_mrev, 0.05))
# dat <- dat %>% filter(wheat_mrev < quantile(dat$wheat_mrev, 0.95) & wheat_mrev > quantile(dat$wheat_mrev, 0.05))
# dat <- dat %>% filter(soybean_mrev < quantile(dat$soybean_mrev, 0.95) & soybean_mrev > quantile(dat$soybean_mrev, 0.05))

dat <- dat %>% 
  group_by(fips) %>% 
  summarise(corn_mrev = mean(corn_mrev, na.rm = TRUE),
            cotton_mrev = mean(cotton_mrev, na.rm = TRUE),
            hay_mrev = mean(hay_mrev, na.rm = TRUE),
            wheat_mrev = mean(wheat_mrev, na.rm = TRUE),
            soybean_mrev = mean(soybean_mrev, na.rm = TRUE),
            corn_a = mean(corn_grain_a, na.rm = TRUE),
            cotton_a = mean(cotton_a, na.rm = TRUE),
            hay_a = mean(hay_a, na.rm = TRUE),
            wheat_a = mean(wheat_a, na.rm = TRUE),
            soybean_a = mean(soybean_a, na.rm = TRUE),
            tavg = mean(tavg, na.rm = TRUE),
            dday0_10 = mean(dday0_10, na.rm = TRUE),
            dday10_30 = mean(dday10_30, na.rm = TRUE),
            dday30 = mean(dday30, na.rm = TRUE))



corn_dens <- density(dat$dday10_30 ,weights = dat$corn_mrev/sum(dat$corn_mrev))
cotton_dens <- density(dat$dday10_30 ,weights = dat$cotton_mrev/sum(dat$cotton_mrev))
hay_dens <- density(dat$dday10_30 ,weights = dat$hay_mrev/sum(dat$hay_mrev))
wheat_dens <- density(dat$dday10_30 ,weights = dat$wheat_mrev/sum(dat$wheat_mrev))
soybean_dens <- density(dat$dday10_30 ,weights = dat$soybean_mrev/sum(dat$soybean_mrev))

corn_dens <- density(filter(dat, corn_mrev > 0)$dday10_30)
cotton_dens <- density(filter(dat, cotton_mrev > 0)$dday10_30)
hay_dens <- density(filter(dat, hay_mrev > 0)$dday10_30)
wheat_dens <- density(filter(dat, wheat_mrev > 0)$dday10_30)
soybean_dens <- density(filter(dat, soybean_mrev > 0)$dday10_30)

dplot <- data.frame(crop = rep(c("Corn", "Cotton", "Hay", "Wheat", "Soybean"), each = 512),
                    x = c(corn_dens$x, cotton_dens$x, hay_dens$x, wheat_dens$x, soybean_dens$x),
                    y = c(corn_dens$y*sum(dat$corn_mrev), cotton_dens$y*sum(dat$cotton_mrev), 
                          hay_dens$y*sum(dat$hay_mrev), wheat_dens$y*sum(dat$wheat_mrev), 
                          soybean_dens$y*sum(dat$soybean_mrev)))


p1 <- ggplot(dplot, aes(x = x, y = y, color = crop)) + 
  geom_line() + xlab('Average Temperature (C)') +
  ylab("Value of Activity \n (yield * avg price)") + 
  theme(legend.position = 'top')

p2 <- ggplot(dat, aes(dday10_30)) + 
  geom_density() + 
  xlab("Average Temperature(C) - phi(c)")

plot_grid(p1, p2, ncol = 1)
