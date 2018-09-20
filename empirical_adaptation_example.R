library(tidyverse)
library(ggthemes)
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

dat <- dat %>% 
  group_by(fips) %>% 
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
            tavg = mean(tavg),
            dday10_30 = mean(dday10_30),
            dday30 = mean(dday30),
            prec = mean(prec),
            corn_a = mean(corn_a))

dat$corn_rev <- dat$corn_yield*mean(dat$corn_mprice)
dat$cotton_rev <- dat$cotton_yield*mean(dat$cotton_mprice)
dat$hay_rev <- dat$hay_yield*mean(dat$hay_mprice)
dat$wheat_rev <- dat$wheat_yield*mean(dat$wheat_mprice)
dat$soybean_rev <- dat$soybean_yield*mean(dat$soybean_mprice)

# dat$ftavg <- floor(dat$tavg)
dat$ftavg <- cut(dat$tavg, 30, labels = 1:30)
ddat <- dat %>% 
  group_by(ftavg) %>% 
  summarise(corn_rev = mean(corn_rev, na.rm = TRUE),
            cotton_rev = mean(cotton_rev, na.rm = TRUE),
            hay_rev = mean(hay_rev, na.rm = TRUE),
            wheat_rev = mean(wheat_rev, na.rm = TRUE),
            soybean_rev = mean(soybean_rev, na.rm = TRUE))

ddpdat <- gather(ddat, key = crop_rev, value = value, -ftavg)
ggplot(ddpdat, aes(x = as.numeric(as.character(ftavg)), y = log(1+value), color = crop_rev)) + 
  geom_line() +
  facet_wrap(~crop_rev) +
  theme(legend.position = 'none') +
  ylab("log(Revenue/Acre)") +
  xlab("Average Temperature") +
  NULL

# tavg
mod <- lm(log(1+corn_mrev) ~ rcs(tavg, 3), data = dat)
cornp <- predict(mod)
plot(x=dat$tavg, y=predict(mod))

cdat <- filter(dat, cotton_mrev >0)
mod <- lm(log(1+cotton_mrev) ~ rcs(tavg, 3), data = cdat)
cottp <- predict(mod)
lines(x=cdat$tavg, y=predict(mod))

mod <- lm(log(1+hay_mrev) ~ rcs(tavg, 3), data = dat)
hdat <- predict(mod)
lines(x=dat$tavg, y=predict(mod))

mod <- lm(log(1+wheat_mrev) ~ rcs(tavg, 3), data = dat)
wdat <- predict(mod)
lines(x=dat$tavg, y=predict(mod))

mod <- lm(log(1+soybean_mrev) ~ rcs(tavg, 3), data = dat)
sdat <- predict(mod)
lines(x=dat$tavg, y=predict(mod))

pdat <- data.frame(x = c(dat$tavg, cdat$tavg, dat$tavg, dat$tavg, dat$tavg),
                   y = c(cornp, cottp, hdat, wdat, sdat),
                   crop = c(rep("Corn", length(cornp)), rep("Cotton", length(cottp)),
                            rep("Hay", length(hdat)), rep("Wheat", length(wdat)),
                            rep("Soybean", length(sdat))))

ggplot(pdat, aes(x, y, color = crop)) + 
  geom_line() + 
  facet_wrap(~crop) +
  ylab("Revenue per acre") + ggtitle("log(Revenue/Acre) ~ tavg \n Restricted Cubic Spline df=3") +
  theme(legend.position = "none")
ggsave("figures/rcs_tavg.pdf", width = 6, height = 4)

# dday30
mod <- lm(log(1+corn_mrev) ~ rcs(dday30, 3), data = dat)
cornp <- predict(mod)
plot(x=dat$dday30, y=predict(mod))

cdat <- filter(dat, cotton_mrev >0)
mod <- lm(log(1+cotton_mrev) ~ rcs(dday30, 3), data = cdat)
cottp <- predict(mod)
plot(x=cdat$dday30, y=predict(mod))

mod <- lm(log(1+hay_mrev) ~ rcs(dday30, 3), data = dat)
hdat <- predict(mod)
plot(x=dat$dday30, y=predict(mod))

mod <- lm(log(1+wheat_mrev) ~ rcs(dday30, 3), data = dat)
wdat <- predict(mod)
plot(x=dat$dday30, y=predict(mod))

mod <- lm(log(1+soybean_mrev) ~ rcs(dday30, 3), data = dat)
sdat <- predict(mod)
plot(x=dat$dday30, y=predict(mod))

pdat <- data.frame(x = c(dat$dday30, cdat$dday30, dat$dday30, dat$dday30, dat$dday30),
                   y = c(cornp, cottp, hdat, wdat, sdat),
                   crop = c(rep("Corn", length(cornp)), rep("Cotton", length(cottp)),
                            rep("Hay", length(hdat)), rep("Wheat", length(wdat)),
                            rep("Soybean", length(sdat))))

ggplot(pdat, aes(x, y, color = crop)) + 
  geom_line() + 
  facet_wrap(~crop) +
  ylab("Revenue per acre") + ggtitle("log(Revenue/Acre) ~ Degree Day > 30 \n Restricted Cubic Spline df=3") +
  theme(legend.position = "none")

ggsave("figures/rcs_dday30.pdf", width = 6, height = 4)

# dday10_30
mod <- lm(log(1+corn_mrev) ~ rcs(dday10_30, 3), data = dat)
cornp <- predict(mod)
plot(x=dat$dday10_30, y=predict(mod))

cdat <- filter(dat, cotton_mrev >0)
mod <- lm(log(1+cotton_mrev) ~ rcs(dday10_30, 3), data = cdat)
cottp <- predict(mod)
plot(x=cdat$dday10_30, y=predict(mod))

mod <- lm(log(1+hay_mrev) ~ rcs(dday10_30, 3), data = dat)
hdat <- predict(mod)
plot(x=dat$dday10_30, y=predict(mod))

mod <- lm(log(1+wheat_mrev) ~ rcs(dday10_30, 3), data = dat)
wdat <- predict(mod)
plot(x=dat$dday10_30, y=predict(mod))

mod <- lm(log(1+soybean_mrev) ~ rcs(dday10_30, 3), data = dat)
sdat <- predict(mod)
plot(x=dat$dday10_30, y=predict(mod))

pdat <- data.frame(x = c(dat$dday10_30, cdat$dday10_30, dat$dday10_30, dat$dday10_30, dat$dday10_30),
                   y = c(cornp, cottp, hdat, wdat, sdat),
                   crop = c(rep("Corn", length(cornp)), rep("Cotton", length(cottp)),
                            rep("Hay", length(hdat)), rep("Wheat", length(wdat)),
                            rep("Soybean", length(sdat))))

ggplot(pdat, aes(x, y, color = crop)) + 
  geom_line() + 
  facet_wrap(~crop) +
  ylab("Revenue per acre") + 
  ggtitle("log(Revenue/Acre) ~ dday10_30 \n Restricted Cubic Spline df=3") +
  theme(legend.position = "none")

ggsave("figures/rcs_dday10_30.pdf", width = 6, height = 4)

corn_dens <- density(dat$tavg, weights = dat$corn_yield*dat$corn_a/sum(dat$corn_yield*dat$corn_a))
cotton_dens <- density(dat$tavg, weights = dat$cotton_yield*dat$cotton_a/sum(dat$cotton_yield*dat$cotton_a))
hay_dens <- density(dat$tavg, weights = dat$hay_yield*dat$hay_a/sum(dat$hay_yield*dat$hay_a))
soybean_dens <- density(dat$tavg, weights = dat$soybean_yield*dat$soybean_a/sum(dat$soybean_yield*dat$soybean_a))
wheat_dens <- density(dat$tavg, weights = dat$wheat_yield*dat$wheat_a/sum(dat$wheat_yield*dat$wheat_a))

# dplot <- data.frame(crop = rep(c("Corn", "Cotton", "Hay", "Wheat", "Soybean"), each = 512),
#                     x = c(corn_dens$x, cotton_dens$x, hay_dens$x, wheat_dens$x, soybean_dens$x),
#                     y = c(corn_dens$y*sum(dat$corn_mrev*dat$corn_yield), cotton_dens$y*sum(dat$cotton_mrev*dat$cotton_yield), 
#                           hay_dens$y*sum(dat$hay_mrev*dat$hay_yield), 10*wheat_dens$y*sum(dat$wheat_mrev*dat$wheat_yield), 
#                           soybean_dens$y*sum(dat$soybean_mrev*dat$soybean_yield)))

dplot <- data.frame(crop = rep(c("Corn", "Cotton", "Wheat"), each = 512),
                    x = c(corn_dens$x, cotton_dens$x, wheat_dens$x),
                    y = c(corn_dens$y*sum(dat$corn_mrev*dat$corn_yield), cotton_dens$y*sum(dat$cotton_mrev*dat$cotton_yield), 
                          10*wheat_dens$y*sum(dat$wheat_mrev*dat$wheat_yield)))

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

