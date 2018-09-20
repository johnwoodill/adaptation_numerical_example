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

# 1) take average yield over 10 years.
# 2) take average price over 10 years.
# 3) find average total acreage in all five crops over 10 years.

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

