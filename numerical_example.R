library(ggplot2)
library(tidyverse)
library(ggthemes)

# download.file("https://spideroak.com/share/NJXWQ3TXN5XWI2LMNQ/crop-choice-data/run/media/john/1TB/SpiderOak/Projects/crop-choice-and-adaptation/data/full_ag_data.rds",
             # destfile = "/Users/john/full_ag_data.rds", method = "auto")
# dat <- readRDS("/Users/john/full_ag_data.rds")
# 
# plot(density(dat$tavg, weights = dat$wheat_mrev))
# plot(density(dat$tavg, weights = dat$corn_mrev))
# plot(density(dat$dday30, weights = dat$cotton_mrev))

# Mendelsohn Plot
x <- seq(0, 20, 0.1)
wheat <- data.frame(y = -100*(x)^2 + 20000, temp = seq(0, 20, 0.1), crop = "Wheat")
plot(wheat$temp, wheat$y)

x <- seq(6, 27, 0.1)
corn <- data.frame(y = -250*(x - 15)^2 + 19000, temp = seq(6, 27, 0.1), crop = "Corn")
plot(corn$temp, corn$y)

x <- seq(15, 35, 0.1)
cotton <- data.frame(y = -250*(x - 25)^2 + 17000, temp = seq(15, 35, 0.1), crop = "Cotton")

dat <- rbind(wheat, corn, cotton)
dat <- filter(dat, y > 0)

ggplot(dat, aes(temp, y, color = crop)) + geom_line() + ylim(0, 20000) +
  theme_tufte(base_size = 12) +
  ylab("Value of Activity") +
  xlab("Temperature (C)") +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") 

# Estimate production fn's
wheat_fn <- function(x) return(-100*(x)^2 + 20000)
corn_fn <- function(x) return(-250*(x - 15)^2 + 19000)
cotton_fn <- function(x) return(-250*(x - 25)^2 + 17000)

# Derivate
wheat_dt <- function(x) return(-200*x)
corn_dt <- function(x) return(-500*(x - 15))
cotton_dt <- function(x) return(-500*(x - 25))

# Calculate Negative Adaptation
wheat_outdat <- data.frame()
# Wheat adaptation calculation
for (i in seq(0, 35, 0.10)){
  delta_t <- wheat_fn(i + 1) - wheat_fn(i) 
  dydt_1c <- wheat_fn(i + 1)
  dydt_2c <- wheat_fn(i + 2)
  dydt_3c <- wheat_fn(i + 3)
  dydt_4c <- wheat_fn(i + 4)
  dydt_5c <- wheat_fn(i + 5)
  approx1c <- wheat_fn(i) + wheat_dt(i)
  approx2c <- wheat_fn(i) + wheat_dt(i)*2
  approx3c <- wheat_fn(i) + wheat_dt(i)*3
  approx4c <- wheat_fn(i) + wheat_dt(i)*4
  approx5c <- wheat_fn(i) + wheat_dt(i)*5
  adaptation <- dydt - approx
  indat <- data.frame(crop = "wheat", 
                       temp = i,
                       y = wheat_fn(i),
                       delta_t = delta_t,
                       dydt = dydt,
                       dydt_1c = dydt_1c,
                       dydt_2c = dydt_2c,
                       dydt_3c = dydt_3c,
                       dydt_4c = dydt_4c,
                       dydt_5c = dydt_5c,
                       approx1c = approx1c,
                       approx2c = approx2c,
                       approx3c = approx3c,
                       approx4c = approx4c,
                       approx5c = approx5c,
                       adaptation = adaptation)
  wheat_outdat <- rbind(wheat_outdat, indat)
}
wheat_outdat


corn_outdat <- data.frame()
# Corn adaptation calculation
for (i in seq(0, 35, 0.10)){
  delta_t <- corn_fn(i + 1) - corn_fn(i) 
  dydt_1c <- corn_fn(i + 1)
  dydt_2c <- corn_fn(i + 2)
  dydt_3c <- corn_fn(i + 3)
  dydt_4c <- corn_fn(i + 4)
  dydt_5c <- corn_fn(i + 5)
  approx1c <- corn_fn(i) + corn_dt(i)
  approx2c <- corn_fn(i) + corn_dt(i)*2
  approx3c <- corn_fn(i) + corn_dt(i)*3
  approx4c <- corn_fn(i) + corn_dt(i)*4
  approx5c <- corn_fn(i) + corn_dt(i)*5
  adaptation <- dydt - approx
  indat <- data.frame(crop = "corn", 
                       temp = i,
                       y = corn_fn(i),
                       delta_t = delta_t,
                       dydt = dydt,
                       dydt_1c = dydt_1c,
                       dydt_2c = dydt_2c,
                       dydt_3c = dydt_3c,
                       dydt_4c = dydt_4c,
                       dydt_5c = dydt_5c,
                       approx1c = approx1c,
                       approx2c = approx2c,
                       approx3c = approx3c,
                       approx4c = approx4c,
                       approx5c = approx5c,
                       adaptation = adaptation)
  corn_outdat <- rbind(corn_outdat, indat)
}
corn_outdat

cotton_outdat <- data.frame()
# Cotton adaptation calculation
for (i in seq(0, 35, 0.10)){
  delta_t <- cotton_fn(i + 1) - cotton_fn(i) 
  dydt_1c <- cotton_fn(i + 1)
  dydt_2c <- cotton_fn(i + 2)
  dydt_3c <- cotton_fn(i + 3)
  dydt_4c <- cotton_fn(i + 4)
  dydt_5c <- cotton_fn(i + 5)
  approx1c <- cotton_fn(i) + cotton_dt(i)
  approx2c <- cotton_fn(i) + cotton_dt(i)*2
  approx3c <- cotton_fn(i) + cotton_dt(i)*3
  approx4c <- cotton_fn(i) + cotton_dt(i)*4
  approx5c <- cotton_fn(i) + cotton_dt(i)*5
  adaptation <- dydt - approx
  indat <- data.frame(crop = "cotton", 
                       temp = i,
                       y = cotton_fn(i),
                       delta_t = delta_t,
                       dydt = dydt,
                       dydt_1c = dydt_1c,
                       dydt_2c = dydt_2c,
                       dydt_3c = dydt_3c,
                       dydt_4c = dydt_4c,
                       dydt_5c = dydt_5c,
                       approx1c = approx1c,
                       approx2c = approx2c,
                       approx3c = approx3c,
                       approx4c = approx4c,
                       approx5c = approx5c,
                       adaptation = adaptation)
  cotton_outdat <- rbind(cotton_outdat, indat)
}
cotton_outdat

# Calculate Positive Adaptation
pwheat <- data.frame(temp = wheat_outdat$temp,
                    wheat_y = wheat_outdat$y,
                    wheat_dydt_1c = wheat_outdat$dydt_1c,
                    wheat_dydt_2c = wheat_outdat$dydt_2c,
                    wheat_dydt_3c = wheat_outdat$dydt_3c,
                    wheat_dydt_4c = wheat_outdat$dydt_4c,
                    wheat_dydt_5c = wheat_outdat$dydt_5c,
                    wheat_approx1c = wheat_outdat$approx1c,
                    wheat_approx2c = wheat_outdat$approx2c,
                    wheat_approx3c = wheat_outdat$approx3c,
                    wheat_approx4c = wheat_outdat$approx4c,
                    wheat_approx5c = wheat_outdat$approx5c)

pcorn <- data.frame(temp = corn_outdat$temp,
                     corn_y = corn_outdat$y,
                    corn_dydt_1c = corn_outdat$dydt_1c,
                    corn_dydt_2c = corn_outdat$dydt_2c,
                    corn_dydt_3c = corn_outdat$dydt_3c,
                    corn_dydt_4c = corn_outdat$dydt_4c,
                    corn_dydt_5c = corn_outdat$dydt_5c,
                    corn_approx1c = corn_outdat$approx1c,
                    corn_approx2c = corn_outdat$approx2c,
                    corn_approx3c = corn_outdat$approx3c,
                    corn_approx4c = corn_outdat$approx4c,
                    corn_approx5c = corn_outdat$approx5c)

pcotton <- data.frame(temp = cotton_outdat$temp,
                     cotton_y = cotton_outdat$y,
                    cotton_dydt_1c = cotton_outdat$dydt_1c,
                    cotton_dydt_2c = cotton_outdat$dydt_2c,
                    cotton_dydt_3c = cotton_outdat$dydt_3c,
                    cotton_dydt_4c = cotton_outdat$dydt_4c,
                    cotton_dydt_5c = cotton_outdat$dydt_5c,
                    cotton_approx1c = cotton_outdat$approx1c,
                    cotton_approx2c = cotton_outdat$approx2c,
                    cotton_approx3c = cotton_outdat$approx3c,
                    cotton_approx4c = cotton_outdat$approx4c,
                    cotton_approx5c = cotton_outdat$approx5c)

pdat <- data.frame(temp = seq(0, 35, 0.10))
pdat <- left_join(pdat, pwheat, by = "temp")
pdat <- left_join(pdat, pcorn, by = "temp")
pdat <- left_join(pdat, pcotton, by = "temp")

# With adaptation
pdat1 <- filter(pdat, temp < 8.4)
pdat2 <- filter(pdat, temp > 8.4 & temp < 19.2)
pdat3 <- filter(pdat, temp > 19.2)

base_adaptation <- (sum(pdat1$wheat_y) + sum(pdat2$corn_y) + sum(pdat3$cotton_y))
adaptation_1c <- (sum(pdat1$wheat_dydt_1c) + sum(pdat2$corn_dydt_1c) + sum(pdat3$cotton_dydt_1c))
adaptation_2c <- (sum(pdat1$wheat_dydt_2c) + sum(pdat2$corn_dydt_2c) + sum(pdat3$cotton_dydt_2c))
adaptation_3c <- (sum(pdat1$wheat_dydt_3c) + sum(pdat2$corn_dydt_3c) + sum(pdat3$cotton_dydt_3c))
adaptation_4c <- (sum(pdat1$wheat_dydt_4c) + sum(pdat2$corn_dydt_4c) + sum(pdat3$cotton_dydt_4c))
adaptation_5c <- (sum(pdat1$wheat_dydt_5c) + sum(pdat2$corn_dydt_5c) + sum(pdat3$cotton_dydt_5c))

approx_adaptation_1C <- (sum(pdat1$wheat_approx1c) + sum(pdat2$corn_approx1c) + sum(pdat3$cotton_approx1c))
approx_adaptation_2C <- (sum(pdat1$wheat_approx2c) + sum(pdat2$corn_approx2c) + sum(pdat3$cotton_approx2c))
approx_adaptation_3C <- (sum(pdat1$wheat_approx3c) + sum(pdat2$corn_approx3c) + sum(pdat3$cotton_approx3c))
approx_adaptation_4C <- (sum(pdat1$wheat_approx4c) + sum(pdat2$corn_approx4c) + sum(pdat3$cotton_approx4c))
approx_adaptation_5C <- (sum(pdat1$wheat_approx5c) + sum(pdat2$corn_approx5c) + sum(pdat3$cotton_approx5c))

# Without adaptation
pdat11 <- filter(pdat, wheat_y > 10000)
pdat22 <- filter(pdat, corn_y > 10000)
pdat33 <- filter(pdat, cotton_y > 10000)


base_wo_adaptation <- sum(pdat11$wheat_y) + sum(pdat22$corn_y) + sum(pdat33$cotton_y)
wo_adaptation_1c <- (sum(pdat11[which(pdat11$wheat_dydt_1c > 0), "wheat_dydt_1c"]) + sum(pdat22[which(pdat22$corn_dydt_1c > 0), "corn_dydt_1c"]) + sum(pdat33[which(pdat33$cotton_dydt_1c > 0), "cotton_dydt_1c"]))
wo_adaptation_2c <- (sum(pdat11[which(pdat11$wheat_dydt_2c > 0), "wheat_dydt_2c"]) + sum(pdat22[which(pdat22$corn_dydt_2c > 0), "corn_dydt_2c"]) + sum(pdat33[which(pdat33$cotton_dydt_2c > 0), "cotton_dydt_2c"]))
wo_adaptation_3c <- (sum(pdat11[which(pdat11$wheat_dydt_3c > 0), "wheat_dydt_3c"]) + sum(pdat22[which(pdat22$corn_dydt_3c > 0), "corn_dydt_3c"]) + sum(pdat33[which(pdat33$cotton_dydt_3c > 0), "cotton_dydt_3c"]))
wo_adaptation_4c <- (sum(pdat11[which(pdat11$wheat_dydt_4c > 0), "wheat_dydt_4c"]) + sum(pdat22[which(pdat22$corn_dydt_4c > 0), "corn_dydt_4c"]) + sum(pdat33[which(pdat33$cotton_dydt_4c > 0), "cotton_dydt_4c"]))
wo_adaptation_5c <- (sum(pdat11[which(pdat11$wheat_dydt_5c > 0), "wheat_dydt_5c"]) + sum(pdat22[which(pdat22$corn_dydt_5c > 0), "corn_dydt_5c"]) + sum(pdat33[which(pdat33$cotton_dydt_5c > 0), "cotton_dydt_5c"]))

wo_approx_adaptation_1c <- (sum(pdat11[which(pdat11$wheat_approx1c > 0), "wheat_approx1c"]) + sum(pdat22[which(pdat22$corn_approx1c > 0), "corn_approx1c"]) + sum(pdat33[which(pdat33$cotton_approx1c > 0), "cotton_approx1c"]))
wo_approx_adaptation_2c <- (sum(pdat11[which(pdat11$wheat_approx2c > 0), "wheat_approx2c"]) + sum(pdat22[which(pdat22$corn_approx2c > 0), "corn_approx2c"]) + sum(pdat33[which(pdat33$cotton_approx2c > 0), "cotton_approx2c"]))
wo_approx_adaptation_3c <- (sum(pdat11[which(pdat11$wheat_approx3c > 0), "wheat_approx3c"]) + sum(pdat22[which(pdat22$corn_approx3c > 0), "corn_approx3c"]) + sum(pdat33[which(pdat33$cotton_approx3c > 0), "cotton_approx3c"]))
wo_approx_adaptation_4c <- (sum(pdat11[which(pdat11$wheat_approx4c > 0), "wheat_approx4c"]) + sum(pdat22[which(pdat22$corn_approx4c > 0), "corn_approx4c"]) + sum(pdat33[which(pdat33$cotton_approx4c > 0), "cotton_approx4c"]))
wo_approx_adaptation_5c <- (sum(pdat11[which(pdat11$wheat_approx5c > 0), "wheat_approx5c"]) + sum(pdat22[which(pdat22$corn_approx5c > 0), "corn_approx5c"]) + sum(pdat33[which(pdat33$cotton_approx5c > 0), "cotton_approx5c"]))

# Build main plot
gdat <- data.frame(temp = 0:5,
                   adaptation = c(base_adaptation, adaptation_1c, adaptation_2c, adaptation_3c, adaptation_4c, adaptation_5c),
                   approx_adaptation = c(base_adaptation, approx_adaptation_1C, approx_adaptation_2C, approx_adaptation_3C, approx_adaptation_4C, approx_adaptation_5C),
                   wo_adaptation = c(base_wo_adaptation, wo_adaptation_1c, wo_adaptation_2c, wo_adaptation_3c, wo_adaptation_4c, wo_adaptation_5c),
                   wo_approx_adaptation = c(base_wo_adaptation, wo_approx_adaptation_1c, wo_approx_adaptation_2c, wo_approx_adaptation_3c, wo_approx_adaptation_4c, wo_approx_adaptation_5c))
gdat$adaptation_change <- (gdat$adaptation - first(gdat$adaptation))/first(gdat$adaptation)
gdat$approx_adaptation_change <- (gdat$approx_adaptation - first(gdat$approx_adaptation))/first(gdat$approx_adaptation)
gdat$wo_adaptation_change <- (gdat$wo_adaptation - first(gdat$wo_adaptation))/first(gdat$wo_adaptation)
gdat$wo_approx_adaptation_change <- (gdat$wo_approx_adaptation - first(gdat$wo_approx_adaptation))/first(gdat$wo_approx_adaptation)
gdat
