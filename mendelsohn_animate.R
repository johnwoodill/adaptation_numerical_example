library(ggplot2)
library(tidyverse)
library(ggthemes)
library(gganimate)
library(cowplot)

set.seed(123)
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


# Density fn
phi <- approxfun(density(rnorm(35, 15, 7)))

# With tangent lines
wtandat <- dat
wtandat$yy <- wheat_fn(wtandat$temp) - wheat_dt(wtandat$temp)*2
wtandat$xx <- wtandat$temp - 2
wtandat$yend <- wheat_fn(wtandat$temp) + wheat_dt(wtandat$temp)*2
wtandat$xend <- wtandat$temp + 2
wtandat$approx <- round((wheat_dt(wtandat$temp)*2) * phi(wtandat$temp), 2)
wtandat$real <- round((wheat_fn(wtandat$temp + 2) - wheat_fn(wtandat$temp))* phi(wtandat$temp), 2)
wtandat <- filter(wtandat, crop == "Wheat" & temp <= 9.4)

ctandat <- dat
ctandat$yy <- corn_fn(ctandat$temp) - corn_dt(ctandat$temp)*2
ctandat$xx <- ctandat$temp - 2
ctandat$yend <- corn_fn(ctandat$temp) + corn_dt(ctandat$temp)*2
ctandat$xend <- ctandat$temp + 2
ctandat$approx <- round((wheat_dt(ctandat$temp)*2)* phi(ctandat$temp), 2)
ctandat$real <- round((wheat_fn(ctandat$temp + 2) - wheat_fn(ctandat$temp)) * phi(ctandat$temp), 2)
ctandat <- filter(ctandat, crop == "Corn" & (temp >= 9.4 & temp <= 20.4))

cttandat <- dat
cttandat$yy <- cotton_fn(cttandat$temp) - cotton_dt(cttandat$temp)*2
cttandat$xx <- cttandat$temp - 2
cttandat$yend <- cotton_fn(cttandat$temp) + cotton_dt(cttandat$temp)*2
cttandat$xend <- cttandat$temp + 2
cttandat$approx <- round((wheat_dt(cttandat$temp)*2)* phi(cttandat$temp), 2)
cttandat$real <- round((wheat_fn(cttandat$temp + 2) - wheat_fn(cttandat$temp))* phi(cttandat$temp), 2)
cttandat <- filter(cttandat, crop == "Cotton" & temp >= 20.4)

tandat <- rbind(wtandat, ctandat, cttandat)
tandat$approx_cumsum <- cumsum(tandat$approx)
tandat$real_cumsum <- cumsum(tandat$real)
# tandat <- filter(tandat, crop == "Wheat")
# tandat$yy <- ifelse(tandat$yy >20000, 20000, tandat$yy)
# # tandat$yend <- ifelse(tandat$yend < 0, 0, tandat$yend)
# tandat$yend <- ifelse(tandat$yend > 20000, 20000, tandat$yend)

p <- ggplot(dat, aes(temp, y, color = crop)) + geom_line() + ylim(-8500, 22000) + xlim(-5, 45) +
  theme_tufte(base_size = 12) +
  ylab("Value of Activity") +
  xlab(NULL) +
  geom_point(data = tandat, aes(x=temp, y = y, frame = temp)) +
  geom_segment(data = tandat, aes(y=yy, x = xx, xend = xend, yend = yend, frame = temp), size = 1) +
  geom_text(data = tandat, aes(xend, yend, label = paste0("Approx: ", approx), frame = temp, color = NULL), hjust = -0.1, size = 3) +
  geom_text(data = tandat, aes(xend, yend, label = paste0("Real: ", real), frame = temp, color = NULL), hjust = 1.2, size = 3) +
  geom_text(data = tandat, aes(35, 20000, label = paste0("Sum Approx: ", approx_cumsum), color = NULL, frame = temp), size = 4) +
  geom_text(data = tandat, aes(35, 18000, label = paste0("Sum Real: ", real_cumsum), color = NULL, frame = temp), size = 4) +
  geom_text(data = tandat, aes(3, 21000, label = "Corn"), color = "red", size = 3) +
  geom_text(data = tandat, aes(18, 21000, label = "Cotton"), color = "green", size = 3) +
  geom_text(data = tandat, aes(27, 21000, label = "Cotton"), color = "blue", size = 3) +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  theme(legend.position = "none",
      legend.title = element_blank(),
      plot.margin = unit(c(0, 0, 3, 0), "cm"),
      axis.ticks.x = element_blank(),
      axis.text.x = element_blank())
p
densdat <- data.frame(x = rnorm(35, 15, 7))
p2 <- ggplot(densdat, aes(x)) + geom_density(fill = "grey") + xlim(-5, 45) +
  theme_tufte(base_size = 12) +
  ylab("Density") +
  xlab("Temperature (C)") +
    annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
    annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank())
  
p2

ggdraw() + draw_plot(p) + draw_plot(p2, 0.033, 0.0, height = 0.3)

gganimate(p, "output.gif", interval = 0.05)
