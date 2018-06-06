library(ggplot2)
library(tidyverse)

# download.file("https://spideroak.com/share/NJXWQ3TXN5XWI2LMNQ/crop-choice-data/run/media/john/1TB/SpiderOak/Projects/crop-choice-and-adaptation/data/full_ag_data.rds",
             # destfile = "/Users/john/full_ag_data.rds", method = "auto")
# dat <- readRDS("/Users/john/full_ag_data.rds")
# 
# plot(density(dat$tavg, weights = dat$wheat_mrev))
# plot(density(dat$tavg, weights = dat$corn_mrev))
# plot(density(dat$dday30, weights = dat$cotton_mrev))

# Estimate production fn's
x <- -40:40
x

wheat <- data.frame(y = -8*x^2 + 20000, temp = 1:81)
wheat <- wheat[40:81, ]
wheat$temp <- 0:41

x <- -40:40
corn <- data.frame(y = -8*x^2 + 19000, temp = 1:81)

x <- -40:40
cotton <- data.frame(y = -8*(x)^2 + 18000, temp = 1:81)

wheat$temp <- seq(0, 15, length.out = 42)
corn$temp <- seq(7, 26, length.out = 81)
cotton$temp <- seq(20, 35, length.out = 81)

ggplot(NULL) + geom_line(data = wheat, aes(temp, y), color = 'grey') +
  geom_line(data = corn, aes(temp, y), color = 'grey')  +
  geom_line(data = cotton, aes(temp, y), color = 'grey') +
  ylim(7200, 20000)+
  xlim(0, 35) + 
  theme_tufte(base_size = 12) +
  ylab("Value of Activity") +
  xlab("Temperature (C)") +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") 
