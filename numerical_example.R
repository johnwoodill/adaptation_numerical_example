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
cotton <- function(x) return(-500*(x - 25))

# Corn adaptation calculation
for (i in unique(corn$temp)){
  delta_t <- corn_fn(i + 1) - corn_fn(i) 
  dydt <- corn_fn(i + 1)
  approx <- corn_dt(i) * (i + 1)
}
