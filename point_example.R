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

ggplot(dat, aes(temp, y, color = crop)) + geom_line() + ylim(0, 22000) +
  
  geom_point(aes(x = 5, y = 17500), color = "red") +
  geom_point(aes(x = 9.4, y = 11164), color = "red") +
  geom_point(aes(x = 13, y = 18000), color = "green") +
  geom_point(aes(x = 17, y = 18000), color = "green") +
  geom_point(aes(x = 22, y = 14750), color = "blue") +
  geom_point(aes(x = 27, y = 16000), color = "blue") +
  geom_point(aes(x = 30, y = 10750), color = "blue") +
  
  geom_point(aes(x = 6, y = 16400), color = "red") +
  geom_point(aes(x = 10.4, y = 13710), color = "red") +
  geom_point(aes(x = 14, y = 18750), color = "green") +
  geom_point(aes(x = 18, y = 16750), color = "green") +
  geom_point(aes(x = 23, y = 16000), color = "blue") +
  geom_point(aes(x = 28, y = 14750), color = "blue") +
  geom_point(aes(x = 31, y = 8000), color = "blue") +
  
  annotate("text", x = 5, y = 18500, label = "A") +
  annotate("text", x = 9.4, y = 12664, label = "B") +
  annotate("text", x = 13, y = 19500, label = "C") +
  annotate("text", x = 17, y = 19500, label = "D") +
  annotate("text", x = 22, y = 16000, label = "E") +
  annotate("text", x = 27, y = 17500, label = "F") +
  annotate("text", x = 30, y = 12000, label = "G") +
  
  annotate("text", x = 6, y = 18000, label = "A'") +
  annotate("text", x = 10.4, y = 15000, label = "B'") +
  annotate("text", x = 14, y = 20000, label = "C'") +
  annotate("text", x = 18, y = 18000, label = "D'") +
  annotate("text", x = 23, y = 17500, label = "E'") +
  annotate("text", x = 28, y = 16050, label = "F'") +
  annotate("text", x = 31, y = 9500, label = "G'") +
  
  annotate("text", x = 1, y = 22000, label = "Wheat", color = "red") +
  annotate("text", x = 13, y = 22000, label = "Corn", color = "green") +
  annotate("text", x = 25, y = 22000, label = "Cotton", color = "blue") +
  
  theme_tufte(base_size = 12) +
  ylab("Value of Activity") +
  xlab("Temperature (C)") +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  theme(legend.position = "none")

# Estimate production fn's
wheat_fn <- function(x) return(-100*(x)^2 + 20000)
corn_fn <- function(x) return(-250*(x - 15)^2 + 19000)
cotton_fn <- function(x) return(-250*(x - 25)^2 + 17000)

# Derivate
wheat_dt <- function(x) return(-200*x)
corn_dt <- function(x) return(-500*(x - 15))
cotton_dt <- function(x) return(-500*(x - 25))