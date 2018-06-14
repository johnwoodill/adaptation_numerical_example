library(ggplot2)
library(tidyverse)
library(ggthemes)
library(gganimate)

set.seed(123)
# download.file("https://spideroak.com/share/NJXWQ3TXN5XWI2LMNQ/crop-choice-data/run/media/john/1TB/SpiderOak/Projects/crop-choice-and-adaptation/data/full_ag_data.rds",
             # destfile = "/Users/john/full_ag_data.rds", method = "auto")
# dat <- readRDS("/Users/john/full_ag_data.rds")
# 
# plot(density(dat$tavg, weights = dat$wheat_mrev))
# plot(density(dat$tavg, weights = dat$corn_mrev))
# plot(density(dat$dday30, weights = dat$cotton_mrev))


# Set Density function
# Density fn
phi <- approxfun(density(rnorm(35, 15, 7)))
# phi <- approxfun(runif(35, 0))

mendelsohn_fn <- function(density_fn){
  phi <- density_fn

# Mendelsohn Plot
x <- seq(0, 20, 0.1)
wheat <- data.frame(y = -250*(x)^2 + 20000, temp = seq(0, 20, 0.1), crop = "Wheat")
# plot(wheat$temp, wheat$y)

x <- seq(6, 27, 0.1)
corn <- data.frame(y = -250*(x - 15)^2 + 19000, temp = seq(6, 27, 0.1), crop = "Corn")
# plot(corn$temp, corn$y)

x <- seq(15, 35, 0.1)
cotton <- data.frame(y = -250*(x - 25)^2 + 17000, temp = seq(15, 35, 0.1), crop = "Cotton")

dat <- rbind(wheat, corn, cotton)
dat <- filter(dat, y > 0)

ggplot(dat, aes(temp, y, color = crop)) + geom_line() + ylim(0, 20000) +
  theme_tufte(base_size = 12) +
  ylab("Value of Activity") +
  xlab("Temperature (C)") +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  geom_point(aes(9.4, wheat_fn(9.4))) +
  geom_point(aes(20.4, corn_fn(20.4))) +
  geom_point(aes(14.1, wheat_fn(14.1))) +
  geom_point(aes(23.7, corn_fn(23.7))) +
  geom_point(aes(6.3, corn_fn(6.3))) +
  geom_point(aes(16.8, cotton_fn(16.8))) +
  geom_point(aes(33.2, cotton_fn(33.2)))

 # Estimate production fn's
wheat_fn <- function(x) return(-100*(x)^2 + 20000)
corn_fn <- function(x) return(-250*(x - 15)^2 + 19000)
cotton_fn <- function(x) return(-250*(x - 25)^2 + 17000)

# Derivate
wheat_dt <- function(x) return(-200*x)
corn_dt <- function(x) return(-500*(x - 15))
cotton_dt <- function(x) return(-500*(x - 25))

# # Density fn
# phi <- approxfun(density(rnorm(35, 15, 7)))
# phi <- approxfun(runif(35, 0))

# Real Integration fn
# int_wheatfn <- function(x) -100*(x + 5)^2 + 20000 + ((-100*(x + 5)^2 + 20000) - (-100*(x)^2 + 20000)) * phi(x)
# int_cornfn <- function(x) (corn_fn(x + 2) - corn_fn(x)) * phi(x)
# int_cottonfn <- function(x) (cotton_fn(x + 2) - cotton_fn(x)) * phi(x)
# 
# 
# # Approximate Integration fn
# approxint_wheatfn <- function(x) (-200*x * 5) * phi(x)
# approxint_cornfn <- function(x) (corn_dt(x) * 2) * phi(x)
# approxint_cottonfn <- function(x) (cotton_dt(x) * 2) * phi(x)

# integrate(int_wheatfn, 0, 9.4)[[1]] - integrate(approxint_wheatfn, 0, 9.4)[[1]]

# # Marginal Effect
# integrate(approxint_wheatfn, 0.1, 9.4)[[1]] + integrate(approxint_cornfn, 9.5, 20.4)[[1]] + integrate(approxint_cottonfn, 20.5, 35)[[1]]
# 
# # Real Effect
# integrate(int_wheatfn, 0.1, 9.4)[[1]] + integrate(int_cornfn, 9.5, 20.4)[[1]] + integrate(int_cottonfn, 20.5, 35)[[1]]

outdat <- data.frame()
for (i in 0:5){
# With tangent lines
  wtandat <- data.frame(temp = seq(0, 14.1, 0.1))
  wtandat$wheat_approx <- round(wheat_fn(wtandat$temp) + (wheat_dt(wtandat$temp)*i) * phi(wtandat$temp), 2)
  wheat_real <- data.frame(temp = wtandat$temp, 
                           real = wheat_fn(wtandat$temp) + round(((wheat_fn(wtandat$temp + i) - wheat_fn(wtandat$temp)) * phi(wtandat$temp)), 2))
  wheat_real$real <- ifelse(wheat_real$real < 0, 0, wheat_real$real)
  wheat_real <- filter(wheat_real, temp <= 9.4)
  wtandat <- filter(wtandat, temp <= 14.1)
  wtandat$wheat_approx <- ifelse(wtandat$wheat_approx < 0, 0, wtandat$wheat_approx)
  wtandat$temp <- as.character(wtandat$temp)
  wheat_real$temp <- as.character(wheat_real$temp)
  
  ctandat <- data.frame(temp = seq(6.3, 23.7, 0.1))
  ctandat$corn_approx <- round(corn_fn(ctandat$temp) + (corn_dt(ctandat$temp)*i) * phi(ctandat$temp), 2)
  corn_real <- data.frame(temp = ctandat$temp, 
                           real = corn_fn(ctandat$temp)  + round(((corn_fn(ctandat$temp + i) - corn_fn(ctandat$temp)) * phi(ctandat$temp)), 2))
  corn_real$real <- ifelse(corn_real$real < 0, 0, corn_real$real)
  corn_real <- filter(corn_real, (temp >= 9.5 & temp <= 20.4))
  ctandat <- filter(ctandat, (temp >= 6.3 & temp <= 23.7))
  ctandat$corn_approx <- ifelse(ctandat$corn_approx < 0, 0, ctandat$corn_approx)
  ctandat$temp <- as.character(ctandat$temp)
  
  cttandat <- data.frame(temp = seq(16.8, 33.2, 0.1))
  cttandat$cotton_approx <- round(cotton_fn(cttandat$temp) + (cotton_dt(cttandat$temp)*i) * phi(cttandat$temp), 2)
  cotton_real <- data.frame(temp = cttandat$temp, 
                           real = cotton_fn(cttandat$temp)  + round(((cotton_fn(cttandat$temp + i) - cotton_fn(cttandat$temp)) * phi(cttandat$temp)), 2))
  cotton_real$real <- ifelse(cotton_real$real < 0, 0, cotton_real$real)
  cotton_real <- filter(cotton_real, (temp >= 20.5))
  cttandat <- filter(cttandat, (temp >= 16.8 & temp <= 33.2))
  cttandat$cotton_approx <- ifelse(cttandat$cotton_approx < 0, 0, cttandat$cotton_approx)
  cttandat$temp <- as.character(cttandat$temp)
  
  # Bind real along envelope
  real <- rbind(wheat_real, corn_real, cotton_real)
  real$temp <- as.character(real$temp)
  
  
  ### !!!!! FIX MERGES
  # Bind all
  tandat <- data.frame(temp = seq(0, 33.2, 0.1))
  tandat$temp <- as.character(tandat$temp)
  tandat <- left_join(tandat, wtandat, by = "temp")
  tandat <- left_join(tandat, ctandat, by = "temp")
  tandat <- left_join(tandat, cttandat, by = "temp")
  tandat <- left_join(tandat, real, by = "temp")
  
  # tandat[is.na(tandat)] <- 0
  
  tandat$wheat_adaptation <- tandat$real - tandat$wheat_approx
  tandat$corn_adaptation <- tandat$real - tandat$corn_approx
  tandat$cotton_adaptation <- tandat$real - tandat$cotton_approx
  adaptation <- sum(tandat$wheat_adaptation[1:96], na.rm = TRUE) + 
    sum(tandat$corn_adaptation[96:206], na.rm = TRUE) + sum(tandat$cotton_adaptation[206:333], na.rm = TRUE)
  adaptation
  # tandat <- rbind(wtandat, ctandat, cttandat)
  # tandat$ctemp <- i
  # tandCat$approx_cumsum <- cumsum(tandat$approx)
  # tandaCt$real_cumsum <- cumsum(tandat$real)
  # indat <- data.frame(ctemp = i, 
  #                     approx = last(tandat$approx_cumsum),
  #                     real = last(tandat$real_cumsum))
  indat <- data.frame(temp = i,
                      adaptation = adaptation)
  outdat <- rbind(outdat, indat)
}

outdat

pdat <- gather(outdat, key = est, value = value, -ctemp)
p1 <- ggplot(pdat, aes(ctemp, value, color = est)) + 
  theme_tufte(base_size = 12) +
  geom_point() + 
  geom_line() +
  xlab("Change in Temperature (C)") +
  ylab("Damage to Value of Activity") +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  geom_text(x = 4.5, y = -105000, label = "Approximate Value", color = "#F8766D") +
  geom_text(x = 4.25, y = -170000, label = "Real Value", color = "#00BFC4") +
  scale_x_continuous(breaks = 0:5, labels = c("+0C", "+1C", "+2C", "+3C", "+4C", "+5C")) +
  theme(legend.position = "none",
    legend.title = element_blank())

  # Return list with data and plot
  retlist <- list(retdat = pdat, plot = p1)
  return(retlist)
}

odat1 <- mendelsohn_fn(approxfun(density(rnorm(351, 15, 7))))
odat1$plot
odat1$retdat$type <- 1

odat2 <- mendelsohn_fn(approxfun(density(rnorm(351, 17, 10))))
odat2$plot
odat2$retdat$type <- 2

odat3 <- mendelsohn_fn(approxfun(density(runif(351, 3, 30))))
odat3$plot
odat3$retdat$type <- 3

odat4 <- mendelsohn_fn(approxfun(density(runif(351, 5, 32))))
odat4$plot
odat4$retdat$type <- 4


odat <- rbind(odat1$retdat, odat2$retdat, odat3$retdat, odat4$retdat)
odat

odat <- spread(odat, key = est, value = value)
odat <- arrange(odat, type)
odat
odat$adaptation <- odat$real - odat$approx

odat  
ggsave(filename = "damage_va.pdf", width = 6, height = 4)

  
  