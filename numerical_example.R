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

# Density fn
phi <- approxfun(density(rnorm(35, 15, 7)))
phi <- approxfun(runif(35, 0))

# Real Integration fn
int_wheatfn <- function(x) (wheat_fn(x + 2) - wheat_fn(x)) * phi(x)
int_cornfn <- function(x) (corn_fn(x + 2) - corn_fn(x)) * phi(x)
int_cottonfn <- function(x) (cotton_fn(x + 2) - cotton_fn(x)) * phi(x)


# Approximate Integration fn 
approxint_wheatfn <- function(x) (wheat_dt(x) * 2) * phi(x)
approxint_cornfn <- function(x) (corn_dt(x) * 2) * phi(x)
approxint_cottonfn <- function(x) (cotton_dt(x) * 2) * phi(x)

# # Marginal Effect
# integrate(approxint_wheatfn, 0.1, 9.4)[[1]] + integrate(approxint_cornfn, 9.5, 20.4)[[1]] + integrate(approxint_cottonfn, 20.5, 35)[[1]]
# 
# # Real Effect
# integrate(int_wheatfn, 0.1, 9.4)[[1]] + integrate(int_cornfn, 9.5, 20.4)[[1]] + integrate(int_cottonfn, 20.5, 35)[[1]]

outdat <- data.frame()
for (i in 0:5){
# With tangent lines
  wtandat <- dat
  wtandat$yy <- wheat_fn(wtandat$temp) - wheat_dt(wtandat$temp)*i
  wtandat$xx <- wtandat$temp - 2
  wtandat$yend <- wheat_fn(wtandat$temp) + wheat_dt(wtandat$temp)*i
  wtandat$xend <- wtandat$temp + 2
  wtandat$approx <- round((wheat_dt(wtandat$temp)*i) * phi(wtandat$temp), 2)
  wtandat$real <- round((wheat_fn(wtandat$temp + i) - wheat_fn(wtandat$temp))* phi(wtandat$temp), 2)
  wtandat <- filter(wtandat, crop == "Wheat" & temp <= 9.4)
  
  ctandat <- dat
  ctandat$yy <- corn_fn(ctandat$temp) - corn_dt(ctandat$temp)*i
  ctandat$xx <- ctandat$temp - 2
  ctandat$yend <- corn_fn(ctandat$temp) + corn_dt(ctandat$temp)*i
  ctandat$xend <- ctandat$temp + 2
  ctandat$approx <- round((wheat_dt(ctandat$temp)*i)* phi(ctandat$temp), 2)
  ctandat$real <- round((wheat_fn(ctandat$temp + i) - wheat_fn(ctandat$temp)) * phi(ctandat$temp), 2)
  ctandat <- filter(ctandat, crop == "Corn" & (temp >= 9.4 & temp <= 20.4))
  
  cttandat <- dat
  cttandat$yy <- cotton_fn(cttandat$temp) - cotton_dt(cttandat$temp)*i
  cttandat$xx <- cttandat$temp - 2
  cttandat$yend <- cotton_fn(cttandat$temp) + cotton_dt(cttandat$temp)*i
  cttandat$xend <- cttandat$temp + 2
  cttandat$approx <- round((wheat_dt(cttandat$temp)*i)* phi(cttandat$temp), 2)
  cttandat$real <- round((wheat_fn(cttandat$temp + i) - wheat_fn(cttandat$temp))* phi(cttandat$temp), 2)
  cttandat <- filter(cttandat, crop == "Cotton" & temp >= 20.4)
  
  tandat <- rbind(wtandat, ctandat, cttandat)
  tandat$ctemp <- i
  tandat$approx_cumsum <- cumsum(tandat$approx)
  tandat$real_cumsum <- cumsum(tandat$real)
  indat <- data.frame(ctemp = i, 
                      approx = last(tandat$approx_cumsum),
                      real = last(tandat$real_cumsum))
  outdat <- rbind(outdat, indat)
}
outdat

pdat <- gather(outdat, key = est, value = value, -ctemp)
ggplot(pdat, aes(ctemp, value, color = est)) + 
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
ggsave(filename = "damage_va.pdf", width = 6, height = 4)

  
  
  
  #-----------------------------------------------------
# Loop through temp
# outdat <- data.frame()
# for (t in 0:5){
#   
#   # Update functions
#   if (t == 0){
#     int_wheatfn <- function(x) (wheat_fn(x + 0) - wheat_fn(x)) * phi(x)
#     int_cornfn <- function(x) (corn_fn(x + 0) - corn_fn(x)) * phi(x)
#     int_cottonfn <- function(x) (cotton_fn(x + 0) - cotton_fn(x)) * phi(x)
#     # Approximate Integration fn 
#     approxint_wheatfn <- function(x) (wheat_dt(x)) * phi(x) * 0
#     approxint_cornfn <- function(x) (corn_dt(x)) * phi(x) * 0
#     approxint_cottonfn <- function(x) (cotton_dt(x)) * phi(x) * 0
#   }
#   
#   if (t == 1){
#     int_wheatfn <- function(x) (wheat_fn(x + 1) - wheat_fn(x)) * phi(x)
#     int_cornfn <- function(x) (corn_fn(x + 1) - corn_fn(x)) * phi(x)
#     int_cottonfn <- function(x) (cotton_fn(x + 1) - cotton_fn(x)) * phi(x)
#      # Approximate Integration fn 
#     approxint_wheatfn <- function(x) (wheat_dt(x)) * phi(x) * 1
#     approxint_cornfn <- function(x) (corn_dt(x)) * phi(x) * 1
#     approxint_cottonfn <- function(x) (cotton_dt(x)) * phi(x) * 1
#     }
#   
#   if (t == 2){
#     int_wheatfn <- function(x) (wheat_fn(x + 2) - wheat_fn(x)) * phi(x)
#     int_cornfn <- function(x) (corn_fn(x + 2) - corn_fn(x)) * phi(x)
#     int_cottonfn <- function(x) (cotton_fn(x + 2) - cotton_fn(x)) * phi(x)
#     # Approximate Integration fn 
#     approxint_wheatfn <- function(x) (wheat_dt(x)) * phi(x) * 2
#     approxint_cornfn <- function(x) (corn_dt(x)) * phi(x) * 2
#     approxint_cottonfn <- function(x) (cotton_dt(x)) * phi(x) * 2
#     }
#   
#   if (t == 3){
#     int_wheatfn <- function(x) (wheat_fn(x + 3) - wheat_fn(x)) * phi(x)
#     int_cornfn <- function(x) (corn_fn(x + 3) - corn_fn(x)) * phi(x)
#     int_cottonfn <- function(x) (cotton_fn(x + 3) - cotton_fn(x)) * phi(x)
#     # Approximate Integration fn 
#     approxint_wheatfn <- function(x) (wheat_dt(x)) * phi(x) * 3
#     approxint_cornfn <- function(x) (corn_dt(x)) * phi(x) * 3
#     approxint_cottonfn <- function(x) (cotton_dt(x)) * phi(x) * 3
#     }
#   
#   if (t == 4){
#     int_wheatfn <- function(x) (wheat_fn(x + 4) - wheat_fn(x)) * phi(x)
#     int_cornfn <- function(x) (corn_fn(x + 4) - corn_fn(x)) * phi(x)
#     int_cottonfn <- function(x) (cotton_fn(x + 4) - cotton_fn(x)) * phi(x)
#     
#     # Approximate Integration fn 
#     approxint_wheatfn <- function(x) (wheat_dt(x)) * phi(x) * 4
#     approxint_cornfn <- function(x) (corn_dt(x)) * phi(x) * 4
#     approxint_cottonfn <- function(x) (cotton_dt(x)) * phi(x) * 4
#   }
#     
#   if (t == 5){
#     int_wheatfn <- function(x) (wheat_fn(x + 5) - wheat_fn(x)) * phi(x)
#     int_cornfn <- function(x) (corn_fn(x + 5) - corn_fn(x)) * phi(x)
#     int_cottonfn <- function(x) (cotton_fn(x + 5) - cotton_fn(x)) * phi(x)
#     # Approximate Integration fn 
#     approxint_wheatfn <- function(x) (wheat_dt(x)) * phi(x) * 5
#     approxint_cornfn <- function(x) (corn_dt(x)) * phi(x) * 5
#     approxint_cottonfn <- function(x) (cotton_dt(x)) * phi(x) * 5
#   }
#    # Marginal Effect
#   me <- integrate(approxint_wheatfn, 0, 9.4)[[1]] + integrate(approxint_cornfn, 9.4, 20.4)[[1]] + integrate(approxint_cottonfn, 20.4, 35)[[1]]
# 
#   # Real Effect
#   re <- integrate(int_wheatfn, 0, 9.4)[[1]] + integrate(int_cornfn, 9.4, 20.4)[[1]] + integrate(int_cottonfn, 20.4, 35)[[1]]
#  
#   indat <- data.frame(temp = t, me = me, re = re)
#   outdat <- rbind(outdat, indat)
#   
#     }
# outdat
# 
# 
