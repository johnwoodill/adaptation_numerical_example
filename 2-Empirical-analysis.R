library(tidyverse)
library(ggthemes)
library(cowplot)
library(numDeriv)


# Steps
# [1] Setup bins and Load data
# [2] Approximate functions for temperature and revenue and proportion of acres
# [3] Estimate four equations
#     * First-order approximation without adaptaiton
#     * First-order approximation with adaptation
#     * Nonlinear effects without adaptaiton
#     * Nonlinear effects with adaptation
# [4] Bind data


# Notes: 
# * Increase in degree days of +1-5C taken directly from the data

# Degree Days 10-30
# +1C = +140 Degree Days between 10-30
# +2C = +280 Degree Days between 10-30
# +3C = +420 Degree Days between 10-30
# +4C = +560 Degree Days between 10-30
# +5C = +700 Degree Days between 10-30

# Degree Days > 30
# +1C = +15 Degree Days between 10-30
# +2C = +30 Degree Days between 10-30
# +3C = +45 Degree Days between 10-30
# +4C = +60 Degree Days between 10-30
# +5C = +75 Degree Days between 10-30




# [1] Setup bins and Load data
#----------------------------------------------------------------------------------

# Set bins for main analysis
bins <- 10

# Load data from 1-Data-step.R for main analysis with 10-bins
tavg_dat <- readRDS("data/tavg_dat_bins_10.rds")
dday10_30_dat <- readRDS("data/dday10_30_dat_bins_10.rds")
dday30_dat <- readRDS("data/dday30_dat_bins_10.rds")






# [2] Approximate functions for temperature and revenue and proportion of acres
#----------------------------------------------------------------------------------


### Approx fn Linear interpolation within bins

# Estimate x=tavg    y=revenue
tavg_corn_fn <- approxfun(tavg_dat$tavg, tavg_dat$corn_rev)
tavg_cotton_fn <- approxfun(tavg_dat$tavg, tavg_dat$cotton_rev)
tavg_hay_fn <- approxfun(tavg_dat$tavg, tavg_dat$hay_rev)
tavg_wheat_fn <- approxfun(tavg_dat$tavg, tavg_dat$wheat_rev)
tavg_soybean_fn <- approxfun(tavg_dat$tavg, tavg_dat$soybean_rev)

# Estimate x=dday10-30C    y=revenue
dday10_30_corn_fn <- approxfun(dday10_30_dat$dday10_30, dday10_30_dat$corn_rev)
dday10_30_cotton_fn <- approxfun(dday10_30_dat$dday10_30, dday10_30_dat$cotton_rev)
dday10_30_hay_fn <- approxfun(dday10_30_dat$dday10_30, dday10_30_dat$hay_rev)
dday10_30_wheat_fn <- approxfun(dday10_30_dat$dday10_30, dday10_30_dat$wheat_rev)
dday10_30_soybean_fn <- approxfun(dday10_30_dat$dday10_30, dday10_30_dat$soybean_rev)

# Estimate x=dday > 30C    y=revenue
dday30_corn_fn <- approxfun(dday30_dat$dday30, dday30_dat$corn_rev)
dday30_cotton_fn <- approxfun(dday30_dat$dday30, dday30_dat$cotton_rev)
dday30_hay_fn <- approxfun(dday30_dat$dday30, dday30_dat$hay_rev)
dday30_wheat_fn <- approxfun(dday30_dat$dday30, dday30_dat$wheat_rev)
dday30_soybean_fn <- approxfun(dday30_dat$dday30, dday30_dat$soybean_rev)

# Estimate x=tavg    y=prop of crop acres
tavg_p_corn_fn <- approxfun(tavg_dat$tavg, tavg_dat$p_corn_a)
tavg_p_cotton_fn <- approxfun(tavg_dat$tavg, tavg_dat$p_cotton_a)
tavg_p_hay_fn <- approxfun(tavg_dat$tavg, tavg_dat$p_hay_a)
tavg_p_wheat_fn <- approxfun(tavg_dat$tavg, tavg_dat$p_wheat_a)
tavg_p_soybean_fn <- approxfun(tavg_dat$tavg, tavg_dat$p_soybean_a)

# Estimate x=dday10-30C    y=prop of crop acres
dday10_30_p_corn_fn <- approxfun(dday10_30_dat$dday10_30, dday10_30_dat$p_corn_a)
dday10_30_p_cotton_fn <- approxfun(dday10_30_dat$dday10_30, dday10_30_dat$p_cotton_a)
dday10_30_p_hay_fn <- approxfun(dday10_30_dat$dday10_30, dday10_30_dat$p_hay_a)
dday10_30_p_wheat_fn <- approxfun(dday10_30_dat$dday10_30, dday10_30_dat$p_wheat_a)
dday10_30_p_soybean_fn <- approxfun(dday10_30_dat$dday10_30, dday10_30_dat$p_soybean_a)

# Estimate x=dday > 30C    y=prop of crop acres
dday30_p_corn_fn <- approxfun(dday30_dat$dday30, dday30_dat$p_corn_a)
dday30_p_cotton_fn <- approxfun(dday30_dat$dday30, dday30_dat$p_cotton_a)
dday30_p_hay_fn <- approxfun(dday30_dat$dday30, dday30_dat$p_hay_a)
dday30_p_wheat_fn <- approxfun(dday30_dat$dday30, dday30_dat$p_wheat_a)
dday30_p_soybean_fn <- approxfun(dday30_dat$dday30, dday30_dat$p_soybean_a)

# Build data.frame to plot
int_pdat <- data.frame(temp = rep(c("Average Temp. (C)", "Degree Day (10-30C)", "Degree Day (30C)"), each = bins*5),
                       crop = rep(c("Corn", "Cotton", "Hay", "Wheat", "Soybean"), each = bins),
                       x = c(rep(tavg_dat$tavg, 5), 
                             rep(dday10_30_dat$dday10_30, 5), 
                             rep(dday30_dat$dday30, 5)),
                       y = c(tavg_corn_fn(tavg_dat$tavg), 
                             tavg_cotton_fn(tavg_dat$tavg), 
                             tavg_hay_fn(tavg_dat$tavg), 
                             tavg_wheat_fn(tavg_dat$tavg), 
                             tavg_soybean_fn(tavg_dat$tavg),
                             dday10_30_corn_fn(dday10_30_dat$dday10_30), 
                             dday10_30_cotton_fn(dday10_30_dat$dday10_30), 
                             dday10_30_hay_fn(dday10_30_dat$dday10_30), 
                             dday10_30_wheat_fn(dday10_30_dat$dday10_30), 
                             dday10_30_soybean_fn(dday10_30_dat$dday10_30),
                             dday30_corn_fn(dday30_dat$dday30), 
                             dday30_cotton_fn(dday30_dat$dday30), 
                             dday30_hay_fn(dday30_dat$dday30), 
                             dday30_wheat_fn(dday30_dat$dday30), 
                             dday30_soybean_fn(dday30_dat$dday30)))





# [3] Estimate four equations
#----------------------------------------------------------------------------------

### First-order approximation without adaptation

# Get temperature and cut end-points
tavg <- tavg_dat$tavg
tavg[1] <- tavg_dat$tavg[1] + 0.1
tavg[length(tavg_dat$tavg)] <- tavg[length(tavg_dat$tavg)] - 0.1

# Get baseline
tavg_base <- sum(tavg_dat$corn_rev*tavg_dat$corn_acres) +
             sum(tavg_dat$cotton_rev*tavg_dat$cotton_acres) +
             sum(tavg_dat$hay_rev*tavg_dat$hay_acres) +
             sum(tavg_dat$wheat_rev*tavg_dat$wheat_acres) +
             sum(tavg_dat$soybean_rev*tavg_dat$soybean_acres)
  
# Apply shift in temperature (+1C)
tavg_temp1 <- tavg_base + 
  sum((tavg_corn_fn(tavg_dat$tavg + 1)*tavg_dat$corn_acres - tavg_corn_fn(tavg_dat$tavg)*tavg_dat$corn_acres), na.rm = TRUE) +
  sum((tavg_cotton_fn(tavg_dat$tavg + 1)*tavg_dat$cotton_acres - tavg_cotton_fn(tavg_dat$tavg)*tavg_dat$cotton_acres), na.rm = TRUE) +
  sum((tavg_hay_fn(tavg_dat$tavg + 1)*tavg_dat$hay_acres - tavg_hay_fn(tavg_dat$tavg)*tavg_dat$hay_acres), na.rm = TRUE) +
  sum((tavg_wheat_fn(tavg_dat$tavg + 1)*tavg_dat$wheat_acres - tavg_wheat_fn(tavg_dat$tavg)*tavg_dat$wheat_acres), na.rm = TRUE) +
  sum((tavg_soybean_fn(tavg_dat$tavg + 1)*tavg_dat$soybean_acres - tavg_soybean_fn(tavg_dat$tavg)*tavg_dat$soybean_acres), na.rm = TRUE)

# Apply shift in temperature (+2C)
tavg_temp2 <- tavg_base + 
  sum((tavg_corn_fn(tavg_dat$tavg + 2)*tavg_dat$corn_acres - tavg_corn_fn(tavg_dat$tavg)*tavg_dat$corn_acres), na.rm = TRUE) +
  sum((tavg_cotton_fn(tavg_dat$tavg + 2)*tavg_dat$cotton_acres - tavg_cotton_fn(tavg_dat$tavg)*tavg_dat$cotton_acres), na.rm = TRUE) +
  sum((tavg_hay_fn(tavg_dat$tavg + 2)*tavg_dat$hay_acres - tavg_hay_fn(tavg_dat$tavg)*tavg_dat$hay_acres), na.rm = TRUE) +
  sum((tavg_wheat_fn(tavg_dat$tavg + 2)*tavg_dat$wheat_acres - tavg_wheat_fn(tavg_dat$tavg)*tavg_dat$wheat_acres), na.rm = TRUE) +
  sum((tavg_soybean_fn(tavg_dat$tavg + 2)*tavg_dat$soybean_acres - tavg_soybean_fn(tavg_dat$tavg)*tavg_dat$soybean_acres), na.rm = TRUE)

# Apply shift in temperature (+3C)
tavg_temp3 <- tavg_base + 
  sum((tavg_corn_fn(tavg_dat$tavg + 3)*tavg_dat$corn_acres - tavg_corn_fn(tavg_dat$tavg)*tavg_dat$corn_acres), na.rm = TRUE) +
  sum((tavg_cotton_fn(tavg_dat$tavg + 3)*tavg_dat$cotton_acres - tavg_cotton_fn(tavg_dat$tavg)*tavg_dat$cotton_acres), na.rm = TRUE) +
  sum((tavg_hay_fn(tavg_dat$tavg + 3)*tavg_dat$hay_acres - tavg_hay_fn(tavg_dat$tavg)*tavg_dat$hay_acres), na.rm = TRUE) +
  sum((tavg_wheat_fn(tavg_dat$tavg + 3)*tavg_dat$wheat_acres - tavg_wheat_fn(tavg_dat$tavg)*tavg_dat$wheat_acres), na.rm = TRUE) +
  sum((tavg_soybean_fn(tavg_dat$tavg + 3)*tavg_dat$soybean_acres - tavg_soybean_fn(tavg_dat$tavg)*tavg_dat$soybean_acres), na.rm = TRUE)

# Apply shift in temperature (+4C)
tavg_temp4 <- tavg_base + 
  sum((tavg_corn_fn(tavg_dat$tavg + 4)*tavg_dat$corn_acres - tavg_corn_fn(tavg_dat$tavg)*tavg_dat$corn_acres), na.rm = TRUE) +
  sum((tavg_cotton_fn(tavg_dat$tavg + 4)*tavg_dat$cotton_acres - tavg_cotton_fn(tavg_dat$tavg)*tavg_dat$cotton_acres), na.rm = TRUE) +
  sum((tavg_hay_fn(tavg_dat$tavg + 4)*tavg_dat$hay_acres - tavg_hay_fn(tavg_dat$tavg)*tavg_dat$hay_acres), na.rm = TRUE) +
  sum((tavg_wheat_fn(tavg_dat$tavg + 4)*tavg_dat$wheat_acres - tavg_wheat_fn(tavg_dat$tavg)*tavg_dat$wheat_acres), na.rm = TRUE) +
  sum((tavg_soybean_fn(tavg_dat$tavg + 4)*tavg_dat$soybean_acres - tavg_soybean_fn(tavg_dat$tavg)*tavg_dat$soybean_acres), na.rm = TRUE)

# Apply shift in temperature (+5C)
tavg_temp5 <- tavg_base + 
  sum((tavg_corn_fn(tavg_dat$tavg + 5)*tavg_dat$corn_acres - tavg_corn_fn(tavg_dat$tavg)*tavg_dat$corn_acres), na.rm = TRUE) +
  sum((tavg_cotton_fn(tavg_dat$tavg + 5)*tavg_dat$cotton_acres - tavg_cotton_fn(tavg_dat$tavg)*tavg_dat$cotton_acres), na.rm = TRUE) +
  sum((tavg_hay_fn(tavg_dat$tavg + 5)*tavg_dat$hay_acres - tavg_hay_fn(tavg_dat$tavg)*tavg_dat$hay_acres), na.rm = TRUE) +
  sum((tavg_wheat_fn(tavg_dat$tavg + 5)*tavg_dat$wheat_acres - tavg_wheat_fn(tavg_dat$tavg)*tavg_dat$wheat_acres), na.rm = TRUE) +
  sum((tavg_soybean_fn(tavg_dat$tavg + 5)*tavg_dat$soybean_acres - tavg_soybean_fn(tavg_dat$tavg)*tavg_dat$soybean_acres), na.rm = TRUE)

# Build data.frame of results
adapt1_tavg <- data.frame(temp = "Average Temperature (C)",
                          model = "Total Effect",
                     rev = c(tavg_base, 
                             tavg_temp1, 
                             tavg_temp2, 
                             tavg_temp3, 
                             tavg_temp4, 
                             tavg_temp5))

adapt1_tavg$change = (adapt1_tavg$rev - first(adapt1_tavg$rev))/first(adapt1_tavg$rev)*100

# Checks
adapt1_tavg
plot(x=0:5, adapt1_tavg$change)
lines(x=0:5, adapt1_tavg$change)




# Degree Days 10-30 and cut end-points
dday10_30 <- dday10_30_dat$dday10_30
dday10_30[1] <- dday10_30_dat$dday10_30[1] + 0.1
dday10_30[length(dday10_30_dat$dday10_30)] <- dday10_30[length(dday10_30_dat$dday10_30)] - 0.1

# Get baseline
dday10_30_base <- sum(dday10_30_dat$corn_rev*dday10_30_dat$corn_acres) +
                   sum(dday10_30_dat$cotton_rev*dday10_30_dat$cotton_acres) +
                   sum(dday10_30_dat$hay_rev*dday10_30_dat$hay_acres) +
                   sum(dday10_30_dat$wheat_rev*dday10_30_dat$wheat_acres) +
                   sum(dday10_30_dat$soybean_rev*dday10_30_dat$soybean_acres)
  
# Apply shift in temperature (+140 Degree Days 10-30)
dday10_30_temp1 <- dday10_30_base + 
  sum((dday10_30_corn_fn(dday10_30_dat$dday10_30 + 140)*dday10_30_dat$corn_acres - dday10_30_corn_fn(dday10_30_dat$dday10_30)*dday10_30_dat$corn_acres), na.rm = TRUE) +
  sum((dday10_30_cotton_fn(dday10_30_dat$dday10_30 + 140)*dday10_30_dat$cotton_acres - dday10_30_cotton_fn(dday10_30_dat$dday10_30)*dday10_30_dat$cotton_acres), na.rm = TRUE) +
  sum((dday10_30_hay_fn(dday10_30_dat$dday10_30 + 140)*dday10_30_dat$hay_acres - dday10_30_hay_fn(dday10_30_dat$dday10_30)*dday10_30_dat$hay_acres), na.rm = TRUE) +
  sum((dday10_30_wheat_fn(dday10_30_dat$dday10_30 + 140)*dday10_30_dat$wheat_acres - dday10_30_wheat_fn(dday10_30_dat$dday10_30)*dday10_30_dat$wheat_acres), na.rm = TRUE) +
  sum((dday10_30_soybean_fn(dday10_30_dat$dday10_30 + 140)*dday10_30_dat$soybean_acres - dday10_30_soybean_fn(dday10_30_dat$dday10_30)*dday10_30_dat$soybean_acres), na.rm = TRUE)

# Apply shift in temperature (+280 Degree Days 10-30)
dday10_30_temp2 <- dday10_30_base + 
  sum((dday10_30_corn_fn(dday10_30_dat$dday10_30 + 280)*dday10_30_dat$corn_acres - dday10_30_corn_fn(dday10_30_dat$dday10_30)*dday10_30_dat$corn_acres), na.rm = TRUE) +
  sum((dday10_30_cotton_fn(dday10_30_dat$dday10_30 + 280)*dday10_30_dat$cotton_acres - dday10_30_cotton_fn(dday10_30_dat$dday10_30)*dday10_30_dat$cotton_acres), na.rm = TRUE) +
  sum((dday10_30_hay_fn(dday10_30_dat$dday10_30 + 280)*dday10_30_dat$hay_acres - dday10_30_hay_fn(dday10_30_dat$dday10_30)*dday10_30_dat$hay_acres), na.rm = TRUE) +
  sum((dday10_30_wheat_fn(dday10_30_dat$dday10_30 + 280)*dday10_30_dat$wheat_acres - dday10_30_wheat_fn(dday10_30_dat$dday10_30)*dday10_30_dat$wheat_acres), na.rm = TRUE) +
  sum((dday10_30_soybean_fn(dday10_30_dat$dday10_30 + 280)*dday10_30_dat$soybean_acres - dday10_30_soybean_fn(dday10_30_dat$dday10_30)*dday10_30_dat$soybean_acres), na.rm = TRUE)

# Apply shift in temperature (+420 Degree Days 10-30)
dday10_30_temp3 <- dday10_30_base + 
 sum((dday10_30_corn_fn(dday10_30_dat$dday10_30 + 420)*dday10_30_dat$corn_acres - dday10_30_corn_fn(dday10_30_dat$dday10_30)*dday10_30_dat$corn_acres), na.rm = TRUE) +
 sum((dday10_30_cotton_fn(dday10_30_dat$dday10_30 + 420)*dday10_30_dat$cotton_acres - dday10_30_cotton_fn(dday10_30_dat$dday10_30)*dday10_30_dat$cotton_acres), na.rm = TRUE) +
 sum((dday10_30_hay_fn(dday10_30_dat$dday10_30 + 420)*dday10_30_dat$hay_acres - dday10_30_hay_fn(dday10_30_dat$dday10_30)*dday10_30_dat$hay_acres), na.rm = TRUE) +
 sum((dday10_30_wheat_fn(dday10_30_dat$dday10_30 + 420)*dday10_30_dat$wheat_acres - dday10_30_wheat_fn(dday10_30_dat$dday10_30)*dday10_30_dat$wheat_acres), na.rm = TRUE) +
 sum((dday10_30_soybean_fn(dday10_30_dat$dday10_30 + 420)*dday10_30_dat$soybean_acres - dday10_30_soybean_fn(dday10_30_dat$dday10_30)*dday10_30_dat$soybean_acres), na.rm = TRUE)

# Apply shift in temperature (+560 Degree Days 10-30)
dday10_30_temp4 <- dday10_30_base + 
 sum((dday10_30_corn_fn(dday10_30_dat$dday10_30 + 560)*dday10_30_dat$corn_acres - dday10_30_corn_fn(dday10_30_dat$dday10_30)*dday10_30_dat$corn_acres), na.rm = TRUE) +
 sum((dday10_30_cotton_fn(dday10_30_dat$dday10_30 + 560)*dday10_30_dat$cotton_acres - dday10_30_cotton_fn(dday10_30_dat$dday10_30)*dday10_30_dat$cotton_acres), na.rm = TRUE) +
 sum((dday10_30_hay_fn(dday10_30_dat$dday10_30 + 560)*dday10_30_dat$hay_acres - dday10_30_hay_fn(dday10_30_dat$dday10_30)*dday10_30_dat$hay_acres), na.rm = TRUE) +
 sum((dday10_30_wheat_fn(dday10_30_dat$dday10_30 + 560)*dday10_30_dat$wheat_acres - dday10_30_wheat_fn(dday10_30_dat$dday10_30)*dday10_30_dat$wheat_acres), na.rm = TRUE) +
 sum((dday10_30_soybean_fn(dday10_30_dat$dday10_30 + 560)*dday10_30_dat$soybean_acres - dday10_30_soybean_fn(dday10_30_dat$dday10_30)*dday10_30_dat$soybean_acres), na.rm = TRUE)

# Apply shift in temperature (+700 Degree Days 10-30)
dday10_30_temp5 <- dday10_30_base + 
 sum((dday10_30_corn_fn(dday10_30_dat$dday10_30 + 700)*dday10_30_dat$corn_acres - dday10_30_corn_fn(dday10_30_dat$dday10_30)*dday10_30_dat$corn_acres), na.rm = TRUE) +
 sum((dday10_30_cotton_fn(dday10_30_dat$dday10_30 + 700)*dday10_30_dat$cotton_acres - dday10_30_cotton_fn(dday10_30_dat$dday10_30)*dday10_30_dat$cotton_acres), na.rm = TRUE) +
 sum((dday10_30_hay_fn(dday10_30_dat$dday10_30 + 700)*dday10_30_dat$hay_acres - dday10_30_hay_fn(dday10_30_dat$dday10_30)*dday10_30_dat$hay_acres), na.rm = TRUE) +
 sum((dday10_30_wheat_fn(dday10_30_dat$dday10_30 + 700)*dday10_30_dat$wheat_acres - dday10_30_wheat_fn(dday10_30_dat$dday10_30)*dday10_30_dat$wheat_acres), na.rm = TRUE) +
 sum((dday10_30_soybean_fn(dday10_30_dat$dday10_30 + 700)*dday10_30_dat$soybean_acres - dday10_30_soybean_fn(dday10_30_dat$dday10_30)*dday10_30_dat$soybean_acres), na.rm = TRUE)


# Build data.frame of results
adapt1_dday10_30 <- data.frame(temp = "Degree Day (10-30C)",
                          model = "Total Effect",
                     rev = c(dday10_30_base, 
                             dday10_30_temp1, 
                             dday10_30_temp2, 
                             dday10_30_temp3, 
                             dday10_30_temp4, 
                             dday10_30_temp5))

adapt1_dday10_30$change = (adapt1_dday10_30$rev - first(adapt1_dday10_30$rev))/first(adapt1_dday10_30$rev)*100

# Check
adapt1_dday10_30
plot(x=0:5, adapt1_dday10_30$change)
lines(x=0:5, adapt1_dday10_30$change)



# Degree Days 30 and cut end-points
dday30 <- dday30_dat$dday30
dday30[1] <- dday30_dat$dday30[1] + 0.1
dday30[length(dday30_dat$dday30)] <- dday30[length(dday30_dat$dday30)] - 0.1

# Get baseline
dday30_base <- sum(dday30_dat$corn_rev*dday30_dat$corn_acres) +
               sum(dday30_dat$cotton_rev*dday30_dat$cotton_acres) +
               sum(dday30_dat$hay_rev*dday30_dat$hay_acres) +
               sum(dday30_dat$wheat_rev*dday30_dat$wheat_acres) +
               sum(dday30_dat$soybean_rev*dday30_dat$soybean_acres)
  
# Apply shift in temperature (+15 Degree Days 30)
dday30_temp1 <- dday30_base + 
  sum((dday30_corn_fn(dday30_dat$dday30 + 15)*dday30_dat$corn_acres - dday30_corn_fn(dday30_dat$dday30)*dday30_dat$corn_acres), na.rm = TRUE) +
  sum((dday30_cotton_fn(dday30_dat$dday30 + 15)*dday30_dat$cotton_acres - dday30_cotton_fn(dday30_dat$dday30)*dday30_dat$cotton_acres), na.rm = TRUE) +
  sum((dday30_hay_fn(dday30_dat$dday30 + 15)*dday30_dat$hay_acres - dday30_hay_fn(dday30_dat$dday30)*dday30_dat$hay_acres), na.rm = TRUE) +
  sum((dday30_wheat_fn(dday30_dat$dday30 + 15)*dday30_dat$wheat_acres - dday30_wheat_fn(dday30_dat$dday30)*dday30_dat$wheat_acres), na.rm = TRUE) +
  sum((dday30_soybean_fn(dday30_dat$dday30 + 15)*dday30_dat$soybean_acres - dday30_soybean_fn(dday30_dat$dday30)*dday30_dat$soybean_acres), na.rm = TRUE)

# Apply shift in temperature (+30 Degree Days 30)
dday30_temp2 <- dday30_base + 
  sum((dday30_corn_fn(dday30_dat$dday30 + 30)*dday30_dat$corn_acres - dday30_corn_fn(dday30_dat$dday30)*dday30_dat$corn_acres), na.rm = TRUE) +
  sum((dday30_cotton_fn(dday30_dat$dday30 + 30)*dday30_dat$cotton_acres - dday30_cotton_fn(dday30_dat$dday30)*dday30_dat$cotton_acres), na.rm = TRUE) +
  sum((dday30_hay_fn(dday30_dat$dday30 + 30)*dday30_dat$hay_acres - dday30_hay_fn(dday30_dat$dday30)*dday30_dat$hay_acres), na.rm = TRUE) +
  sum((dday30_wheat_fn(dday30_dat$dday30 + 30)*dday30_dat$wheat_acres - dday30_wheat_fn(dday30_dat$dday30)*dday30_dat$wheat_acres), na.rm = TRUE) +
  sum((dday30_soybean_fn(dday30_dat$dday30 + 30)*dday30_dat$soybean_acres - dday30_soybean_fn(dday30_dat$dday30)*dday30_dat$soybean_acres), na.rm = TRUE)

# Apply shift in temperature (+45 Degree Days 30)
dday30_temp3 <- dday30_base + 
  sum((dday30_corn_fn(dday30_dat$dday30 + 45)*dday30_dat$corn_acres - dday30_corn_fn(dday30_dat$dday30)*dday30_dat$corn_acres), na.rm = TRUE) +
  sum((dday30_cotton_fn(dday30_dat$dday30 + 45)*dday30_dat$cotton_acres - dday30_cotton_fn(dday30_dat$dday30)*dday30_dat$cotton_acres), na.rm = TRUE) +
  sum((dday30_hay_fn(dday30_dat$dday30 + 45)*dday30_dat$hay_acres - dday30_hay_fn(dday30_dat$dday30)*dday30_dat$hay_acres), na.rm = TRUE) +
  sum((dday30_wheat_fn(dday30_dat$dday30 + 45)*dday30_dat$wheat_acres - dday30_wheat_fn(dday30_dat$dday30)*dday30_dat$wheat_acres), na.rm = TRUE) +
  sum((dday30_soybean_fn(dday30_dat$dday30 + 45)*dday30_dat$soybean_acres - dday30_soybean_fn(dday30_dat$dday30)*dday30_dat$soybean_acres), na.rm = TRUE)

# Apply shift in temperature (+60 Degree Days 30)
dday30_temp4 <- dday30_base + 
  sum((dday30_corn_fn(dday30_dat$dday30 + 60)*dday30_dat$corn_acres - dday30_corn_fn(dday30_dat$dday30)*dday30_dat$corn_acres), na.rm = TRUE) +
  sum((dday30_cotton_fn(dday30_dat$dday30 + 60)*dday30_dat$cotton_acres - dday30_cotton_fn(dday30_dat$dday30)*dday30_dat$cotton_acres), na.rm = TRUE) +
  sum((dday30_hay_fn(dday30_dat$dday30 + 60)*dday30_dat$hay_acres - dday30_hay_fn(dday30_dat$dday30)*dday30_dat$hay_acres), na.rm = TRUE) +
  sum((dday30_wheat_fn(dday30_dat$dday30 + 60)*dday30_dat$wheat_acres - dday30_wheat_fn(dday30_dat$dday30)*dday30_dat$wheat_acres), na.rm = TRUE) +
  sum((dday30_soybean_fn(dday30_dat$dday30 + 60)*dday30_dat$soybean_acres - dday30_soybean_fn(dday30_dat$dday30)*dday30_dat$soybean_acres), na.rm = TRUE)

# Apply shift in temperature (+75 Degree Days 30)
dday30_temp5 <- dday30_base + 
  sum((dday30_corn_fn(dday30_dat$dday30 + 75)*dday30_dat$corn_acres - dday30_corn_fn(dday30_dat$dday30)*dday30_dat$corn_acres), na.rm = TRUE) +
  sum((dday30_cotton_fn(dday30_dat$dday30 + 75)*dday30_dat$cotton_acres - dday30_cotton_fn(dday30_dat$dday30)*dday30_dat$cotton_acres), na.rm = TRUE) +
  sum((dday30_hay_fn(dday30_dat$dday30 + 75)*dday30_dat$hay_acres - dday30_hay_fn(dday30_dat$dday30)*dday30_dat$hay_acres), na.rm = TRUE) +
  sum((dday30_wheat_fn(dday30_dat$dday30 + 75)*dday30_dat$wheat_acres - dday30_wheat_fn(dday30_dat$dday30)*dday30_dat$wheat_acres), na.rm = TRUE) +
  sum((dday30_soybean_fn(dday30_dat$dday30 + 75)*dday30_dat$soybean_acres - dday30_soybean_fn(dday30_dat$dday30)*dday30_dat$soybean_acres), na.rm = TRUE)

# Build data.frame of results
adapt1_dday30 <- data.frame(temp = "Degree Day (30C)",
                          model = "Total Effect",
                         rev = c(dday30_base, 
                                 dday30_temp1, 
                                 dday30_temp2, 
                                 dday30_temp3, 
                                 dday30_temp4, 
                                 dday30_temp5))

adapt1_dday30$change = (adapt1_dday30$rev - first(adapt1_dday30$rev))/first(adapt1_dday30$rev)*100

# Checks
adapt1_dday30
plot(x=0:5, adapt1_dday30$change)
lines(x=0:5, adapt1_dday30$change)





#----------------------------------------------------------------------------------

### First-order approximation without adaptation


# Average Temperatures and cut end points
tavg <- tavg_dat$tavg
tavg[1] <- tavg_dat$tavg[1] + 0.1
tavg[length(tavg_dat$tavg)] <- tavg[length(tavg_dat$tavg)] - 0.1

# Get baseline
tavg_base <- sum(tavg_dat$corn_rev*tavg_dat$corn_acres) +
             sum(tavg_dat$cotton_rev*tavg_dat$cotton_acres) +
             sum(tavg_dat$hay_rev*tavg_dat$hay_acres) +
             sum(tavg_dat$wheat_rev*tavg_dat$wheat_acres) +
             sum(tavg_dat$soybean_rev*tavg_dat$soybean_acres)

# Apply shift in temperature (+1C Average Temperature)
tavg_temp1 <- tavg_base + 
 sum(((tavg_corn_fn(tavg + 1)*(tavg_p_corn_fn(tavg + 1)*tavg_dat$corn_acres))) - ((tavg_corn_fn(tavg)*(tavg_p_corn_fn(tavg)*tavg_dat$corn_acres))), na.rm = TRUE) +
 sum(((tavg_cotton_fn(tavg + 1)*(tavg_p_cotton_fn(tavg + 1)*tavg_dat$cotton_acres))) - ((tavg_cotton_fn(tavg)*(tavg_p_cotton_fn(tavg)*tavg_dat$cotton_acres))), na.rm = TRUE) +
 sum(((tavg_hay_fn(tavg + 1)*(tavg_p_hay_fn(tavg + 1)*tavg_dat$hay_acres))) - ((tavg_hay_fn(tavg)*(tavg_p_hay_fn(tavg)*tavg_dat$hay_acres))), na.rm = TRUE) +
 sum(((tavg_wheat_fn(tavg + 1)*(tavg_p_wheat_fn(tavg + 1)*tavg_dat$wheat_acres))) - ((tavg_wheat_fn(tavg)*(tavg_p_wheat_fn(tavg)*tavg_dat$wheat_acres))), na.rm = TRUE) +
 sum(((tavg_soybean_fn(tavg + 1)*(tavg_p_soybean_fn(tavg + 1)*tavg_dat$soybean_acres))) - ((tavg_soybean_fn(tavg)*(tavg_p_soybean_fn(tavg)*tavg_dat$soybean_acres))), na.rm = TRUE) 

# Apply shift in temperature (+2C Average Temperature)
tavg_temp2 <- tavg_base + 
 sum(((tavg_corn_fn(tavg + 2)*(tavg_p_corn_fn(tavg + 2)*tavg_dat$corn_acres))) - ((tavg_corn_fn(tavg)*(tavg_p_corn_fn(tavg)*tavg_dat$corn_acres))), na.rm = TRUE) +
 sum(((tavg_cotton_fn(tavg + 2)*(tavg_p_cotton_fn(tavg + 2)*tavg_dat$cotton_acres))) - ((tavg_cotton_fn(tavg)*(tavg_p_cotton_fn(tavg)*tavg_dat$cotton_acres))), na.rm = TRUE) +
 sum(((tavg_hay_fn(tavg + 2)*(tavg_p_hay_fn(tavg + 2)*tavg_dat$hay_acres))) - ((tavg_hay_fn(tavg)*(tavg_p_hay_fn(tavg)*tavg_dat$hay_acres))), na.rm = TRUE) +
 sum(((tavg_wheat_fn(tavg + 2)*(tavg_p_wheat_fn(tavg + 2)*tavg_dat$wheat_acres))) - ((tavg_wheat_fn(tavg)*(tavg_p_wheat_fn(tavg)*tavg_dat$wheat_acres))), na.rm = TRUE) +
 sum(((tavg_soybean_fn(tavg + 2)*(tavg_p_soybean_fn(tavg + 2)*tavg_dat$soybean_acres))) - ((tavg_soybean_fn(tavg)*(tavg_p_soybean_fn(tavg)*tavg_dat$soybean_acres))), na.rm = TRUE) 

# Apply shift in temperature (+3C Average Temperature)
tavg_temp3 <- tavg_base + 
 sum(((tavg_corn_fn(tavg + 3)*(tavg_p_corn_fn(tavg + 3)*tavg_dat$corn_acres))) - ((tavg_corn_fn(tavg)*(tavg_p_corn_fn(tavg)*tavg_dat$corn_acres))), na.rm = TRUE) +
 sum(((tavg_cotton_fn(tavg + 3)*(tavg_p_cotton_fn(tavg + 3)*tavg_dat$cotton_acres))) - ((tavg_cotton_fn(tavg)*(tavg_p_cotton_fn(tavg)*tavg_dat$cotton_acres))), na.rm = TRUE) +
 sum(((tavg_hay_fn(tavg + 3)*(tavg_p_hay_fn(tavg + 3)*tavg_dat$hay_acres))) - ((tavg_hay_fn(tavg)*(tavg_p_hay_fn(tavg)*tavg_dat$hay_acres))), na.rm = TRUE) +
 sum(((tavg_wheat_fn(tavg + 3)*(tavg_p_wheat_fn(tavg + 3)*tavg_dat$wheat_acres))) - ((tavg_wheat_fn(tavg)*(tavg_p_wheat_fn(tavg)*tavg_dat$wheat_acres))), na.rm = TRUE) +
 sum(((tavg_soybean_fn(tavg + 3)*(tavg_p_soybean_fn(tavg + 3)*tavg_dat$soybean_acres))) - ((tavg_soybean_fn(tavg)*(tavg_p_soybean_fn(tavg)*tavg_dat$soybean_acres))), na.rm = TRUE)

# Apply shift in temperature (+4C Average Temperature)
tavg_temp4 <- tavg_base + 
 sum(((tavg_corn_fn(tavg + 4)*(tavg_p_corn_fn(tavg + 4)*tavg_dat$corn_acres))) - ((tavg_corn_fn(tavg)*(tavg_p_corn_fn(tavg)*tavg_dat$corn_acres))), na.rm = TRUE) +
 sum(((tavg_cotton_fn(tavg + 4)*(tavg_p_cotton_fn(tavg + 4)*tavg_dat$cotton_acres))) - ((tavg_cotton_fn(tavg)*(tavg_p_cotton_fn(tavg)*tavg_dat$cotton_acres))), na.rm = TRUE) +
 sum(((tavg_hay_fn(tavg + 4)*(tavg_p_hay_fn(tavg + 4)*tavg_dat$hay_acres))) - ((tavg_hay_fn(tavg)*(tavg_p_hay_fn(tavg)*tavg_dat$hay_acres))), na.rm = TRUE) +
 sum(((tavg_wheat_fn(tavg + 4)*(tavg_p_wheat_fn(tavg + 4)*tavg_dat$wheat_acres))) - ((tavg_wheat_fn(tavg)*(tavg_p_wheat_fn(tavg)*tavg_dat$wheat_acres))), na.rm = TRUE) +
 sum(((tavg_soybean_fn(tavg + 4)*(tavg_p_soybean_fn(tavg + 4)*tavg_dat$soybean_acres))) - ((tavg_soybean_fn(tavg)*(tavg_p_soybean_fn(tavg)*tavg_dat$soybean_acres))), na.rm = TRUE)

# Apply shift in temperature (+5C Average Temperature)
tavg_temp5 <- tavg_base + 
 sum(((tavg_corn_fn(tavg + 5)*(tavg_p_corn_fn(tavg + 5)*tavg_dat$corn_acres))) - ((tavg_corn_fn(tavg)*(tavg_p_corn_fn(tavg)*tavg_dat$corn_acres))), na.rm = TRUE) +
 sum(((tavg_cotton_fn(tavg + 5)*(tavg_p_cotton_fn(tavg + 5)*tavg_dat$cotton_acres))) - ((tavg_cotton_fn(tavg)*(tavg_p_cotton_fn(tavg)*tavg_dat$cotton_acres))), na.rm = TRUE) +
 sum(((tavg_hay_fn(tavg + 5)*(tavg_p_hay_fn(tavg + 5)*tavg_dat$hay_acres))) - ((tavg_hay_fn(tavg)*(tavg_p_hay_fn(tavg)*tavg_dat$hay_acres))), na.rm = TRUE) +
 sum(((tavg_wheat_fn(tavg + 5)*(tavg_p_wheat_fn(tavg + 5)*tavg_dat$wheat_acres))) - ((tavg_wheat_fn(tavg)*(tavg_p_wheat_fn(tavg)*tavg_dat$wheat_acres))), na.rm = TRUE) +
 sum(((tavg_soybean_fn(tavg + 5)*(tavg_p_soybean_fn(tavg + 5)*tavg_dat$soybean_acres))) - ((tavg_soybean_fn(tavg)*(tavg_p_soybean_fn(tavg)*tavg_dat$soybean_acres))), na.rm = TRUE)

# Bind data
adapt4_tavg <- data.frame(temp = "Average Temperature (C)",
                          model = "Total Effect w/ Crop-switching",
                          rev = c(tavg_base, 
                                  tavg_temp1, 
                                  tavg_temp2, 
                                  tavg_temp3, 
                                  tavg_temp4, 
                                  tavg_temp5))

adapt4_tavg$change = (adapt4_tavg$rev - first(adapt4_tavg$rev))/first(adapt4_tavg$rev)*100

# adapt4_tavg
# plot(x=0:5, adapt4_tavg$change)
# lines(x=0:5, adapt4_tavg$change)



# Degree Days 10-30C and cut end points
dday10_30 <- dday10_30_dat$dday10_30
dday10_30[1] <- dday10_30_dat$dday10_30[1] + 0.1
dday10_30[length(dday10_30_dat$dday10_30)] <- dday10_30[length(dday10_30_dat$dday10_30)] - 0.1

# Get baseline
dday10_30_base <- sum(dday10_30_dat$corn_rev*dday10_30_dat$corn_acres) +
                  sum(dday10_30_dat$cotton_rev*dday10_30_dat$cotton_acres) +
                  sum(dday10_30_dat$hay_rev*dday10_30_dat$hay_acres) +
                  sum(dday10_30_dat$wheat_rev*dday10_30_dat$wheat_acres) +
                  sum(dday10_30_dat$soybean_rev*dday10_30_dat$soybean_acres)

# Apply shift in temperature (+140 Degree Days 10-30)
dday10_30_temp1 <- dday10_30_base + 
  sum(((dday10_30_corn_fn(dday10_30 + 140)*(dday10_30_p_corn_fn(dday10_30 + 140)*dday10_30_dat$corn_acres))) - ((dday10_30_corn_fn(dday10_30)*(dday10_30_p_corn_fn(dday10_30)*dday10_30_dat$corn_acres))), na.rm = TRUE) +
  sum(((dday10_30_cotton_fn(dday10_30 + 140)*(dday10_30_p_cotton_fn(dday10_30 + 140)*dday10_30_dat$cotton_acres))) - ((dday10_30_cotton_fn(dday10_30)*(dday10_30_p_cotton_fn(dday10_30)*dday10_30_dat$cotton_acres))), na.rm = TRUE) +
  sum(((dday10_30_hay_fn(dday10_30 + 140)*(dday10_30_p_hay_fn(dday10_30 + 140)*dday10_30_dat$hay_acres))) - ((dday10_30_hay_fn(dday10_30)*(dday10_30_p_hay_fn(dday10_30)*dday10_30_dat$hay_acres))), na.rm = TRUE) +
  sum(((dday10_30_wheat_fn(dday10_30 + 140)*(dday10_30_p_wheat_fn(dday10_30 + 140)*dday10_30_dat$wheat_acres))) - ((dday10_30_wheat_fn(dday10_30)*(dday10_30_p_wheat_fn(dday10_30)*dday10_30_dat$wheat_acres))), na.rm = TRUE) +
  sum(((dday10_30_soybean_fn(dday10_30 + 140)*(dday10_30_p_soybean_fn(dday10_30 + 140)*dday10_30_dat$soybean_acres))) - ((dday10_30_soybean_fn(dday10_30)*(dday10_30_p_soybean_fn(dday10_30)*dday10_30_dat$soybean_acres))), na.rm = TRUE)

# Apply shift in temperature (+280 Degree Days 10-30)
dday10_30_temp2 <- dday10_30_base + 
  sum(((dday10_30_corn_fn(dday10_30 + 280)*(dday10_30_p_corn_fn(dday10_30 + 280)*dday10_30_dat$corn_acres))) - ((dday10_30_corn_fn(dday10_30)*(dday10_30_p_corn_fn(dday10_30)*dday10_30_dat$corn_acres))), na.rm = TRUE) +
  sum(((dday10_30_cotton_fn(dday10_30 + 280)*(dday10_30_p_cotton_fn(dday10_30 + 280)*dday10_30_dat$cotton_acres))) - ((dday10_30_cotton_fn(dday10_30)*(dday10_30_p_cotton_fn(dday10_30)*dday10_30_dat$cotton_acres))), na.rm = TRUE) +
  sum(((dday10_30_hay_fn(dday10_30 + 280)*(dday10_30_p_hay_fn(dday10_30 + 280)*dday10_30_dat$hay_acres))) - ((dday10_30_hay_fn(dday10_30)*(dday10_30_p_hay_fn(dday10_30)*dday10_30_dat$hay_acres))), na.rm = TRUE) +
  sum(((dday10_30_wheat_fn(dday10_30 + 280)*(dday10_30_p_wheat_fn(dday10_30 + 280)*dday10_30_dat$wheat_acres))) - ((dday10_30_wheat_fn(dday10_30)*(dday10_30_p_wheat_fn(dday10_30)*dday10_30_dat$wheat_acres))), na.rm = TRUE) +
  sum(((dday10_30_soybean_fn(dday10_30 + 280)*(dday10_30_p_soybean_fn(dday10_30 + 280)*dday10_30_dat$soybean_acres))) - ((dday10_30_soybean_fn(dday10_30)*(dday10_30_p_soybean_fn(dday10_30)*dday10_30_dat$soybean_acres))), na.rm = TRUE)

# Apply shift in temperature (+420 Degree Days 10-30)
dday10_30_temp3 <- dday10_30_base + 
  sum(((dday10_30_corn_fn(dday10_30 +420)*(dday10_30_p_corn_fn(dday10_30 +420)*dday10_30_dat$corn_acres))) - ((dday10_30_corn_fn(dday10_30)*(dday10_30_p_corn_fn(dday10_30)*dday10_30_dat$corn_acres))), na.rm = TRUE) +
  sum(((dday10_30_cotton_fn(dday10_30 +420)*(dday10_30_p_cotton_fn(dday10_30 +420)*dday10_30_dat$cotton_acres))) - ((dday10_30_cotton_fn(dday10_30)*(dday10_30_p_cotton_fn(dday10_30)*dday10_30_dat$cotton_acres))), na.rm = TRUE) +
  sum(((dday10_30_hay_fn(dday10_30 +420)*(dday10_30_p_hay_fn(dday10_30 +420)*dday10_30_dat$hay_acres))) - ((dday10_30_hay_fn(dday10_30)*(dday10_30_p_hay_fn(dday10_30)*dday10_30_dat$hay_acres))), na.rm = TRUE) +
  sum(((dday10_30_wheat_fn(dday10_30 +420)*(dday10_30_p_wheat_fn(dday10_30 +420)*dday10_30_dat$wheat_acres))) - ((dday10_30_wheat_fn(dday10_30)*(dday10_30_p_wheat_fn(dday10_30)*dday10_30_dat$wheat_acres))), na.rm = TRUE) +
  sum(((dday10_30_soybean_fn(dday10_30 +420)*(dday10_30_p_soybean_fn(dday10_30 +420)*dday10_30_dat$soybean_acres))) - ((dday10_30_soybean_fn(dday10_30)*(dday10_30_p_soybean_fn(dday10_30)*dday10_30_dat$soybean_acres))), na.rm = TRUE)

# Apply shift in temperature (+560 Degree Days 10-30)
dday10_30_temp4 <- dday10_30_base + 
  sum(((dday10_30_corn_fn(dday10_30 + 560)*(dday10_30_p_corn_fn(dday10_30 + 560)*dday10_30_dat$corn_acres))) - ((dday10_30_corn_fn(dday10_30)*(dday10_30_p_corn_fn(dday10_30)*dday10_30_dat$corn_acres))), na.rm = TRUE) +
  sum(((dday10_30_cotton_fn(dday10_30 + 560)*(dday10_30_p_cotton_fn(dday10_30 + 560)*dday10_30_dat$cotton_acres))) - ((dday10_30_cotton_fn(dday10_30)*(dday10_30_p_cotton_fn(dday10_30)*dday10_30_dat$cotton_acres))), na.rm = TRUE) +
  sum(((dday10_30_hay_fn(dday10_30 + 560)*(dday10_30_p_hay_fn(dday10_30 + 560)*dday10_30_dat$hay_acres))) - ((dday10_30_hay_fn(dday10_30)*(dday10_30_p_hay_fn(dday10_30)*dday10_30_dat$hay_acres))), na.rm = TRUE) +
  sum(((dday10_30_wheat_fn(dday10_30 + 560)*(dday10_30_p_wheat_fn(dday10_30 + 560)*dday10_30_dat$wheat_acres))) - ((dday10_30_wheat_fn(dday10_30)*(dday10_30_p_wheat_fn(dday10_30)*dday10_30_dat$wheat_acres))), na.rm = TRUE) +
  sum(((dday10_30_soybean_fn(dday10_30 + 560)*(dday10_30_p_soybean_fn(dday10_30 + 560)*dday10_30_dat$soybean_acres))) - ((dday10_30_soybean_fn(dday10_30)*(dday10_30_p_soybean_fn(dday10_30)*dday10_30_dat$soybean_acres))), na.rm = TRUE)

# Apply shift in temperature (+700 Degree Days 10-30)
dday10_30_temp5 <- dday10_30_base + 
  sum(((dday10_30_corn_fn(dday10_30 + + 700)*(dday10_30_p_corn_fn(dday10_30 + 700)*dday10_30_dat$corn_acres))) - ((dday10_30_corn_fn(dday10_30)*(dday10_30_p_corn_fn(dday10_30)*dday10_30_dat$corn_acres))), na.rm = TRUE) +
  sum(((dday10_30_cotton_fn(dday10_30 + 700)*(dday10_30_p_cotton_fn(dday10_30 + 700)*dday10_30_dat$cotton_acres))) - ((dday10_30_cotton_fn(dday10_30)*(dday10_30_p_cotton_fn(dday10_30)*dday10_30_dat$cotton_acres))), na.rm = TRUE) +
  sum(((dday10_30_hay_fn(dday10_30 + 700)*(dday10_30_p_hay_fn(dday10_30 + 700)*dday10_30_dat$hay_acres))) - ((dday10_30_hay_fn(dday10_30)*(dday10_30_p_hay_fn(dday10_30)*dday10_30_dat$hay_acres))), na.rm = TRUE) +
  sum(((dday10_30_wheat_fn(dday10_30 + 700)*(dday10_30_p_wheat_fn(dday10_30 + 700)*dday10_30_dat$wheat_acres))) - ((dday10_30_wheat_fn(dday10_30)*(dday10_30_p_wheat_fn(dday10_30)*dday10_30_dat$wheat_acres))), na.rm = TRUE) +
  sum(((dday10_30_soybean_fn(dday10_30 + 700)*(dday10_30_p_soybean_fn(dday10_30 + 700)*dday10_30_dat$soybean_acres))) - ((dday10_30_soybean_fn(dday10_30)*(dday10_30_p_soybean_fn(dday10_30)*dday10_30_dat$soybean_acres))), na.rm = TRUE)

# Bind data
adapt4_dday10_30 <- data.frame(temp = "Degree Day (10-30C)",
                               model = "Total Effect w/ Crop-switching",
                               rev = c(dday10_30_base, 
                                       dday10_30_temp1, 
                                       dday10_30_temp2, 
                                       dday10_30_temp3, 
                                       dday10_30_temp4, 
                                       dday10_30_temp5))

adapt4_dday10_30$change = (adapt4_dday10_30$rev - first(adapt4_dday10_30$rev))/first(adapt4_dday10_30$rev)*100



# Degree Day 30C
dday30 <- dday30_dat$dday30
dday30[1] <- dday30_dat$dday30[1] + 0.1
dday30[length(dday30_dat$dday30)] <- dday30[length(dday30_dat$dday30)] - 0.1

# Get baseline
dday30_base <- sum(dday30_dat$corn_rev*dday30_dat$corn_acres) +
               sum(dday30_dat$cotton_rev*dday30_dat$cotton_acres) +
               sum(dday30_dat$hay_rev*dday30_dat$hay_acres) +
               sum(dday30_dat$wheat_rev*dday30_dat$wheat_acres) +
               sum(dday30_dat$soybean_rev*dday30_dat$soybean_acres)

# Apply shift in temperature (+15 Degree Days 30)
dday30_temp1 <- dday30_base + 
  sum(((dday30_corn_fn(dday30 + 15)*(dday30_p_corn_fn(dday30 + 15)*dday30_dat$corn_acres))) - ((dday30_corn_fn(dday30)*(dday30_p_corn_fn(dday30)*dday30_dat$corn_acres))), na.rm = TRUE) +
  sum(((dday30_cotton_fn(dday30 + 15)*(dday30_p_cotton_fn(dday30 + 15)*dday30_dat$cotton_acres))) - ((dday30_cotton_fn(dday30)*(dday30_p_cotton_fn(dday30)*dday30_dat$cotton_acres))), na.rm = TRUE) +
  sum(((dday30_hay_fn(dday30 + 15)*(dday30_p_hay_fn(dday30 + 15)*dday30_dat$hay_acres))) - ((dday30_hay_fn(dday30)*(dday30_p_hay_fn(dday30)*dday30_dat$hay_acres))), na.rm = TRUE) +
  sum(((dday30_wheat_fn(dday30 + 15)*(dday30_p_wheat_fn(dday30 + 15)*dday30_dat$wheat_acres))) - ((dday30_wheat_fn(dday30)*(dday30_p_wheat_fn(dday30)*dday30_dat$wheat_acres))), na.rm = TRUE) +
  sum(((dday30_soybean_fn(dday30 + 15)*(dday30_p_soybean_fn(dday30 + 15)*dday30_dat$soybean_acres))) - ((dday30_soybean_fn(dday30)*(dday30_p_soybean_fn(dday30)*dday30_dat$soybean_acres))), na.rm = TRUE)

# Apply shift in temperature (+30 Degree Days 30)
dday30_temp2 <- dday30_base + 
  sum(((dday30_corn_fn(dday30 + 30)*(dday30_p_corn_fn(dday30 + 30)*dday30_dat$corn_acres))) - ((dday30_corn_fn(dday30)*(dday30_p_corn_fn(dday30)*dday30_dat$corn_acres))), na.rm = TRUE) +
  sum(((dday30_cotton_fn(dday30 + 30)*(dday30_p_cotton_fn(dday30 + 30)*dday30_dat$cotton_acres))) - ((dday30_cotton_fn(dday30)*(dday30_p_cotton_fn(dday30)*dday30_dat$cotton_acres))), na.rm = TRUE) +
  sum(((dday30_hay_fn(dday30 + 30)*(dday30_p_hay_fn(dday30 + 30)*dday30_dat$hay_acres))) - ((dday30_hay_fn(dday30)*(dday30_p_hay_fn(dday30)*dday30_dat$hay_acres))), na.rm = TRUE) +
  sum(((dday30_wheat_fn(dday30 + 30)*(dday30_p_wheat_fn(dday30 + 30)*dday30_dat$wheat_acres))) - ((dday30_wheat_fn(dday30)*(dday30_p_wheat_fn(dday30)*dday30_dat$wheat_acres))), na.rm = TRUE) +
  sum(((dday30_soybean_fn(dday30 + 30)*(dday30_p_soybean_fn(dday30 + 30)*dday30_dat$soybean_acres))) - ((dday30_soybean_fn(dday30)*(dday30_p_soybean_fn(dday30)*dday30_dat$soybean_acres))), na.rm = TRUE)

# Apply shift in temperature (+45 Degree Days 30)
dday30_temp3 <- dday30_base + 
  sum(((dday30_corn_fn(dday30 + 45)*(dday30_p_corn_fn(dday30 + 45)*dday30_dat$corn_acres))) - ((dday30_corn_fn(dday30)*(dday30_p_corn_fn(dday30)*dday30_dat$corn_acres))), na.rm = TRUE) +
  sum(((dday30_cotton_fn(dday30 + 45)*(dday30_p_cotton_fn(dday30 + 45)*dday30_dat$cotton_acres))) - ((dday30_cotton_fn(dday30)*(dday30_p_cotton_fn(dday30)*dday30_dat$cotton_acres))), na.rm = TRUE) +
  sum(((dday30_hay_fn(dday30 + 45)*(dday30_p_hay_fn(dday30 + 45)*dday30_dat$hay_acres))) - ((dday30_hay_fn(dday30)*(dday30_p_hay_fn(dday30)*dday30_dat$hay_acres))), na.rm = TRUE) +
  sum(((dday30_wheat_fn(dday30 + 45)*(dday30_p_wheat_fn(dday30 + 45)*dday30_dat$wheat_acres))) - ((dday30_wheat_fn(dday30)*(dday30_p_wheat_fn(dday30)*dday30_dat$wheat_acres))), na.rm = TRUE) +
  sum(((dday30_soybean_fn(dday30 + 45)*(dday30_p_soybean_fn(dday30 + 45)*dday30_dat$soybean_acres))) - ((dday30_soybean_fn(dday30)*(dday30_p_soybean_fn(dday30)*dday30_dat$soybean_acres))), na.rm = TRUE)

# Apply shift in temperature (+60 Degree Days 30)
dday30_temp4 <- dday30_base + 
  sum(((dday30_corn_fn(dday30 + 60)*(dday30_p_corn_fn(dday30 + 60)*dday30_dat$corn_acres))) - ((dday30_corn_fn(dday30)*(dday30_p_corn_fn(dday30)*dday30_dat$corn_acres))), na.rm = TRUE) +
  sum(((dday30_cotton_fn(dday30 + 60)*(dday30_p_cotton_fn(dday30 + 60)*dday30_dat$cotton_acres))) - ((dday30_cotton_fn(dday30)*(dday30_p_cotton_fn(dday30)*dday30_dat$cotton_acres))), na.rm = TRUE) +
  sum(((dday30_hay_fn(dday30 + 60)*(dday30_p_hay_fn(dday30 + 60)*dday30_dat$hay_acres))) - ((dday30_hay_fn(dday30)*(dday30_p_hay_fn(dday30)*dday30_dat$hay_acres))), na.rm = TRUE) +
  sum(((dday30_wheat_fn(dday30 + 60)*(dday30_p_wheat_fn(dday30 + 60)*dday30_dat$wheat_acres))) - ((dday30_wheat_fn(dday30)*(dday30_p_wheat_fn(dday30)*dday30_dat$wheat_acres))), na.rm = TRUE) +
  sum(((dday30_soybean_fn(dday30 + 60)*(dday30_p_soybean_fn(dday30 + 60)*dday30_dat$soybean_acres))) - ((dday30_soybean_fn(dday30)*(dday30_p_soybean_fn(dday30)*dday30_dat$soybean_acres))), na.rm = TRUE)

# Apply shift in temperature (+75 Degree Days 30)
dday30_temp5 <- dday30_base + 
  sum(((dday30_corn_fn(dday30 + 75)*(dday30_p_corn_fn(dday30 + 75)*dday30_dat$corn_acres))) - ((dday30_corn_fn(dday30)*(dday30_p_corn_fn(dday30)*dday30_dat$corn_acres))), na.rm = TRUE) +
  sum(((dday30_cotton_fn(dday30 + 75)*(dday30_p_cotton_fn(dday30 + 75)*dday30_dat$cotton_acres))) - ((dday30_cotton_fn(dday30)*(dday30_p_cotton_fn(dday30)*dday30_dat$cotton_acres))), na.rm = TRUE) +
  sum(((dday30_hay_fn(dday30 + 75)*(dday30_p_hay_fn(dday30 + 75)*dday30_dat$hay_acres))) - ((dday30_hay_fn(dday30)*(dday30_p_hay_fn(dday30)*dday30_dat$hay_acres))), na.rm = TRUE) +
  sum(((dday30_wheat_fn(dday30 + 75)*(dday30_p_wheat_fn(dday30 + 75)*dday30_dat$wheat_acres))) - ((dday30_wheat_fn(dday30)*(dday30_p_wheat_fn(dday30)*dday30_dat$wheat_acres))), na.rm = TRUE) +
  sum(((dday30_soybean_fn(dday30 + 75)*(dday30_p_soybean_fn(dday30 + 75)*dday30_dat$soybean_acres))) - ((dday30_soybean_fn(dday30)*(dday30_p_soybean_fn(dday30)*dday30_dat$soybean_acres))), na.rm = TRUE)

# Bind data
adapt4_dday30 <- data.frame(temp = "Degree Day (30C)",
                            model = "Total Effect w/ Crop-switching",
                            rev = c(dday30_base, 
                                    dday30_temp1, 
                                    dday30_temp2, 
                                    dday30_temp3, 
                                    dday30_temp4, 
                                    dday30_temp5))

adapt4_dday30$change = (adapt4_dday30$rev - first(adapt4_dday30$rev))/first(adapt4_dday30$rev)*100






#----------------------------------------------------------------------------------
# Nonlinear effects without adaptation


# Average Temperatures and cut end points
tavg <- tavg_dat$tavg
tavg[1] <- tavg_dat$tavg[1] + 0.1
tavg[length(tavg_dat$tavg)] <- tavg[length(tavg_dat$tavg)] - 0.1

# Get baseline
tavg_base <- sum(tavg_dat$corn_rev*tavg_dat$corn_acres) +
             sum(tavg_dat$cotton_rev*tavg_dat$cotton_acres) +
             sum(tavg_dat$hay_rev*tavg_dat$hay_acres) +
             sum(tavg_dat$wheat_rev*tavg_dat$wheat_acres) +
             sum(tavg_dat$soybean_rev*tavg_dat$soybean_acres)

# Apply shift in temperature (+1C Average Temperature)
tavg_temp1 <- tavg_base + 
  (sum(grad(tavg_corn_fn, tavg)*tavg_dat$corn_acres) + 
          sum(grad(tavg_cotton_fn, tavg)*1*tavg_dat$cotton_acres) +
          sum(grad(tavg_hay_fn, tavg)*1*tavg_dat$hay_acres) +
          sum(grad(tavg_wheat_fn, tavg)*1*tavg_dat$wheat_acres) +
          sum(grad(tavg_soybean_fn, tavg)*1*tavg_dat$soybean_acres))

# Apply shift in temperature (+2C Average Temperature)
tavg_temp2 <- tavg_base + 
  (sum((2*grad(tavg_corn_fn, tavg))*tavg_dat$corn_acres) + 
          sum((2*grad(tavg_cotton_fn, tavg))*tavg_dat$cotton_acres) +
          sum((2*grad(tavg_hay_fn, tavg))*tavg_dat$hay_acres) +
          sum((2*grad(tavg_wheat_fn, tavg))*tavg_dat$wheat_acres) +
          sum((2*grad(tavg_soybean_fn, tavg))*tavg_dat$soybean_acres))

# Apply shift in temperature (+3C Average Temperature)
tavg_temp3 <- tavg_base +
  (sum((3*grad(tavg_corn_fn, tavg))*tavg_dat$corn_acres) + 
          sum((3*grad(tavg_cotton_fn, tavg))*tavg_dat$cotton_acres) +
          sum((3*grad(tavg_hay_fn, tavg))*tavg_dat$hay_acres) +
          sum((3*grad(tavg_wheat_fn, tavg))*tavg_dat$wheat_acres) +
          sum((3*grad(tavg_soybean_fn, tavg))*tavg_dat$soybean_acres))

# Apply shift in temperature (+4C Average Temperature)
tavg_temp4 <- tavg_base + 
  (sum((4*grad(tavg_corn_fn, tavg))*tavg_dat$corn_acres) + 
          sum((4*grad(tavg_cotton_fn, tavg))*tavg_dat$cotton_acres) +
          sum((4*grad(tavg_hay_fn, tavg))*tavg_dat$hay_acres) +
          sum((4*grad(tavg_wheat_fn, tavg))*tavg_dat$wheat_acres) +
          sum((4*grad(tavg_soybean_fn, tavg))*tavg_dat$soybean_acres))

# Apply shift in temperature (+5C Average Temperature)
tavg_temp5 <- tavg_base + 
  (sum((5*grad(tavg_corn_fn, tavg))*tavg_dat$corn_acres) + 
          sum((5*grad(tavg_cotton_fn, tavg))*tavg_dat$cotton_acres) +
          sum((5*grad(tavg_hay_fn, tavg))*tavg_dat$hay_acres) +
          sum((5*grad(tavg_wheat_fn, tavg))*tavg_dat$wheat_acres) +
          sum((5*grad(tavg_soybean_fn, tavg))*tavg_dat$soybean_acres))

# Bind data
adapt2_tavg <- data.frame(temp = "Average Temperature (C)",
                          model = "Adaptation w/o Crop-switching",
                     rev = c(tavg_base, 
                             tavg_temp1, 
                             tavg_temp2, 
                             tavg_temp3, 
                             tavg_temp4, 
                             tavg_temp5))

adapt2_tavg$change = (adapt2_tavg$rev - first(adapt2_tavg$rev))/first(adapt2_tavg$rev)*100



# Degree Days 10-30 and cut endpoints
dday10_30 <- dday10_30_dat$dday10_30
dday10_30[1] <- dday10_30_dat$dday10_30[1] + 6
dday10_30[length(dday10_30_dat$dday10_30)] <- dday10_30[length(dday10_30_dat$dday10_30)] - 70

# Get baseline
dday10_30_base <- sum(dday10_30_dat$corn_rev*dday10_30_dat$corn_acres) +
                  sum(dday10_30_dat$cotton_rev*dday10_30_dat$cotton_acres) +
                  sum(dday10_30_dat$hay_rev*dday10_30_dat$hay_acres) +
                  sum(dday10_30_dat$wheat_rev*dday10_30_dat$wheat_acres) +
                  sum(dday10_30_dat$soybean_rev*dday10_30_dat$soybean_acres)
   
# Apply shift in temperature (+140 Degree Days 10-30C)
dday10_30_temp1 <- dday10_30_base + 
  (sum(140*grad(dday10_30_corn_fn, dday10_30)*dday10_30_dat$corn_acres) + 
    sum(140*grad(dday10_30_cotton_fn, dday10_30)*dday10_30_dat$cotton_acres) +
    sum(140*grad(dday10_30_hay_fn, dday10_30)*dday10_30_dat$hay_acres) +
    sum(140*grad(dday10_30_wheat_fn, dday10_30)*dday10_30_dat$wheat_acres) +
    sum(140*grad(dday10_30_soybean_fn, dday10_30)*dday10_30_dat$soybean_acres))

# Apply shift in temperature (+280 Degree Days 10-30C)
dday10_30_temp2 <- dday10_30_base + 
 (sum((280*grad(dday10_30_corn_fn, dday10_30))*dday10_30_dat$corn_acres) + 
 sum((280*grad(dday10_30_cotton_fn, dday10_30))*dday10_30_dat$cotton_acres) +
 sum((280*grad(dday10_30_hay_fn, dday10_30))*dday10_30_dat$hay_acres) +
 sum((280*grad(dday10_30_wheat_fn, dday10_30))*dday10_30_dat$wheat_acres) +
 sum((280*grad(dday10_30_soybean_fn, dday10_30))*dday10_30_dat$soybean_acres))

# Apply shift in temperature (+420 Degree Days 10-30C)
dday10_30_temp3 <- dday10_30_base + 
 (sum((420*grad(dday10_30_corn_fn, dday10_30))*dday10_30_dat$corn_acres) + 
 sum((420*grad(dday10_30_cotton_fn, dday10_30))*dday10_30_dat$cotton_acres) +
 sum((420*grad(dday10_30_hay_fn, dday10_30))*dday10_30_dat$hay_acres) +
 sum((420*grad(dday10_30_wheat_fn, dday10_30))*dday10_30_dat$wheat_acres) +
 sum((420*grad(dday10_30_soybean_fn, dday10_30))*dday10_30_dat$soybean_acres))

# Apply shift in temperature (+560 Degree Days 10-30C)
dday10_30_temp4 <- dday10_30_base +
 (sum((560*grad(dday10_30_corn_fn, dday10_30))*dday10_30_dat$corn_acres) + 
 sum((560*grad(dday10_30_cotton_fn, dday10_30))*dday10_30_dat$cotton_acres) +
 sum((560*grad(dday10_30_hay_fn, dday10_30))*dday10_30_dat$hay_acres) +
 sum((560*grad(dday10_30_wheat_fn, dday10_30))*dday10_30_dat$wheat_acres) +
 sum((560*grad(dday10_30_soybean_fn, dday10_30))*dday10_30_dat$soybean_acres))

# Apply shift in temperature (+700 Degree Days 10-30C)
dday10_30_temp5 <- dday10_30_base + 
 (sum((700*grad(dday10_30_corn_fn, dday10_30))*dday10_30_dat$corn_acres) + 
 sum((700*grad(dday10_30_cotton_fn, dday10_30))*dday10_30_dat$cotton_acres) +
 sum((700*grad(dday10_30_hay_fn, dday10_30))*dday10_30_dat$hay_acres) +
 sum((700*grad(dday10_30_wheat_fn, dday10_30))*dday10_30_dat$wheat_acres) +
 sum((700*grad(dday10_30_soybean_fn, dday10_30))*dday10_30_dat$soybean_acres))

# Binddata
adapt2_dday10_30 <- data.frame(temp = "Degree Day (10-30C)",
                          model = "Adaptation w/o Crop-switching",
                     rev = c(dday10_30_base, 
                             dday10_30_temp1, 
                             dday10_30_temp2, 
                             dday10_30_temp3, 
                             dday10_30_temp4, 
                             dday10_30_temp5))

adapt2_dday10_30$change = 100*((adapt2_dday10_30$rev - first(adapt2_dday10_30$rev))/first(adapt2_dday10_30$rev))



# Degree Days 30 and cut end points
dday30 <- dday30_dat$dday30
dday30[1] <- dday30_dat$dday30[1] + 0.1
dday30[length(dday30_dat$dday30)] <- dday30[length(dday30_dat$dday30)] - 0.1

# Get baseline
dday30_base <- sum(dday30_dat$corn_rev*dday30_dat$corn_acres) +
               sum(dday30_dat$cotton_rev*dday30_dat$cotton_acres) +
               sum(dday30_dat$hay_rev*dday30_dat$hay_acres) +
               sum(dday30_dat$wheat_rev*dday30_dat$wheat_acres) +
               sum(dday30_dat$soybean_rev*dday30_dat$soybean_acres)

# Apply shift in temperature (+15 Degree Days 30C)
dday30_temp1 <- dday30_base + 
  (sum(15*grad(dday30_corn_fn, dday30)*dday30_dat$corn_acres) + 
  sum(15*grad(dday30_cotton_fn, dday30)*dday30_dat$cotton_acres) +
  sum(15*grad(dday30_hay_fn, dday30)*dday30_dat$hay_acres) +
  sum(15*grad(dday30_wheat_fn, dday30)*dday30_dat$wheat_acres) +
  sum(15*grad(dday30_soybean_fn, dday30)*dday30_dat$soybean_acres))

# Apply shift in temperature (+30 Degree Days 30C)
dday30_temp2 <- dday30_base + (sum((30*grad(dday30_corn_fn, dday30))*dday30_dat$corn_acres) + 
  sum((30*grad(dday30_cotton_fn, dday30))*dday30_dat$cotton_acres) +
  sum((30*grad(dday30_hay_fn, dday30))*dday30_dat$hay_acres) +
  sum((30*grad(dday30_wheat_fn, dday30))*dday30_dat$wheat_acres) +
  sum((30*grad(dday30_soybean_fn, dday30))*dday30_dat$soybean_acres))

# Apply shift in temperature (+45 Degree Days 30C)
dday30_temp3 <- dday30_base + (sum((45*grad(dday30_corn_fn, dday30))*dday30_dat$corn_acres) + 
  sum((45*grad(dday30_cotton_fn, dday30))*dday30_dat$cotton_acres) +
  sum((45*grad(dday30_hay_fn, dday30))*dday30_dat$hay_acres) +
  sum((45*grad(dday30_wheat_fn, dday30))*dday30_dat$wheat_acres) +
  sum((45*grad(dday30_soybean_fn, dday30))*dday30_dat$soybean_acres))

# Apply shift in temperature (+60 Degree Days 30C)
dday30_temp4 <- dday30_base + (sum((60*grad(dday30_corn_fn, dday30))*dday30_dat$corn_acres) + 
  sum((60*grad(dday30_cotton_fn, dday30))*dday30_dat$cotton_acres) +
  sum((60*grad(dday30_hay_fn, dday30))*dday30_dat$hay_acres) +
  sum((60*grad(dday30_wheat_fn, dday30))*dday30_dat$wheat_acres) +
  sum((60*grad(dday30_soybean_fn, dday30))*dday30_dat$soybean_acres))

# Apply shift in temperature (+75 Degree Days 30C)
dday30_temp5 <- dday30_base + (sum((75*grad(dday30_corn_fn, dday30))*dday30_dat$corn_acres) + 
  sum((75*grad(dday30_cotton_fn, dday30))*dday30_dat$cotton_acres) +
  sum((75*grad(dday30_hay_fn, dday30))*dday30_dat$hay_acres) +
  sum((75*grad(dday30_wheat_fn, dday30))*dday30_dat$wheat_acres) +
  sum((75*grad(dday30_soybean_fn, dday30))*dday30_dat$soybean_acres))

# Bind data
adapt2_dday30 <- data.frame(temp = "Degree Day (30C)",
                          model = "Adaptation w/o Crop-switching",
                     rev = c(dday30_base, 
                             dday30_temp1, 
                             dday30_temp2, 
                             dday30_temp3, 
                             dday30_temp4, 
                             dday30_temp5))

adapt2_dday30$change = 100*(adapt2_dday30$rev - first(adapt2_dday30$rev))/first(adapt2_dday30$rev)


#----------------------------------------------------------------------------------
# Nonlinear effects with adaptation

# Average Temperatures and cut end points
tavg <- tavg_dat$tavg
tavg[1] <- tavg_dat$tavg[1] + 0.1
tavg[length(tavg_dat$tavg)] <- tavg[length(tavg_dat$tavg)] - 0.1

# Get baseline
tavg_base <- sum(tavg_dat$corn_rev*tavg_dat$corn_acres) +
        sum(tavg_dat$cotton_rev*tavg_dat$cotton_acres) +
        sum(tavg_dat$hay_rev*tavg_dat$hay_acres) +
        sum(tavg_dat$wheat_rev*tavg_dat$wheat_acres) +
        sum(tavg_dat$soybean_rev*tavg_dat$soybean_acres)
  
# Apply shift in temperature (+1C Average Temperature)
tavg_temp1 <- tavg_base + 
  (sum(grad(tavg_corn_fn, tavg)*(grad(tavg_p_corn_fn, tavg)*tavg_dat$corn_acres + tavg_dat$corn_acres))) + 
  (sum(grad(tavg_cotton_fn, tavg)*(grad(tavg_p_cotton_fn, tavg)*tavg_dat$cotton_acres + tavg_dat$cotton_acres))) + 
  (sum(grad(tavg_hay_fn, tavg)*(grad(tavg_p_hay_fn, tavg)*tavg_dat$hay_acres + tavg_dat$hay_acres))) + 
  (sum(grad(tavg_wheat_fn, tavg)*(grad(tavg_p_wheat_fn, tavg)*tavg_dat$wheat_acres + tavg_dat$wheat_acres))) + 
  (sum(grad(tavg_soybean_fn, tavg)*(grad(tavg_p_soybean_fn, tavg)*tavg_dat$soybean_acres + tavg_dat$soybean_acres))) 

# Apply shift in temperature (+2C Average Temperature)
tavg_temp2 <- tavg_base + 
  (sum(2*grad(tavg_corn_fn, tavg)*(2*grad(tavg_p_corn_fn, tavg)*tavg_dat$corn_acres + tavg_dat$corn_acres))) + 
  (sum(2*grad(tavg_cotton_fn, tavg)*(2*grad(tavg_p_cotton_fn, tavg)*tavg_dat$cotton_acres + tavg_dat$cotton_acres))) + 
  (sum(2*grad(tavg_hay_fn, tavg)*(2*grad(tavg_p_hay_fn, tavg)*tavg_dat$hay_acres + tavg_dat$hay_acres))) + 
  (sum(2*grad(tavg_wheat_fn, tavg)*(2*grad(tavg_p_wheat_fn, tavg)*tavg_dat$wheat_acres + tavg_dat$wheat_acres))) + 
  (sum(2*grad(tavg_soybean_fn, tavg)*(2*grad(tavg_p_soybean_fn, tavg)*tavg_dat$soybean_acres + tavg_dat$soybean_acres))) 

# Apply shift in temperature (+3C Average Temperature)
tavg_temp3 <- tavg_base + 
  (sum(3*grad(tavg_corn_fn, tavg)*(3*grad(tavg_p_corn_fn, tavg)*tavg_dat$corn_acres + tavg_dat$corn_acres))) + 
  (sum(3*grad(tavg_cotton_fn, tavg)*(3*grad(tavg_p_cotton_fn, tavg)*tavg_dat$cotton_acres + tavg_dat$cotton_acres))) + 
  (sum(3*grad(tavg_hay_fn, tavg)*(3*grad(tavg_p_hay_fn, tavg)*tavg_dat$hay_acres + tavg_dat$hay_acres))) + 
  (sum(3*grad(tavg_wheat_fn, tavg)*(3*grad(tavg_p_wheat_fn, tavg)*tavg_dat$wheat_acres + tavg_dat$wheat_acres))) + 
  (sum(3*grad(tavg_soybean_fn, tavg)*(3*grad(tavg_p_soybean_fn, tavg)*tavg_dat$soybean_acres + tavg_dat$soybean_acres))) 

# Apply shift in temperature (+4C Average Temperature)
tavg_temp4 <- tavg_base + 
  (sum(4*grad(tavg_corn_fn, tavg)*(4*grad(tavg_p_corn_fn, tavg)*tavg_dat$corn_acres + tavg_dat$corn_acres))) + 
  (sum(4*grad(tavg_cotton_fn, tavg)*(grad(tavg_p_cotton_fn, tavg)*tavg_dat$cotton_acres + tavg_dat$cotton_acres))) + 
  (sum(4*grad(tavg_hay_fn, tavg)*(4*grad(tavg_p_hay_fn, tavg)*tavg_dat$hay_acres + tavg_dat$hay_acres))) + 
  (sum(4*grad(tavg_wheat_fn, tavg)*(4*grad(tavg_p_wheat_fn, tavg)*tavg_dat$wheat_acres + tavg_dat$wheat_acres))) + 
  (sum(4*grad(tavg_soybean_fn, tavg)*(4*grad(tavg_p_soybean_fn, tavg)*tavg_dat$soybean_acres + tavg_dat$soybean_acres))) 

# Apply shift in temperature (+5C Average Temperature)
tavg_temp5 <- tavg_base + 
  (sum(5*grad(tavg_corn_fn, tavg)*(5*grad(tavg_p_corn_fn, tavg)*tavg_dat$corn_acres + tavg_dat$corn_acres))) + 
  (sum(5*grad(tavg_cotton_fn, tavg)*(5*grad(tavg_p_cotton_fn, tavg)*tavg_dat$cotton_acres + tavg_dat$cotton_acres))) + 
  (sum(5*grad(tavg_hay_fn, tavg)*(5*grad(tavg_p_hay_fn, tavg)*tavg_dat$hay_acres + tavg_dat$hay_acres))) + 
  (sum(5*grad(tavg_wheat_fn, tavg)*(5*grad(tavg_p_wheat_fn, tavg)*tavg_dat$wheat_acres + tavg_dat$wheat_acres))) + 
  (sum(5*grad(tavg_soybean_fn, tavg)*(5*grad(tavg_p_soybean_fn, tavg)*tavg_dat$soybean_acres + tavg_dat$soybean_acres))) 

# Bind data
adapt3_tavg <- data.frame(temp = "Average Temperature (C)",
                          model = "Adaptation w/ Crop-switching",
                     rev = c(tavg_base, 
                             tavg_temp1, 
                             tavg_temp2, 
                             tavg_temp3, 
                             tavg_temp4, 
                             tavg_temp5))

adapt3_tavg$change = (adapt3_tavg$rev - first(adapt3_tavg$rev))/first(adapt3_tavg$rev)*100


# Degree Days 10-30C and cut end points
dday10_30 <- dday10_30_dat$dday10_30
dday10_30[1] <- dday10_30_dat$dday10_30[1] + 6
dday10_30[length(dday10_30_dat$dday10_30)] <- dday10_30[length(dday10_30_dat$dday10_30)] - 6

# Get baseline
dday10_30_base <- sum(dday10_30_dat$corn_rev*dday10_30_dat$corn_acres) +
                  sum(dday10_30_dat$cotton_rev*dday10_30_dat$cotton_acres) +
                  sum(dday10_30_dat$hay_rev*dday10_30_dat$hay_acres) +
                  sum(dday10_30_dat$wheat_rev*dday10_30_dat$wheat_acres) +
                  sum(dday10_30_dat$soybean_rev*dday10_30_dat$soybean_acres)
  
# Apply shift in temperature (+140 Degree Days 10-30C)
dday10_30_temp1 <- dday10_30_base + 
  (sum(140*grad(dday10_30_corn_fn, dday10_30)*(140*grad(dday10_30_p_corn_fn, dday10_30)*dday10_30_dat$corn_acres + dday10_30_dat$corn_acres))) + 
  (sum(140*grad(dday10_30_cotton_fn, dday10_30)*(140*grad(dday10_30_p_cotton_fn, dday10_30)*dday10_30_dat$cotton_acres + dday10_30_dat$cotton_acres))) + 
  (sum(140*grad(dday10_30_hay_fn, dday10_30)*(140*grad(dday10_30_p_hay_fn, dday10_30)*dday10_30_dat$hay_acres + dday10_30_dat$hay_acres))) + 
  (sum(140*grad(dday10_30_wheat_fn, dday10_30)*(140*grad(dday10_30_p_wheat_fn, dday10_30)*dday10_30_dat$wheat_acres + dday10_30_dat$wheat_acres))) + 
  (sum(140*grad(dday10_30_soybean_fn, dday10_30)*(140*grad(dday10_30_p_soybean_fn, dday10_30)*dday10_30_dat$soybean_acres + dday10_30_dat$soybean_acres))) 

# Apply shift in temperature (+280 Degree Days 10-30C)
dday10_30_temp2 <- dday10_30_base + 
  (sum(280*grad(dday10_30_corn_fn, dday10_30)*(280*grad(dday10_30_p_corn_fn, dday10_30)*dday10_30_dat$corn_acres + dday10_30_dat$corn_acres))) + 
  (sum(280*grad(dday10_30_cotton_fn, dday10_30)*(280*grad(dday10_30_p_cotton_fn, dday10_30)*dday10_30_dat$cotton_acres + dday10_30_dat$cotton_acres))) + 
  (sum(280*grad(dday10_30_hay_fn, dday10_30)*(280*grad(dday10_30_p_hay_fn, dday10_30)*dday10_30_dat$hay_acres + dday10_30_dat$hay_acres))) + 
  (sum(280*grad(dday10_30_wheat_fn, dday10_30)*(280*grad(dday10_30_p_wheat_fn, dday10_30)*dday10_30_dat$wheat_acres + dday10_30_dat$wheat_acres))) + 
  (sum(280*grad(dday10_30_soybean_fn, dday10_30)*(280*grad(dday10_30_p_soybean_fn, dday10_30)*dday10_30_dat$soybean_acres + dday10_30_dat$soybean_acres))) 

# Apply shift in temperature (+420 Degree Days 10-30C)
dday10_30_temp3 <- dday10_30_base + 
  (sum(420*grad(dday10_30_corn_fn, dday10_30)*(420*grad(dday10_30_p_corn_fn, dday10_30)*dday10_30_dat$corn_acres + dday10_30_dat$corn_acres))) + 
  (sum(420*grad(dday10_30_cotton_fn, dday10_30)*(420*grad(dday10_30_p_cotton_fn, dday10_30)*dday10_30_dat$cotton_acres + dday10_30_dat$cotton_acres))) + 
  (sum(420*grad(dday10_30_hay_fn, dday10_30)*(420*grad(dday10_30_p_hay_fn, dday10_30)*dday10_30_dat$hay_acres + dday10_30_dat$hay_acres))) + 
  (sum(420*grad(dday10_30_wheat_fn, dday10_30)*(420*grad(dday10_30_p_wheat_fn, dday10_30)*dday10_30_dat$wheat_acres + dday10_30_dat$wheat_acres))) + 
  (sum(420*grad(dday10_30_soybean_fn, dday10_30)*(420*grad(dday10_30_p_soybean_fn, dday10_30)*dday10_30_dat$soybean_acres + dday10_30_dat$soybean_acres))) 

# Apply shift in temperature (+560 Degree Days 10-30C)
  dday10_30_temp4 <- dday10_30_base + 
  (sum(560*grad(dday10_30_corn_fn, dday10_30)*(560*grad(dday10_30_p_corn_fn, dday10_30)*dday10_30_dat$corn_acres + dday10_30_dat$corn_acres))) + 
  (sum(560*grad(dday10_30_cotton_fn, dday10_30)*(grad(dday10_30_p_cotton_fn, dday10_30)*dday10_30_dat$cotton_acres + dday10_30_dat$cotton_acres))) + 
  (sum(560*grad(dday10_30_hay_fn, dday10_30)*(560*grad(dday10_30_p_hay_fn, dday10_30)*dday10_30_dat$hay_acres + dday10_30_dat$hay_acres))) + 
  (sum(560*grad(dday10_30_wheat_fn, dday10_30)*(560*grad(dday10_30_p_wheat_fn, dday10_30)*dday10_30_dat$wheat_acres + dday10_30_dat$wheat_acres))) + 
  (sum(560*grad(dday10_30_soybean_fn, dday10_30)*(560*grad(dday10_30_p_soybean_fn, dday10_30)*dday10_30_dat$soybean_acres + dday10_30_dat$soybean_acres))) 

# Apply shift in temperature (+700 Degree Days 10-30C)
dday10_30_temp5 <- dday10_30_base + 
  (sum(700*grad(dday10_30_corn_fn, dday10_30)*(700*grad(dday10_30_p_corn_fn, dday10_30)*dday10_30_dat$corn_acres + dday10_30_dat$corn_acres))) + 
  (sum(700*grad(dday10_30_cotton_fn, dday10_30)*(700*grad(dday10_30_p_cotton_fn, dday10_30)*dday10_30_dat$cotton_acres + dday10_30_dat$cotton_acres))) + 
  (sum(700*grad(dday10_30_hay_fn, dday10_30)*(700*grad(dday10_30_p_hay_fn, dday10_30)*dday10_30_dat$hay_acres + dday10_30_dat$hay_acres))) + 
  (sum(700*grad(dday10_30_wheat_fn, dday10_30)*(700*grad(dday10_30_p_wheat_fn, dday10_30)*dday10_30_dat$wheat_acres + dday10_30_dat$wheat_acres))) + 
  (sum(700*grad(dday10_30_soybean_fn, dday10_30)*(700*grad(dday10_30_p_soybean_fn, dday10_30)*dday10_30_dat$soybean_acres + dday10_30_dat$soybean_acres))) 

# Bind data
adapt3_dday10_30 <- data.frame(temp = "Degree Day (10-30C)",
                          model = "Adaptation w/ Crop-switching",
                     rev = c(dday10_30_base, 
                             dday10_30_temp1, 
                             dday10_30_temp2, 
                             dday10_30_temp3, 
                             dday10_30_temp4, 
                             dday10_30_temp5))

adapt3_dday10_30$change = (adapt3_dday10_30$rev - first(adapt3_dday10_30$rev))/first(adapt3_dday10_30$rev)*100




# Degree Days 10-30C and cut end point
dday30 <- dday30_dat$dday30
dday30[1] <- dday30_dat$dday30[1] + 0.1
dday30[length(dday30_dat$dday30)] <- dday30[length(dday30_dat$dday30)] - 0.1

# Get baseline
dday30_base <- sum(dday30_dat$corn_rev*dday30_dat$corn_acres) +
                sum(dday30_dat$cotton_rev*dday30_dat$cotton_acres) +
                sum(dday30_dat$hay_rev*dday30_dat$hay_acres) +
                sum(dday30_dat$wheat_rev*dday30_dat$wheat_acres) +
                sum(dday30_dat$soybean_rev*dday30_dat$soybean_acres)
  
# Apply shift in temperature (+15 Degree Days 30C)
dday30_temp1 <- dday30_base + 
  (sum(15*grad(dday30_corn_fn, dday30)*(15*grad(dday30_p_corn_fn, dday30)*dday30_dat$corn_acres + dday30_dat$corn_acres))) + 
  (sum(15*grad(dday30_cotton_fn, dday30)*(15*grad(dday30_p_cotton_fn, dday30)*dday30_dat$cotton_acres + dday30_dat$cotton_acres))) + 
  (sum(15*grad(dday30_hay_fn, dday30)*(15*grad(dday30_p_hay_fn, dday30)*dday30_dat$hay_acres + dday30_dat$hay_acres))) + 
  (sum(15*grad(dday30_wheat_fn, dday30)*(15*grad(dday30_p_wheat_fn, dday30)*dday30_dat$wheat_acres + dday30_dat$wheat_acres))) + 
  (sum(15*grad(dday30_soybean_fn, dday30)*(15*grad(dday30_p_soybean_fn, dday30)*dday30_dat$soybean_acres + dday30_dat$soybean_acres))) 

# Apply shift in temperature (+30 Degree Days 30C)
dday30_temp2 <- dday30_base + 
  (sum(30*grad(dday30_corn_fn, dday30)*(30*grad(dday30_p_corn_fn, dday30)*dday30_dat$corn_acres + dday30_dat$corn_acres))) + 
  (sum(30*grad(dday30_cotton_fn, dday30)*(30*grad(dday30_p_cotton_fn, dday30)*dday30_dat$cotton_acres + dday30_dat$cotton_acres))) + 
  (sum(30*grad(dday30_hay_fn, dday30)*(30*grad(dday30_p_hay_fn, dday30)*dday30_dat$hay_acres + dday30_dat$hay_acres))) + 
  (sum(30*grad(dday30_wheat_fn, dday30)*(30*grad(dday30_p_wheat_fn, dday30)*dday30_dat$wheat_acres + dday30_dat$wheat_acres))) + 
  (sum(30*grad(dday30_soybean_fn, dday30)*(30*grad(dday30_p_soybean_fn, dday30)*dday30_dat$soybean_acres + dday30_dat$soybean_acres))) 

# Apply shift in temperature (+45 Degree Days 30C)
dday30_temp3 <- dday30_base + 
  (sum(45*grad(dday30_corn_fn, dday30)*(45*grad(dday30_p_corn_fn, dday30)*dday30_dat$corn_acres + dday30_dat$corn_acres))) + 
  (sum(45*grad(dday30_cotton_fn, dday30)*(45*grad(dday30_p_cotton_fn, dday30)*dday30_dat$cotton_acres + dday30_dat$cotton_acres))) + 
  (sum(45*grad(dday30_hay_fn, dday30)*(45*grad(dday30_p_hay_fn, dday30)*dday30_dat$hay_acres + dday30_dat$hay_acres))) + 
  (sum(45*grad(dday30_wheat_fn, dday30)*(45*grad(dday30_p_wheat_fn, dday30)*dday30_dat$wheat_acres + dday30_dat$wheat_acres))) + 
  (sum(45*grad(dday30_soybean_fn, dday30)*(45*grad(dday30_p_soybean_fn, dday30)*dday30_dat$soybean_acres + dday30_dat$soybean_acres))) 

# Apply shift in temperature (+60 Degree Days 30C)
dday30_temp4 <- dday30_base + 
  (sum(60*grad(dday30_corn_fn, dday30)*(60*grad(dday30_p_corn_fn, dday30)*dday30_dat$corn_acres + dday30_dat$corn_acres))) + 
  (sum(60*grad(dday30_cotton_fn, dday30)*(grad(dday30_p_cotton_fn, dday30)*dday30_dat$cotton_acres + dday30_dat$cotton_acres))) + 
  (sum(60*grad(dday30_hay_fn, dday30)*(60*grad(dday30_p_hay_fn, dday30)*dday30_dat$hay_acres + dday30_dat$hay_acres))) + 
  (sum(60*grad(dday30_wheat_fn, dday30)*(60*grad(dday30_p_wheat_fn, dday30)*dday30_dat$wheat_acres + dday30_dat$wheat_acres))) + 
  (sum(60*grad(dday30_soybean_fn, dday30)*(60*grad(dday30_p_soybean_fn, dday30)*dday30_dat$soybean_acres + dday30_dat$soybean_acres))) 

# Apply shift in temperature (+75 Degree Days 30C)
dday30_temp5 <- dday30_base + 
  (sum(75*grad(dday30_corn_fn, dday30)*(75*grad(dday30_p_corn_fn, dday30)*dday30_dat$corn_acres + dday30_dat$corn_acres))) + 
  (sum(75*grad(dday30_cotton_fn, dday30)*(75*grad(dday30_p_cotton_fn, dday30)*dday30_dat$cotton_acres + dday30_dat$cotton_acres))) + 
  (sum(75*grad(dday30_hay_fn, dday30)*(75*grad(dday30_p_hay_fn, dday30)*dday30_dat$hay_acres + dday30_dat$hay_acres))) + 
  (sum(75*grad(dday30_wheat_fn, dday30)*(75*grad(dday30_p_wheat_fn, dday30)*dday30_dat$wheat_acres + dday30_dat$wheat_acres))) + 
  (sum(75*grad(dday30_soybean_fn, dday30)*(75*grad(dday30_p_soybean_fn, dday30)*dday30_dat$soybean_acres + dday30_dat$soybean_acres))) 

# Bind data
adapt3_dday30 <- data.frame(temp = "Degree Day (30C)",
                          model = "Adaptation w/ Crop-switching",
                         rev = c(dday30_base, 
                                 dday30_temp1, 
                                 dday30_temp2, 
                                 dday30_temp3, 
                                 dday30_temp4, 
                                 dday30_temp5))

adapt3_dday30$change = (adapt3_dday30$rev - first(adapt3_dday30$rev))/first(adapt3_dday30$rev)*100



# [4] Bind all data
#----------------------------------------------------------------------------------


pdat <- rbind(adapt1_tavg, adapt1_dday10_30, adapt1_dday30, 
              adapt2_tavg, adapt2_dday10_30, adapt2_dday30,
              adapt3_tavg, adapt3_dday10_30, adapt3_dday30)
# Set Increase in C
pdat$c <- seq(0,5)

# Build data.frame for plot
pdat <- rbind(adapt1_tavg, adapt1_dday10_30, adapt1_dday30, 
              adapt2_tavg, adapt2_dday10_30, adapt2_dday30,
              # adapt3_tavg, adapt3_dday10_30, adapt3_dday30,  # Do not display ME w/adapt
              adapt4_tavg, adapt4_dday10_30, adapt4_dday30)
pdat$c <- seq(0,5)



# Average temperature main plot
atext2 <- data.frame(temp = "Average Temperature (C)",
                    x = c(3.3, 2.7, 3.7),
                    y = c(-14, -24, -7),
                    model= c(
                             #"Marginal Effect \n w/ Adaptation", 
                             "Marginal Effect \n w/o Adaptation", 
                             "NL w/o Adaptation", 
                             "NL w/ Adaptation"))
