library(tidyverse)

setwd("~/Projects/adaptation_numerical_example/")

# Number of BINS in data
BINS <- c(5, 10, 20, 30)

slope <- function(x1, y1, x2, y2){
  m = (y2 - y1)/(x2 - x1)
  return(m)
}

remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

# Load data east of 100th degree
mdat <- as.data.frame(readRDS("data/full_ag_data.rds"))
mdat <- filter(mdat, abs(long) <= 100)

for (i in BINS){
  
  dat <- mdat %>% 
    group_by(fips) %>% 
    summarise(corn_yield = mean(corn_yield, na.rm = TRUE),
              corn_mprice = mean(corn_rprice, na.rm = TRUE),
              corn_a = mean(corn_grain_a, na.rm = TRUE),
              cotton_yield = mean(cotton_yield, na.rm = TRUE),
              cotton_mprice = mean(cotton_rprice, na.rm = TRUE),
              cotton_a = mean(cotton_a, na.rm = TRUE),
              hay_yield = mean(hay_yield, na.rm = TRUE),
              hay_mprice = mean(hay_rprice, na.rm = TRUE),
              hay_a = mean(hay_a, na.rm = TRUE),
              soybean_yield = mean(soybean_yield, na.rm = TRUE),
              soybean_mprice = mean(soybean_rprice, na.rm = TRUE),
              soybean_a = mean(soybean_a, na.rm = TRUE),
              wheat_yield = mean(wheat_yield, na.rm = TRUE),
              wheat_mprice = mean(wheat_rprice, na.rm = TRUE),
              wheat_a = mean(wheat_a, na.rm = TRUE),
              corn_mrev = mean(corn_mrev, na.rm = TRUE),
              cotton_mrev = mean(cotton_mrev, na.rm = TRUE),
              hay_mrev = mean(hay_mrev, na.rm = TRUE),
              wheat_mrev = mean(wheat_mrev, na.rm = TRUE),
              soybean_mrev = mean(soybean_mrev, na.rm = TRUE),
              tavg = mean(tavg, na.rm = TRUE),
              dday10_30 = mean(dday10_30, na.rm = TRUE),
              dday30 = mean(dday30, na.rm = TRUE),
              prec = mean(prec, na.rm = TRUE),
              corn_a = mean(corn_a, na.rm = TRUE),
              p_corn_a = mean(p_corn_a, na.rm = TRUE),
              p_cotton_a = mean(p_cotton_a, na.rm = TRUE),
              p_hay_a = mean(p_hay_a, na.rm = TRUE),
              p_wheat_a = mean(p_wheat_a, na.rm = TRUE),
              p_soybean_a = mean(p_soybean_a, na.rm = TRUE))
  
  dat$corn_rev <- dat$corn_yield*mean(mdat$corn_rprice, na.rm = TRUE)
  dat$cotton_rev <- dat$cotton_yield*mean(mdat$cotton_rprice, na.rm = TRUE)
  dat$hay_rev <- dat$hay_yield*mean(mdat$hay_rprice, na.rm = TRUE)
  dat$wheat_rev <- dat$wheat_yield*mean(mdat$wheat_rprice, na.rm = TRUE)
  dat$soybean_rev <- dat$soybean_yield*mean(mdat$soybean_rprice, na.rm = TRUE)
  
  dat$corn_a <- remove_outliers(dat$corn_a)
  dat$cotton_a <- remove_outliers(dat$cotton_a)
  dat$hay_a <- remove_outliers(dat$hay_a)
  dat$wheat_a <- remove_outliers(dat$wheat_a)
  dat$soybean_a <- remove_outliers(dat$soybean_a)
  
  # Cut into BINS
  dat$ftavg <- cut(dat$tavg, i, labels = 1:i)
  dat$fdday10_30 <- cut(dat$dday10_30, i, labels = 1:i)
  dat$fdday30 <- cut(dat$dday30, i, labels = 1:i)
  
  tavg_dat <- dat %>% 
    group_by(ftavg) %>% 
    summarise(corn_rev = mean(corn_rev, na.rm = TRUE),
              cotton_rev = mean(cotton_rev, na.rm = TRUE),
              hay_rev = mean(hay_rev, na.rm = TRUE),
              wheat_rev = mean(wheat_rev, na.rm = TRUE),
              soybean_rev = mean(soybean_rev, na.rm = TRUE),
              tavg = mean(tavg, na.rm = TRUE),
              p_corn_a = mean(p_corn_a, na.rm = TRUE),
              p_cotton_a = mean(p_cotton_a, na.rm = TRUE),
              p_hay_a = mean(p_hay_a, na.rm = TRUE),
              p_wheat_a = mean(p_wheat_a, na.rm = TRUE),
              p_soybean_a = mean(p_soybean_a, na.rm = TRUE),
              corn_acres = sum(corn_a, na.rm = TRUE),
              cotton_acres = sum(cotton_a, na.rm = TRUE),
              hay_acres = sum(hay_a, na.rm = TRUE),
              wheat_acres = sum(wheat_a, na.rm = TRUE),
              soybean_acres = sum(soybean_a, na.rm = TRUE)) %>% 
    mutate(ftavg = NULL) %>% 
    arrange(tavg) %>% 
    ungroup()
  
  
  dday10_30_dat <- dat %>% 
      group_by(fdday10_30) %>% 
      summarise(corn_rev = mean(corn_rev, na.rm = TRUE),
              cotton_rev = mean(cotton_rev, na.rm = TRUE),
              hay_rev = mean(hay_rev, na.rm = TRUE),
              wheat_rev = mean(wheat_rev, na.rm = TRUE),
              soybean_rev = mean(soybean_rev, na.rm = TRUE),
              dday10_30 = mean(dday10_30, na.rm = TRUE),
              p_corn_a = mean(p_corn_a, na.rm = TRUE),
              p_cotton_a = mean(p_cotton_a, na.rm = TRUE),
              p_hay_a = mean(p_hay_a, na.rm = TRUE),
              p_wheat_a = mean(p_wheat_a, na.rm = TRUE),
              p_soybean_a = mean(p_soybean_a, na.rm = TRUE),
              corn_acres = sum(corn_a, na.rm = TRUE),
              cotton_acres = sum(cotton_a, na.rm = TRUE),
              hay_acres = sum(hay_a, na.rm = TRUE),
              wheat_acres = sum(wheat_a, na.rm = TRUE),
              soybean_acres = sum(soybean_a, na.rm = TRUE)) %>% 
    mutate(fdday10_30 = NULL) %>% 
    arrange(dday10_30) %>% 
    ungroup()
  
  dday30_dat <- dat %>% 
      group_by(fdday30) %>% 
      summarise(corn_rev = mean(corn_rev, na.rm = TRUE),
              cotton_rev = mean(cotton_rev, na.rm = TRUE),
              hay_rev = mean(hay_rev, na.rm = TRUE),
              wheat_rev = mean(wheat_rev, na.rm = TRUE),
              soybean_rev = mean(soybean_rev, na.rm = TRUE),
              dday30 = mean(dday30, na.rm = TRUE),
              p_corn_a = mean(p_corn_a, na.rm = TRUE),
              p_cotton_a = mean(p_cotton_a, na.rm = TRUE),
              p_hay_a = mean(p_hay_a, na.rm = TRUE),
              p_wheat_a = mean(p_wheat_a, na.rm = TRUE),
              p_soybean_a = mean(p_soybean_a, na.rm = TRUE),
              corn_acres = sum(corn_a, na.rm = TRUE),
              cotton_acres = sum(cotton_a, na.rm = TRUE),
              hay_acres = sum(hay_a, na.rm = TRUE),
              wheat_acres = sum(wheat_a, na.rm = TRUE),
              soybean_acres = sum(soybean_a, na.rm = TRUE)) %>% 
    mutate(fdday30 = NULL) %>% 
    arrange(dday30) %>% 
    ungroup()
  
  # Save files
  saveRDS(tavg_dat, paste0("data/tavg_dat_bins_", i, ".rds"))
  saveRDS(dday10_30_dat, paste0("data/dday10_30_dat_bins_", i, ".rds"))
  saveRDS(dday30_dat, paste0("data/dday30_dat_bins_", i, ".rds"))
          
}
