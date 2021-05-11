library(tidyverse)

# Load data for main analysis with 10-bins
tavg_dat <- readRDS("data/tavg_dat_bins_10.rds")
dday10_30_dat <- readRDS("data/dday10_30_dat_bins_10.rds")
dday30_dat <- readRDS("data/dday30_dat_bins_10.rds")

# Load model data
source("2-empirical_adaptation_example.R")





# Figure 1. Tavg 10-bin plot
tavg_pdat <- gather(tavg_dat, key = crop_var, value = value, -tavg)
tavg_pdat$crop_var <- factor(tavg_pdat$crop_var, 
                             levels = c('corn_rev', 'cotton_rev', 'hay_rev', 'wheat_rev', 'soybean_rev',
                                        'p_corn_a', 'p_cotton_a', 'p_hay_a', 'p_wheat_a', 'p_soybean_a',
                                        'corn_acres', 'cotton_acres', 'hay_acres', 'wheat_acres', 'soybean_acres'),
                             labels = c("Corn Revenue", "Cotton Revenue", "Hay Revenue", "Wheat Revenue", "Soybean Revenue",
                                        "P(Corn Acres)", "P(Cotton Acres)", "P(Hay Acres)", "P(Wheat Acres)", "P(Soybean Acres)",
                                        "Corn Acres", "Cotton Acres", "Hay Acres", "Wheat Acres", "Soybean Acres"))

tavg_pdat <- filter(tavg_pdat, crop_var %in% c("Corn Revenue", "Cotton Revenue", "Hay Revenue", "Wheat Revenue", "Soybean Revenue",
                                  "P(Corn Acres)", "P(Cotton Acres)", "P(Hay Acres)", "P(Wheat Acres)", "P(Soybean Acres)"))

ggplot(tavg_pdat, aes(x = tavg, y = value)) + 
  theme_minimal(12) +
  geom_line() +
  theme(legend.position = 'none', 
        plot.title = element_text(hjust = 0.5, color='black'),
        panel.border = element_rect(colour = "grey", fill=NA, size=1)) +
  labs(x="Average Temperature (C)", y=NULL) +
  scale_x_continuous(breaks = seq(10, 30, by = 2)) +
  facet_wrap(~crop_var, scales = 'free', ncol = 5) +
  NULL

ggsave("figures/1-bins_tavg.pdf", width = 7, height = 4)






# Figure 2. Degree Day 10_30 1-bin plot
dday10_30_pdat <- gather(dday10_30_dat, key = crop_var, value = value, -dday10_30)
dday10_30_pdat$crop_var <- factor(dday10_30_pdat$crop_var, 
                                  levels = c('corn_rev', 'cotton_rev', 'hay_rev', 'wheat_rev', 'soybean_rev',
                                             'p_corn_a', 'p_cotton_a', 'p_hay_a', 'p_wheat_a', 'p_soybean_a',
                                             'corn_acres', 'cotton_acres', 'hay_acres', 'wheat_acres', 'soybean_acres'),
                                  labels = c("Corn Revenue", "Cotton Revenue", "Hay Revenue", "Wheat Revenue", "Soybean Revenue",
                                             "P(Corn Acres)", "P(Cotton Acres)", "P(Hay Acres)", "P(Wheat Acres)", "P(Soybean Acres)",
                                             "Corn Acres", "Cotton Acres", "Hay Acres", "Wheat Acres", "Soybean Acres"))

dday10_30_pdat <- filter(dday10_30_pdat, crop_var %in% c("Corn Revenue", "Cotton Revenue", "Hay Revenue", "Wheat Revenue", "Soybean Revenue",
                                  "P(Corn Acres)", "P(Cotton Acres)", "P(Hay Acres)", "P(Wheat Acres)", "P(Soybean Acres)"))

ggplot(dday10_30_pdat, aes(x = dday10_30, y = value)) + 
  theme_minimal(12) +
  geom_line() +
  facet_wrap(~crop_var, scales = 'free', ncol = 5) +
  theme(legend.position = 'none', 
        plot.title = element_text(hjust = 0.5, color='black'),
        panel.border = element_rect(colour = "grey", fill=NA, size=1)) +
  labs(x="Degree Days (10-30C)", y=NULL) +
  scale_x_continuous(limits = c(1000, 3000), 
                     breaks = c(1000, 1500, 2000, 2500, 3000), 
                     labels=c("1k", "1.5k", "2k", "2.5k", "3k")) +
  NULL

ggsave("figures/2-bins_dday10_30.pdf", width = 7, height = 4)



# Degree Day 30 10-bin Plot
dday30_pdat <- gather(dday30_dat, key = crop_var, value = value, -dday30)
dday30_pdat$crop_var <- factor(dday30_pdat$crop_var, 
                               levels = c('corn_rev', 'cotton_rev', 'hay_rev', 'wheat_rev', 'soybean_rev',
                                          'p_corn_a', 'p_cotton_a', 'p_hay_a', 'p_wheat_a', 'p_soybean_a',
                                          'corn_acres', 'cotton_acres', 'hay_acres', 'wheat_acres', 'soybean_acres'),
                               labels = c("Corn Revenue", "Cotton Revenue", "Hay Revenue", "Wheat Revenue", "Soybean Revenue",
                                          "P(Corn Acres)", "P(Cotton Acres)", "P(Hay Acres)", "P(Wheat Acres)", "P(Soybean Acres)",
                                          "Corn Acres", "Cotton Acres", "Hay Acres", "Wheat Acres", "Soybean Acres"))

dday30_pdat <- filter(dday30_pdat, crop_var %in% c("Corn Revenue", "Cotton Revenue", "Hay Revenue", "Wheat Revenue", "Soybean Revenue",
                                                         "P(Corn Acres)", "P(Cotton Acres)", "P(Hay Acres)", "P(Wheat Acres)", "P(Soybean Acres)"))


ggplot(dday30_pdat, aes(x = dday30, y = value)) + 
  theme_minimal(12) +
  geom_line() +
  facet_wrap(~crop_var, scales = 'free', ncol = 5) +
  theme(legend.position = 'none', 
        plot.title = element_text(hjust = 0.5, color='black'),
        panel.border = element_rect(colour = "grey", fill=NA, size=1)) +
  labs(x="Degree Days (30C)",  y=NULL) +
  scale_x_continuous(limits = c(0, 170),
                     breaks = seq(0, 150, 30)) +
  
  NULL

ggsave("figures/3-bins_dday30.pdf", width = 7, height = 4)



# Combine all temperature plots into one for appendix
ggplot(int_pdat, aes(x=x, y=y)) + 
  theme_minimal(12) +
  geom_line() + 
  labs(y="Revenue", x=NULL) +
  facet_wrap(temp~crop) +
  theme(legend.position = 'none', 
        plot.title = element_text(hjust = 0.5, color='black'),
        panel.border = element_rect(colour = "grey", fill=NA, size=1)) +
  facet_wrap(temp~crop, scales = 'free', ncol = 5)

ggsave("figures/a1-interp_rev_plot.pdf", height = 6, width = 8)





ggplot(filter(pdat, temp == "Average Temperature (C)"), aes(x=c, y=change, linetype = model)) + 
  theme_minimal(12) +
  geom_line() +
  geom_text(data = atext2, aes(x=x, y=y, label=model, group=model), size = 2.8) +
  xlab('Change in Temperature (C)') +
  ylab("Percentage Change from Baseline (+0C)") + 
  scale_x_continuous(breaks = 0:5, labels = c("+0C", "+1C", "+2C", "+3C", "+4C", "+5C")) +
  scale_y_continuous(labels=function(x) paste0(x,"%")) +
  facet_wrap(~temp) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey", alpha = 0.5) +
  theme(legend.position = 'none', 
        strip.placement = "outside",
        plot.title = element_text(hjust = 0.5, color='black'),
        panel.border = element_rect(colour = "grey", fill=NA, size=1)) +
  NULL
  
ggsave("figures/4-tavg_main_plot.pdf", width = 6, height = 4)




atext <- data.frame(temp = "Average Temperature (C)",
                    x = c(4, 2.7, 3.7),
                    y = c(-18, -25, -7),
                    model= c(
                      #"Marginal Effect \n w/ Adaptation", 
                      "Marginal Effect \n w/o Adaptation", 
                      "NL w/o Adaptation", 
                      "NL w/ Adaptation"))
pdat <- filter(pdat, model != "Adaptation w/ Crop-switching")

ggplot(pdat, aes(x=c, y=change, linetype = model)) + 
  theme_minimal(12) +    
  geom_line() +
  geom_text(data = atext, aes(x=x, y=y, label=model, group=model), size = 2.8) +
  xlab('Change in Temperature (C)') +
  ylab("Percentage Change from Baseline (+0C)") + 
  theme(legend.position = 'none', 
        strip.placement = "outside",
        plot.title = element_text(hjust = 0.5, color='black'),
        panel.border = element_rect(colour = "grey", fill=NA, size=1)) +
  scale_x_continuous(breaks = 0:5, labels = c("+0C", "+1C", "+2C", "+3C", "+4C", "+5C")) +
  scale_y_continuous(labels=function(x) paste0(x,"%")) +
  facet_wrap(~temp) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey", alpha = 0.5) +
  theme(legend.position = 'none')

ggsave("figures/5-main_plot.pdf", width = 7, height = 5)






