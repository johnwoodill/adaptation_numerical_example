library(ggplot2)
library(ggthemes)
library(tidyverse)
library(cowplot)

set.seed(123)

  wheat_fn <- function(x) return(-100*(x)^2 + 20000)
  corn_fn <- function(x) return(-250*(x - 15)^2 + 19000)
  cotton_fn <- function(x) return(-250*(x - 25.433)^2 + 17000)
  
  # First Derivate w.r.t. t
  wheat_dt <- function(x) return(-200*x)
  corn_dt <- function(x) return(-500*(x - 15))
  cotton_dt <- function(x) return(-500*(x - 25.433))

# Mendelsohn Plot
  x <- seq(0, 40, 0.1)
  wheat <- data.frame(y = -100*(x)^2 + 20000, temp = x, crop = "Wheat")
  # plot(wheat$temp, wheat$y)
  
  # x <- seq(6, 27, 0.1)
  corn <- data.frame(y = -250*(x - 15)^2 + 19000, temp = x, crop = "Corn")
  # plot(corn$temp, corn$y)
  
  # x <- seq(15, 35, 0.1)
  cotton <- data.frame(y = -250*(x - 25.433)^2 + 17000, temp = x, crop = "Cotton")
  
  dat <- rbind(wheat, corn, cotton)
  dat <- filter(dat, y > 0)
  
  ggplot(dat, aes(temp, y, color = crop)) + geom_line() + ylim(-1, 20000) +
    theme_tufte(base_size = 12) +
    ylab("Value of Activity") +
    xlab("Temperature (C)") +
    annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
    annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
    geom_point(aes(9.400855, wheat_fn(9.400855))) +
    geom_point(aes(20.5999, corn_fn(20.5999))) +
    geom_point(aes(14.142, wheat_fn(14.142))) +
    geom_point(aes(23.717798, corn_fn(23.7177978))) +
    geom_point(aes(6.28220211, corn_fn(6.28220211))) +
    geom_point(aes(17.19, cotton_fn(17.19))) +
    geom_point(aes(33.6792, cotton_fn(33.6792)))
  
p1 <- ggplot(dat, aes(temp, y, color = crop)) + geom_line() + ylim(-1, 23000) +
    theme_tufte(base_size = 12) +
    ylab("Value of Activity") +
    xlab(NULL) +
    annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
    annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
    theme(legend.position = "none",
          plot.margin = unit(c(0, 0, 4, 0), "cm")) +
    annotate("text", x = 3, y = 23000, label = "Wheat", color = "red") +
    annotate("text", x = 15, y = 23000, label = "Corn", color = "green") +
    annotate("text", x = 26, y = 23000, label = "Cotton", color = "blue")
p1    

set.seed(1234)
densdat <- rnorm(3510, 15, 7)
densplot <- ggplot(NULL) + geom_density(aes(densdat), fill = "grey95") +
  # geom_density(fill = "grey95") + 
  # xlim(13, 23) + 
  # ylim(0, 0.15) +
  theme_tufte(base_size = 12) +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  xlab("Climate (t)") +
  ylab("Density") +
      theme(legend.position = "none",
        # axis.title.x=element_blank(),
        # axis.title.y=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.box.background = element_rect(colour = "grey"),
        legend.title = element_blank(),
        legend.key = element_rect(fill = NA, color = NA),
        legend.text=element_text(size=8)) + 
  # geom_segment(aes(x = 15.2, xend = 15.2, y = 0, yend = .15), linetype = 'dashed', size = .25) +
  # annotate("text", x = 22.5, y = .10, label=TeX("$\\mathbf{\\phi}", output = "character"), parse=TRUE, size = 4) +
  # annotate("text", x = 22.7, y = .10, label=c("(t)"), parse=TRUE, size = 3) +
  scale_x_continuous(limits = c(0, 35)) 

densplot

ggdraw() + draw_plot(p1, 0.005, width = 0.98) +
  draw_plot(densplot, 0.055, height = .30, width = .98) 
ggsave("figures/numerical_example_baseplot.pdf", width = 6, height = 5)
 
CalcAdaptation <- function(density, delta){
  # Functions
  wheat_fn <- function(x) return(-100*(x)^2 + 20000)
  corn_fn <- function(x) return(-250*(x - 15)^2 + 19000)
  cotton_fn <- function(x) return(-250*(x - 25.433)^2 + 17000)
  
  # First Derivate w.r.t. t
  wheat_dt <- function(x) return(-200*x)
  corn_dt <- function(x) return(-500*(x - 15))
  cotton_dt <- function(x) return(-500*(x - 25.433))
  
  # Delta t
  delta <- delta

  # Baseline integration
  integrate_base1 <- function(t) wheat_fn(t) * phi(t)
  integrate_base2 <- function(t) corn_fn(t) * phi(t) 
  integrate_base3 <- function(t) cotton_fn(t) * phi(t)
  
  # Marginal Integration functions
  integrate_fn1 <- function(t) wheat_dt(t) * phi(t) * delta
  integrate_fn2 <- function(t) corn_dt(t) * phi(t) * delta
  integrate_fn3 <- function(t) cotton_dt(t) * phi(t) * delta
  
  # Real Integration functions
  integrate_fn4 <- function(t) (wheat_fn(t + delta) - wheat_fn(t)) * phi(t)
  integrate_fn5 <- function(t) (corn_fn(t + delta) - corn_fn(t)) * phi(t)
  integrate_fn6 <- function(t) (cotton_fn(t + delta) - cotton_fn(t)) * phi(t)
  
  
  # Envelope is :
  # wheat (temp from 0 to 9.400855)
  # corn (temp from 9.400855 to 20.5999)
  # cotton (temp from 20.5999 to 33)
  
  # Baseline integration
  baseline <- integrate(integrate_base1, 0, 9.400855)[[1]] + integrate(integrate_base2, 9.400855, 20.5999)[[1]] + integrate(integrate_base3, 20.5999, 33)[[1]]
  
  # Aggregate Marginal Effect
  marginal <- integrate(integrate_fn1, 0, 9.400855)[[1]] + integrate(integrate_fn2, 9.400855, 20.5999)[[1]] + integrate(integrate_fn3, 20.5999, 33)[[1]]

  # Aggregate Real Effect
  real <- integrate(integrate_fn4, 0, 9.400855)[[1]] + integrate(integrate_fn5, 9.400855, 20.5999)[[1]] + integrate(integrate_fn6, 20.5999, 33)[[1]]

  adaptation <- real - marginal

  # Build return data  
  retdat <- data.frame(delta = delta, 
                       baseline = baseline,
                      marginal = marginal,
                      real = real,
                      adaptation = adaptation,
                      marginal_ch = 100 * ((baseline + marginal) - baseline)/baseline,
                      real_ch = 100 * ((baseline + real) - baseline)/baseline,
                      adaptation_ch = 100 * ((baseline + adaptation) - baseline)/baseline)
  return(retdat)
}

phi <- approxfun(density(rnorm(35, 15, 7)))
#   delta baseline  marginal      real adaptation marginal_ch
# 1     0 15674.73    0.0000     0.000     0.0000   0.0000000
# 2     1 15674.73 -136.4371  -352.282  -215.8449  -0.8704269
# 3     2 15674.73 -272.8742 -1136.260  -863.3860  -1.7408539
# 4     3 15674.73 -409.3112 -2351.926 -1942.6143  -2.6112808
# 5     4 15674.73 -545.7483 -3999.287 -3453.5382  -3.4817078
# 6     5 15674.73 -682.1854 -6078.341 -5396.1552  -4.3521347
#
#      real_ch adaptation_ch
# 1   0.000000      0.000000
# 2  -2.247451     -1.377024
# 3  -7.248993     -5.508139
# 4 -15.004567    -12.393286
# 5 -25.514228    -22.032520
# 6 -38.777958    -34.425823

phi <- approxfun(density(rnorm(35, 17, 10)))
#   delta baseline   marginal       real adaptation marginal_ch
# 1     0 13783.37     0.0000     0.0000     0.0000    0.000000
# 2     1 13783.37  -297.3331  -492.1124  -194.7793   -2.157188
# 3     2 13783.37  -594.6663 -1373.7834  -779.1171   -4.314376
# 4     3 13783.37  -891.9994 -2645.0128 -1753.0134   -6.471564
# 5     4 13783.37 -1189.3326 -4305.8009 -3116.4683   -8.628752
# 6     5 13783.37 -1486.6657 -6356.1474 -4869.4817  -10.785939
#
#      real_ch adaptation_ch
# 1   0.000000      0.000000
# 2  -3.570335     -1.413147
# 3  -9.966964     -5.652588
# 4 -19.189888    -12.718324
# 5 -31.239105    -22.610354
# 6 -46.114617    -35.328678

phi <- approxfun(density(rnorm(35, 20, 13)))
rbind(CalcAdaptation(phi, 0), 
             CalcAdaptation(phi, 1), 
             CalcAdaptation(phi, 2), 
             CalcAdaptation(phi, 3),
             CalcAdaptation(phi, 4),
             CalcAdaptation(phi, 5))
#   delta baseline   marginal       real adaptation marginal_ch
# 1     0 11373.67     0.0000     0.0000     0.0000    0.000000
# 2     1 11373.67  -381.2313  -544.7541  -163.5229   -3.351877
# 3     2 11373.67  -762.4626 -1416.5540  -654.0914   -6.703753
# 4     3 11373.67 -1143.6938 -2615.3996 -1471.7058  -10.055630
# 5     4 11373.67 -1524.9251 -4141.2909 -2616.3658  -13.407507
# 6     5 11373.67 -1906.1564 -5994.2279 -4088.0715  -16.759383
#      real_ch adaptation_ch
# 1   0.000000      0.000000
# 2  -4.789609     -1.437732
# 3 -12.454682     -5.750929
# 4 -22.995219    -12.939589
# 5 -36.411221    -23.003714
# 6 -52.702687    -35.943304
 
# phi <- approxfun(density(runif(35, 3, 30)))
#  delta baseline   marginal       real adaptation marginal_ch
# 1     0 14763.74     0.0000     0.0000     0.0000    0.000000
# 2     1 14763.74  -414.1246  -630.2348  -216.1102   -2.805011
# 3     2 14763.74  -828.2493 -1692.6911  -864.4418   -5.610023
# 4     3 14763.74 -1242.3739 -3187.3689 -1944.9949   -8.415034
# 5     4 14763.74 -1656.4986 -5114.2681 -3457.7695  -11.220046
# 6     5 14763.74 -2070.6232 -7473.3888 -5402.7656  -14.025057
#
#      real_ch adaptation_ch
# 1   0.000000      0.000000
# 2  -4.268801     -1.463790
# 3 -11.465190     -5.855168
# 4 -21.589167    -13.174133
# 5 -34.640731    -23.420686
# 6 -50.619883    -36.594826

# phi <- approxfun(density(runif(35, 5, 32)))
#   delta baseline   marginal       real adaptation marginal_ch
# 1     0 15536.71     0.0000     0.0000     0.0000    0.000000
# 2     1 15536.71  -230.1086  -450.8236  -220.7151   -1.481064
# 3     2 15536.71  -460.2171 -1343.0774  -882.8603   -2.962127
# 4     3 15536.71  -690.3257 -2676.7881 -1986.4624   -4.443191
# 5     4 15536.71  -920.4342 -4451.9134 -3531.4791   -5.924255
# 6     5 15536.71 -1150.5428 -6668.4699 -5517.9272   -7.405318
#
#      real_ch adaptation_ch
# 1   0.000000      0.000000
# 2  -2.901667     -1.420604
# 3  -8.644542     -5.682415
# 4 -17.228797    -12.785606
# 5 -28.654158    -22.729904
# 6 -42.920735    -35.515417

u1 <- rbind(CalcAdaptation(approxfun(density(runif(35, 3, 30))), 0), 
             CalcAdaptation(approxfun(density(runif(35, 3, 30))), 1), 
             CalcAdaptation(approxfun(density(runif(35, 3, 30))), 2), 
             CalcAdaptation(approxfun(density(runif(35, 3, 30))), 3),
             CalcAdaptation(approxfun(density(runif(35, 3, 30))), 4),
             CalcAdaptation(approxfun(density(runif(35, 3, 30))), 5))
u1$distr <- "Uniform (3, 30)"

u2 <- rbind(CalcAdaptation(approxfun(density(runif(35, 5, 32))), 0), 
             CalcAdaptation(approxfun(density(runif(35, 5, 32))), 1), 
             CalcAdaptation(approxfun(density(runif(35, 5, 32))), 2), 
             CalcAdaptation(approxfun(density(runif(35, 5, 32))), 3),
             CalcAdaptation(approxfun(density(runif(35, 5, 32))), 4),
             CalcAdaptation(approxfun(density(runif(35, 5, 32))), 5))
u2$distr <- "Uniform (5, 32)"

n1 <- rbind(CalcAdaptation(approxfun(density(rnorm(35, 15, 7))), 0), 
             CalcAdaptation(approxfun(density(rnorm(35, 15, 7))), 1), 
             CalcAdaptation(approxfun(density(rnorm(35, 15, 7))), 2), 
             CalcAdaptation(approxfun(density(rnorm(35, 15, 7))), 3),
             CalcAdaptation(approxfun(density(rnorm(35, 15, 7))), 4),
             CalcAdaptation(approxfun(density(rnorm(35, 15, 7))), 5))
n1$distr <- "Normal (15, 7)"

n2 <- rbind(CalcAdaptation(approxfun(density(rnorm(35, 17, 10))), 0), 
             CalcAdaptation(approxfun(density(rnorm(35, 17, 10))), 1), 
             CalcAdaptation(approxfun(density(rnorm(35, 17, 10))), 2), 
             CalcAdaptation(approxfun(density(rnorm(35, 17, 10))), 3),
             CalcAdaptation(approxfun(density(rnorm(35, 17, 10))), 4),
             CalcAdaptation(approxfun(density(rnorm(35, 17, 10))), 5))
n2$distr <- "Normal (17, 10)"

dat <- rbind(u1, u2, n1, n2)

# Plot percentage change
# pdat <- select(dat, delta, marginal_ch, real_ch)
# pdat <- gather(pdat, key = change, value = value, -delta)
# pdat
# 
# ggplot(pdat, aes(delta, value, color = change)) + geom_line() + geom_point(size = 0.5) +
#     theme_tufte(base_size = 12) +
#     ylab("% Change in \n Value of Activity") +
#     xlab("Temperature (C)") +
#     annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
#     annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
#     annotate("text", x = 4.2, y = -2, label = "Approximate Effect", color = "red", size = 3.5) +
#     annotate("text", x = 4.2, y = -20, label = "Real Effect", color = "#00BFC4", size = 3.5) +
#   geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
#   theme(legend.position = "none") +
#   scale_x_continuous(breaks = 0:5, labels = c("+0C", "+1C", "+2C", "+3C", "+4C", "+5C"))
# 
# ggsave("figures/adaptation_example_plot_1.pdf", width = 6, height = 4)

pdat <- select(dat, delta, adaptation_ch, real_ch, distr)
pdat <- gather(pdat, key = change, value = value, -delta, -distr)
pdat$distr <- factor(pdat$distr, levels = c("Uniform (3, 30)", "Uniform (5, 32)", "Normal (15, 7)", "Normal (17, 10)"))

ggplot(pdat, aes(delta, value, color = change)) + geom_line() + geom_point(size = 0.5) +
    theme_tufte(base_size = 12) +
    ylab("% Change in \n Value of Activity") +
    xlab("Temperature (C)") +
    annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
    annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
    annotate("text", x = 4.4, y = -15, label = "Adaptation + NL", color = "red", size = 2.5) +
    annotate("text", x = 3, y = -28, label = "Total Effect", color = "#00BFC4", size = 2.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  theme(legend.position = "none") +
  scale_x_continuous(breaks = 0:5, labels = c("+0C", "+1C", "+2C", "+3C", "+4C", "+5C")) +
  facet_wrap(~distr)
ggsave("figures/adapatation_total_effect_plot.pdf", width = 6, height = 4)
