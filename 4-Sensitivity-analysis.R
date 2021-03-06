library(tidyverse)

# Load data
# tavg_dat_bins30 <- readRDS("data/tavg_dat_bins_30.rds")
# dday10_30_dat_bins30 <- readRDS("data/dday10_30_dat_bins_30.rds")
# dday30_dat_bins30 <- readRDS("data/dday30_dat_bins_30.rds")
# 
# bins_30 <- data.frame(temp = rep(c("Average Temperature (C)", "Degree Day (10-30C)", "Degree Day (30C)"), 3, each = 6),
#                       model = rep(c("Total Effect", "Adaptation w/o Crop-switching", "Total Effect w/ Crop-switching"), each = 18))
# bins_30

# Results from 2-empirical_adaptation_example.R
bins_30 <- structure(list(temp = structure(c(1L, 1L, 1L, 1L, 1L, 1L, 2L, 
2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L, 3L, 1L, 1L, 1L, 1L, 1L, 
1L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L, 3L, 1L, 1L, 1L, 
1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L, 3L), .Label = c("Average Temperature (C)", 
"Degree Day (10-30C)", "Degree Day (30C)"), class = "factor"), 
    model = structure(c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
    1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 
    2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 4L, 4L, 4L, 4L, 
    4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L), .Label = c("Total Effect", 
    "Adaptation w/o Crop-switching", "Adaptation w/ Crop-switching", 
    "Total Effect w/ Crop-switching"), class = "factor"), rev = c(7007457556.49329, 
    6578854175.82135, 6028013487.72085, 5497831232.8457, 5026671644.85327, 
    4940107096.7591, 6963390662.56194, 6592504554.21771, 6101010430.65698, 
    5564890439.51572, 5057470891.44893, 4742174687.70835, 6753042889.0708, 
    6151256186.69838, 5498159568.55762, 4805953983.07963, 4622365230.78307, 
    4465840776.66346, 7007457556.49329, 6782194964.39911, 6556932372.30494, 
    6331669780.21076, 6106407188.11658, 5881144596.0224, 6963390662.56194, 
    6744645079.40814, 6525899496.25434, 6307153913.10054, 6088408329.94674, 
    5869662746.79294, 6753042889.0708, 7705876469.106, 8658710049.1412, 
    9611543629.17639, 10564377209.2116, 11517210789.2468, 7007457556.49329, 
    6796641381.64633, 6538698835.94056, 6301319238.4118, 6124133803.6711, 
    6059206011.59905, 6963390662.56194, 6789898158.18674, 6566312045.52246, 
    6322155105.49865, 6104909559.91282, 5984306519.88265, 6753042889.0708, 
    6495319340.93212, 6104314049.77127, 5753919338.01622, 5640960763.67905, 
    5532269989.11051), change = c(0, -6.11638924983262, -13.9771673374584, 
    -21.5431390269176, -28.2668271005733, -29.5021474346074, 
    0, -5.32622864803886, -12.3844872949822, -20.0836099942681, 
    -27.3705707962075, -31.8984828295757, 0, -8.91134133542026, 
    -18.5824870525271, -28.8327638069997, -31.5513716303512, 
    -33.8692075554411, 0, -3.21461229380467, -6.42922458760932, 
    -9.64383688141398, -12.8584491752186, -16.0730614690233, 
    0, -3.1413659487736, -6.28273189754719, -9.42409784632079, 
    -12.5654637950944, -15.706829743868, 0, 14.1096924110651, 
    28.2193848221303, 42.3290772331954, 56.4387696442605, 70.5484620553257, 
    0, -3.00845453786036, -6.68942646849657, -10.0769546213971, 
    -12.6054813133142, -13.5320340829688, 0, -2.49149462930419, 
    -5.70237455115571, -9.20866842227946, -12.3284926015235, 
    -14.0604511526727, 0, -3.8164062093517, -9.60646703946515, 
    -14.7951607514825, -16.4678670587382, -18.0773751923893), 
    c = c(0L, 1L, 2L, 3L, 4L, 5L, 0L, 1L, 2L, 3L, 4L, 5L, 0L, 
    1L, 2L, 3L, 4L, 5L, 0L, 1L, 2L, 3L, 4L, 5L, 0L, 1L, 2L, 3L, 
    4L, 5L, 0L, 1L, 2L, 3L, 4L, 5L, 0L, 1L, 2L, 3L, 4L, 5L, 0L, 
    1L, 2L, 3L, 4L, 5L, 0L, 1L, 2L, 3L, 4L, 5L)), class = "data.frame", row.names = c(NA, 
-54L))

bins_20 <- structure(list(temp = structure(c(1L, 1L, 1L, 1L, 1L, 1L, 2L, 
2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L, 3L, 1L, 1L, 1L, 1L, 1L, 
1L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L, 3L, 1L, 1L, 1L, 
1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L, 3L), .Label = c("Average Temperature (C)", 
"Degree Day (10-30C)", "Degree Day (30C)"), class = "factor"), 
    model = structure(c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
    1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 
    2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 4L, 4L, 4L, 4L, 
    4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L), .Label = c("Total Effect", 
    "Adaptation w/o Crop-switching", "Adaptation w/ Crop-switching", 
    "Total Effect w/ Crop-switching"), class = "factor"), rev = c(6983298036.56817, 
    6562771618.06294, 6024168721.0883, 5494503496.94123, 5132230207.51137, 
    4942029392.94622, 6953453408.28424, 6584048934.99519, 6109274141.26929, 
    5575145202.87225, 5069220293.48878, 4841197373.11763, 6681753954.77428, 
    6158545394.05662, 5481923547.66744, 4809737299.12165, 4617507143.07471, 
    4516178044.1436, 6983298036.56817, 6729617948.06658, 6475937859.56499, 
    6222257771.0634, 5968577682.56181, 5714897594.06022, 6953453408.28424, 
    6740670893.68573, 6527888379.08723, 6315105864.48873, 6102323349.89023, 
    5889540835.29173, 6681753954.77428, 7296352346.53543, 7910950738.29658, 
    8525549130.05773, 9140147521.81887, 9754745913.58002, 6983298036.56817, 
    6779321775.92925, 6527038975.31912, 6291055519.58153, 6124942319.84704, 
    6050363915.40737, 6953453408.28424, 6780050050.00143, 6567265393.09332, 
    6325585959.45672, 6108726211.74547, 6002304910.37441, 6681753954.77428, 
    6440233786.67809, 6043925251.18484, 5706382062.62382, 5578647023.00709, 
    5474665009.09732), change = c(0, -6.02188845876463, -13.7346180910133, 
    -21.3193613079499, -26.5070718643778, -29.2307249802717, 
    0, -5.31253251584232, -12.1404317746517, -19.8219233592605, 
    -27.097803122555, -30.3770790014944, 0, -7.83040746874274, 
    -17.9568181532565, -28.0168451026999, -30.8937866564904, 
    -32.4102911494268, 0, -3.63266879307154, -7.26533758614308, 
    -10.8980063792146, -14.5306751722862, -18.1633439653577, 
    0, -3.06009837277396, -6.1201967455479, -9.18029511832186, 
    -12.2403934910958, -15.3004918638698, 0, 9.19815958386198, 
    18.396319167724, 27.594478751586, 36.7926383354479, 45.9907979193099, 
    0, -2.92091587056427, -6.53357566668137, -9.9128307765429, 
    -12.2915521036956, -13.3595060138559, 0, -2.49377321024713, 
    -5.55390239231076, -9.02957727565251, -12.1483117371919, 
    -13.6787929976872, 0, -3.61462229424985, -9.54582745648247, 
    -14.5975427822141, -16.5092420228823, -18.0654503869371), 
    c = c(0L, 1L, 2L, 3L, 4L, 5L, 0L, 1L, 2L, 3L, 4L, 5L, 0L, 
    1L, 2L, 3L, 4L, 5L, 0L, 1L, 2L, 3L, 4L, 5L, 0L, 1L, 2L, 3L, 
    4L, 5L, 0L, 1L, 2L, 3L, 4L, 5L, 0L, 1L, 2L, 3L, 4L, 5L, 0L, 
    1L, 2L, 3L, 4L, 5L, 0L, 1L, 2L, 3L, 4L, 5L)), class = "data.frame", row.names = c(NA, 
-54L))

bins_10 <- structure(list(temp = structure(c(1L, 1L, 1L, 1L, 1L, 1L, 2L, 
2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L, 3L, 1L, 1L, 1L, 1L, 1L, 
1L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L, 3L, 1L, 1L, 1L, 
1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L, 3L), .Label = c("Average Temperature (C)", 
"Degree Day (10-30C)", "Degree Day (30C)"), class = "factor"), 
    model = structure(c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
    1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 
    2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 4L, 4L, 4L, 4L, 
    4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L), .Label = c("Total Effect", 
    "Adaptation w/o Crop-switching", "Adaptation w/ Crop-switching", 
    "Total Effect w/ Crop-switching"), class = "factor"), rev = c(6928835727.98709, 
    6514725713.7211, 6004140770.79961, 5526393039.58884, 5328335169.11694, 
    4960767372.73351, 6926810838.77867, 6543828698.06755, 6096871531.57668, 
    5564888367.59804, 5060331720.26645, 4858401784.38087, 6512493528.96839, 
    6115744897.73836, 5487171135.95016, 4838600400.16148, 4578634680.21964, 
    4404850124.65848, 6928835727.98709, 6697803233.2209, 6466770738.45472, 
    6235738243.68853, 6004705748.92234, 5773673254.15616, 6926810838.77867, 
    6756827069.91885, 6586843301.05904, 6416859532.19922, 6246875763.33941, 
    6076891994.47959, 6512493528.96839, 6242652756.68147, 5972811984.39456, 
    5702971212.10764, 5433130439.82072, 5163289667.5338, 6928835727.98709, 
    6730988467.39545, 6495127783.23616, 6273038776.75202, 6162838082.42385, 
    6032461618.70184, 6926810838.77867, 6739628850.27949, 6538394374.83733, 
    6298801832.81311, 6094110144.83828, 5998362019.09902, 6512493528.96839, 
    6372784728.89267, 6015825291.84159, 5686176043.1179, 5527717780.8794, 
    5416928736.57095), change = c(0, -5.97661758083416, -13.3456036986479, 
    -20.2406687567072, -23.0991269197706, -28.4040267732733, 
    0, -5.52898223475447, -11.981550045451, -19.6616091139103, 
    -26.9457209378814, -29.8609143881646, 0, -6.09211555397708, 
    -15.7439295479905, -25.7027991100678, -29.69459916001, -32.3630786723219, 
    0, -3.33436242156811, -6.66872484313622, -10.0030872647043, 
    -13.3374496862725, -16.6718121078406, 0, -2.45399755841733, 
    -4.90799511683466, -7.36199267525199, -9.81599023366931, 
    -12.2699877920866, 0, -4.14343248229932, -8.28686496459863, 
    -12.430297446898, -16.5737299291973, -20.7171624114966, 0, 
    -2.85541854878289, -6.25946351995461, -9.46474959113494, 
    -11.0552144059241, -12.9368647847227, 0, -2.70228237576905, 
    -5.60743570138869, -9.06635132072255, -12.0214152417538, 
    -13.4036981994927, 0, -2.14524282372449, -7.62639125732036, 
    -12.6881889736232, -15.1213316943593, -16.8225087291716), 
    c = c(0L, 1L, 2L, 3L, 4L, 5L, 0L, 1L, 2L, 3L, 4L, 5L, 0L, 
    1L, 2L, 3L, 4L, 5L, 0L, 1L, 2L, 3L, 4L, 5L, 0L, 1L, 2L, 3L, 
    4L, 5L, 0L, 1L, 2L, 3L, 4L, 5L, 0L, 1L, 2L, 3L, 4L, 5L, 0L, 
    1L, 2L, 3L, 4L, 5L, 0L, 1L, 2L, 3L, 4L, 5L)), class = "data.frame", row.names = c(NA, 
-54L))

bins_5 <- structure(list(temp = structure(c(1L, 1L, 1L, 1L, 1L, 1L, 2L, 
2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L, 3L, 1L, 1L, 1L, 1L, 1L, 
1L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L, 3L, 1L, 1L, 1L, 
1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L, 3L), .Label = c("Average Temperature (C)", 
"Degree Day (10-30C)", "Degree Day (30C)"), class = "factor"), 
    model = structure(c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
    1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 
    2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 4L, 4L, 4L, 4L, 
    4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L), .Label = c("Total Effect", 
    "Adaptation w/o Crop-switching", "Adaptation w/ Crop-switching", 
    "Total Effect w/ Crop-switching"), class = "factor"), rev = c(6859500523.88934, 
    6377599690.08247, 5895698856.2756, 5806017149.66125, 5322429934.42392, 
    6246732654.61501, 6818619647.82425, 6393150596.30099, 5967681544.77773, 
    5735263078.23801, 5281796218.32904, 4828329358.42007, 6469676646.97295, 
    5987141088.07223, 5489721836.22937, 5024651916.35617, 4745782283.35152, 
    4457253175.47265, 6859500523.88934, 6648517456.46276, 6437534389.03618, 
    6226551321.6096, 6015568254.18302, 5804585186.75644, 6818619647.82425, 
    6707179587.0945, 6595739526.36474, 6484299465.63498, 6372859404.90522, 
    6261419344.17547, 6469676646.97295, 5985299233.39816, 5500921819.82338, 
    5016544406.24859, 4532166992.6738, 4047789579.09901, 6859500523.88934, 
    6620682056.13557, 6408511257.04744, 6330022260.76229, 6121549810.02226, 
    6526222207.5698, 6818619647.82425, 6607968822.07151, 6420931812.30755, 
    6293375977.24314, 6102275127.76966, 5930671373.33377, 6469676646.97295, 
    6214542854.94388, 5988312604.29548, 5729288656.06775, 5567223601.79047, 
    5383137812.58782), change = c(0, -7.02530500768347, -14.0506100153669, 
    -15.3580187151988, -22.407908332572, -8.93312664880283, 0, 
    -6.23981206605395, -12.4796241321079, -15.8882094256707, 
    -22.5386296475064, -29.1890498693421, 0, -7.45841848412153, 
    -15.1468900876536, -22.3353470268547, -26.645757704568, -31.1054722099887, 
    0, -3.07577886599463, -6.15155773198928, -9.22733659798391, 
    -12.3031154639786, -15.3788943299732, 0, -1.63434927427455, 
    -3.26869854854911, -4.90304782282365, -6.53739709709821, 
    -8.17174637137276, 0, -7.48688752167267, -14.9737750433453, 
    -22.460662565018, -29.9475500866907, -37.4344376083634, 0, 
    -3.48157226494915, -6.57466626427472, -7.71890404094367, 
    -10.7580823311705, -4.85863825155847, 0, -3.08934706190814, 
    -5.83238039452174, -7.70307918185048, -10.5057116697098, 
    -13.0224051252634, 0, -3.94353235796479, -7.44031068233834, 
    -11.4439721071936, -13.9489667633502, -16.794329820077), 
    c = c(0L, 1L, 2L, 3L, 4L, 5L, 0L, 1L, 2L, 3L, 4L, 5L, 0L, 
    1L, 2L, 3L, 4L, 5L, 0L, 1L, 2L, 3L, 4L, 5L, 0L, 1L, 2L, 3L, 
    4L, 5L, 0L, 1L, 2L, 3L, 4L, 5L, 0L, 1L, 2L, 3L, 4L, 5L, 0L, 
    1L, 2L, 3L, 4L, 5L, 0L, 1L, 2L, 3L, 4L, 5L)), class = "data.frame", row.names = c(NA, 
-54L))

bins_5$bins <- "5-bins"
bins_10$bins <- "10-bins"
bins_20$bins <- "20-bins"
bins_30$bins <- "30-bins"

bindat <- rbind(bins_5, bins_10, bins_20, bins_30)

bindat$bins <- factor(bindat$bins, levels = c("5-bins", "10-bins", "20-bins", "30-bins"))
atext2 <- data.frame(temp = "Average Temperature (C)",
                    x = c(3.5, 2.7, 3.7),
                    y = c(-14, -24, -5),
                    model= c(
                             "Marginal Effect \n w/o Adaptation", 
                             "NL w/o Adaptation", 
                             "NL w/ Adaptation"))

levels(bindat$model)

bindat$model <- factor(bindat$model, labels = c("NL w/o Adaptation", "Marginal Effect w/o Adaptation", "NL w Adaptation"))
bindat$model <- factor(bindat$model, levels = c("Marginal Effect w/o Adaptation", "NL w/o Adaptation", "NL w Adaptation"))

ggplot(bindat, aes(x=c, y=change, linetype=model, shape=model)) + 
  theme_minimal(12) +
  geom_line() +
  geom_point() +
  labs(x="Change in Temperature (C)", y="Percentage Change from Baseline (+0C)", linetype=NULL, shape=NULL) +
  scale_x_continuous(breaks = 0:5, labels = c("+0C", "+1C", "+2C", "+3C", "+4C", "+5C")) +
  scale_y_continuous(labels=function(x) paste0(x,"%")) +
  facet_wrap(bins~temp, ncol = 3, scales = "free") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey", alpha = 0.5) +
  theme(plot.title = element_text(hjust = 0.5, color='black'),
        panel.border = element_rect(colour = "grey", fill=NA, size=1),
        legend.position="bottom") +
  NULL

ggsave("figures/7-robust_main_plot.png", width = 12, height = 10)





dday30_dat <- readRDS("data/dday30_dat_bins_30.rds")



# Degree Day 30 30-bin Plot
dday30_pdat <- gather(dday30_dat, key = crop_var, value = value, -dday30)
dday30_pdat$crop_var <- factor(dday30_pdat$crop_var, 
                             levels = c('corn_rev', 'cotton_rev', 'hay_rev', 'wheat_rev', 'soybean_rev',
                                        'p_corn_a', 'p_cotton_a', 'p_hay_a', 'p_wheat_a', 'p_soybean_a',
                                        'corn_acres', 'cotton_acres', 'hay_acres', 'wheat_acres', 'soybean_acres'),
                             labels = c("Corn Revenue", "Cotton Revenue", "Hay Revenue", "Wheat Revenue", "Soybean Revenue",
                                        "P(Corn Acres)", "P(Cotton Acres)", "P(Hay Acres)", "P(Wheat Acres)", "P(Soybean Acres)",
                                        "Corn Acres", "Cotton Acres", "Hay Acres", "Wheat Acres", "Soybean Acres"))

ggplot(filter(dday30_pdat, crop_var %in% c("Corn Revenue", "Cotton Revenue", "Hay Revenue", "Wheat Revenue", "Soybean Revenue",
                                        "P(Corn Acres)", "P(Cotton Acres)", "P(Hay Acres)", "P(Wheat Acres)", "P(Soybean Acres)")), aes(x = dday30, y = value, group = crop_var)) + 
  theme_minimal(12) +
  geom_line() +
  facet_wrap(~crop_var, scales = 'free', ncol = 5) +
  theme(plot.title = element_text(hjust = 0.5, color='black'),
        panel.border = element_rect(colour = "grey", fill=NA, size=1),
        legend.position="bottom") +
  ylab(NULL) +
  xlab("Degree Days (30C)") +
  scale_x_continuous(breaks = seq(0, 165, 30)) +
  NULL

ggsave("figures/8-robust_30bins_dday30.png", width = 8, height = 5)
