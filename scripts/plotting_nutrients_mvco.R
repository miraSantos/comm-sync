save_r_path = "/home/mira/MIT-WHOI/Week.2023.08.28-09.03/comm-sync/data/ifcb/r_objects/unfilled/"
load(paste0(save_r_path,"2023_Jul_28_dfcarbon_group.RData"))
load(paste0(save_r_path,"2023_Jul_28_dfcount_index.RData"))

list.of.packages <- c("RColorBrewer", "lubridate","ggplot2",
                      "tibbletime","dplyr","sets",
                      "reshape2","ggformula","tidyr","ggbump","purrr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

#######################################

dfnut_mvco$doy_numeric <- factor(dfnut_mvco$doy_numeric)
dfnut_mvco$week <- factor(week(dfnut_mvco$date))
dfnut_mvco$month <- factor(month(dfnut_mvco$date))

metseasons <- c(
  "01" = "Winter", "02" = "Winter",
  "03" = "Spring", "04" = "Spring", "05" = "Spring",
  "06" = "Summer", "07" = "Summer", "08" = "Summer",
  "09" = "Fall", "10" = "Fall", "11" = "Fall",
  "12" = "Winter")

seasons = metseasons[format(dfnut_mvco$date, "%m")]
dfnut_mvco$season = seasons

regime_1_index = (which(dfnut_mvco$year < regime_1_end))
regime_2_index = (which((dfnut_mvco$year >= regime_1_end)&(dfnut_mvco$year < regime_2_end)))
regime_3_index = (which(dfnut_mvco$year >= regime_2_end))

dfnut_mvco$regime[regime_1_index] = paste0("2006 - ",as.character(regime_1_end-1))
dfnut_mvco$regime[regime_2_index] = paste0(as.character(regime_1_end)," - ",as.character(regime_2_end-1))
dfnut_mvco$regime[regime_3_index] = paste0(as.character(regime_2_end)," - 2022")




dfnut_mvco %>% filter(season=="Summer",Latitude >= 41)  %>% ggplot() +
  geom_point(aes(x=nitrite_nitrate_mean,y=Depth)) +
  facet_grid(cols=vars(regime))+
  scale_y_reverse()+
  xlab(expression("Nitrite and Nitrate (umol kg"^-1*")"))+ylab("Depth (m)")

dfnut_mvco %>% filter(season=="Summer")  %>% ggplot() +
  geom_sf(data=nes_shp)+
  geom_point(aes(x=Longitude,y=Latitude,color=nitrite_nitrate_mean))+
  xlim(-71,-70.3)+
  ylim(41,41.8)

ggplot(data=dfnut_mvco[dfnut_mvco$season=="Summer",],aes(x = month,y = nitrite_nitrate_mean)) +
  geom_boxplot(alpha=0.5) + 
  facet_grid(cols=vars(regime))+
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  annotation_logticks(sides="l")+
  scale_x_discrete(breaks=seq(1,12,1))+
  xlab("Month of Year")+
  ylab("Nitrate Concentration (umol/L) ")+
  theme(text = element_text(size = 15))


ggsave(filename="/dos/MIT-WHOI/community_synchrony/figures/biwavelet_coherence/group_level/nitrate-regime.png")

ggplot(data=dfnut_mvco,aes(x = month,y = phosphate_mean)) +
  geom_boxplot(alpha=0.5,color="purple") + 
  facet_grid(cols=vars(regime))+
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  annotation_logticks(sides="l")+
  scale_x_discrete(breaks=seq(1,12,1))+
  xlab("Month of Year")+
  ylab("Phosphate Concentration  (umol/L)")+
  theme(text = element_text(size = 15))

ggsave(filename="/dos/MIT-WHOI/community_synchrony/figures/biwavelet_coherence/group_level/phosphate-regime.png")


ggplot(data=dfnut_mvco[dfnut_mvco$season=="Summer",],aes(x = month,y = silicate_mean)) +
  geom_boxplot(alpha=0.5,color="brown") + 
  facet_grid(cols=vars(year))+
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  annotation_logticks(sides="l")+
  scale_x_discrete(breaks=seq(1,12,1))+
  xlab("Month of Year")+
  ylab("Silicate Concentration  (umol/L)")+

ggsave()


ggplot(data=dfnut_mvco[dfnut_mvco$season=="Summer",],aes(x = month,y = silicate_mean)) +
  geom_boxplot(alpha=0.5,color="brown") + 
  facet_grid(cols=vars(year))+
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  annotation_logticks(sides="l")+
  scale_x_discrete(breaks=seq(1,12,1))+
  xlab("Month of Year")+
  ylab("Silicate Concentration  (umol/L)")+
  ggtitle("Summer Silicate Concentrations at MVCO")+
  theme(text = element_text(size = 15))


ggsave(filename="/dos/MIT-WHOI/community_synchrony/figures/biwavelet_coherence/group_level/silicate-regime.png")


ggplot(data=dfnut_mvco,aes(x = month,y = ammonia_mean)) +
  geom_boxplot(alpha=0.5) + 
  facet_grid(cols=vars(regime))+
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  annotation_logticks(sides="l")+
  scale_x_discrete(breaks=seq(1,12,1))+  facet_grid(cols=vars(regime))+
  xlab("Month of Year")+
  ylab("Ammonia Concentration")
ggsave(filename="/dos/MIT-WHOI/community_synchrony/figures/biwavelet_coherence/group_level/ammonia-regime.png")


##############################################
ggplot(data=dfnut_mvco,aes(x = month,y = nitrite_nitrate_mean/phosphate_mean)) +
  geom_boxplot(alpha=0.5) + 
  facet_grid(cols=vars(regime))+
  xlab("Month of Year")+
  ylab("N:P Ratio")+
  ylim(0,20)+
  theme(text = element_text(size = 15))

ggsave(filename="/dos/MIT-WHOI/community_synchrony/figures/biwavelet_coherence/group_level/np_ratio-regime.png")
