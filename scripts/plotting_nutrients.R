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

dfnut$doy_numeric <- factor(dfnut$doy_numeric)
dfnut$week <- factor(week(dfnut$date))
dfnut$month <- factor(month(dfnut$date))

metseasons <- c(
  "01" = "Winter", "02" = "Winter",
  "03" = "Spring", "04" = "Spring", "05" = "Spring",
  "06" = "Summer", "07" = "Summer", "08" = "Summer",
  "09" = "Fall", "10" = "Fall", "11" = "Fall",
  "12" = "Winter")

seasons = metseasons[format(dfnut$date, "%m")]
dfnut$season = seasons

regime_1_index = (which(dfnut$year < regime_1_end))
regime_2_index = (which((dfnut$year >= regime_1_end)&(dfnut$year < regime_2_end)))
regime_3_index = (which(dfnut$year >= regime_2_end))

dfnut$regime[regime_1_index] = paste0("2006 - ",as.character(regime_1_end-1))
dfnut$regime[regime_2_index] = paste0(as.character(regime_1_end)," - ",as.character(regime_2_end-1))
dfnut$regime[regime_3_index] = paste0(as.character(regime_2_end)," - 2022")


ggplot(data=dfnut[dfnut$season=="Summer",],aes(x = month,y = nitrate_mean)) +
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

ggplot(data=dfnut,aes(x = month,y = phosphate_mean)) +
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


ggplot(data=dfnut[dfnut$season=="Summer",],aes(x = month,y = silicate_mean)) +
  geom_boxplot(alpha=0.5,color="brown") + 
  facet_grid(cols=vars(year))+
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  annotation_logticks(sides="l")+
  scale_x_discrete(breaks=seq(1,12,1))+
  xlab("Month of Year")+
  ylab("Silicate Concentration  (umol/L)")+

ggsave()


ggplot(data=dfnut[dfnut$season=="Summer",],aes(x = month,y = silicate_mean)) +
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


ggplot(data=dfnut,aes(x = month,y = ammonia_mean)) +
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
ggplot(data=dfnut,aes(x = month,y = nitrate_mean/phosphate_mean)) +
  geom_boxplot(alpha=0.5) + 
  facet_grid(cols=vars(regime))+
  xlab("Month of Year")+
  ylab("N:P Ratio")+
  ylim(0,20)+
  theme(text = element_text(size = 15))

ggsave(filename="/dos/MIT-WHOI/community_synchrony/figures/biwavelet_coherence/group_level/np_ratio-regime.png")
