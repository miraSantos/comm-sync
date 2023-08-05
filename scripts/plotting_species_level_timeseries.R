save_r_path = "C:\\Users\\Miraflor P Santos\\comm-sync\\data\\ifcb\\r_objects\\unfilled\\"
load(paste0(save_r_path,"2023_Jul_28_dfcarbon_group.RData"))
load(paste0(save_r_path,"2023_Jul_28_dfcount_index.RData"))

list.of.packages <- c("RColorBrewer", "lubridate","ggplot2",
                      "tibbletime","dplyr","sets",
                      "reshape2","ggformula","tidyr","ggbump","purrr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)


########################## SPECIES LEVEL ###########################
dfcarbon_class$regime = NaN
dfcarbon_class$year = year(dfcarbon_class$date)
dfcarbon_class$week = factor(week(dfcarbon_class$date))
dfcarbon_class$month = factor(month(dfcarbon_class$date))

regime_1_index = (which(dfcarbon_class$year < regime_1_end))
regime_2_index = (which((dfcarbon_class$year >= regime_1_end)&(dfcarbon_class$year < regime_2_end)))
regime_3_index = (which(dfcarbon_class$year >= regime_2_end))

dfcarbon_class$regime[regime_1_index] = paste0("2006 - ",as.character(regime_1_end-1))
dfcarbon_class$regime[regime_2_index] = paste0(as.character(regime_1_end)," - ",as.character(regime_2_end-1))
dfcarbon_class$regime[regime_3_index] = paste0(as.character(regime_2_end)," - 2022")

ggplot(data=dfcarbon_class,aes(x = month,y = Chaetoceros_danicus)) + 
  geom_boxplot(alpha=0.5) + 
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  annotation_logticks(sides="l")+
  facet_grid(cols=vars(regime))+
  xlab("Month of Year")+
  ylab("Chaetoceros Danicus Concentration")
ggsave(filename="/dos/MIT-WHOI/community_synchrony/figures/biwavelet_coherence/group_level/chaetoceros_danicus-regime.png")

ggplot(data=dfcarbon_class,aes(x = week,y =Thalassiosira)) +
  geom_boxplot(alpha=0.5) + 
  scale_x_discrete(breaks=seq(1,52,5))+
  facet_grid(cols=vars(regime))+
  ylim(0,10000)+
  xlab("Month of Year")+
  ylab("Thalassiosira Concentration")
ggsave(filename="/dos/MIT-WHOI/community_synchrony/figures/biwavelet_coherence/group_level/thalassiosira-regime.png")
