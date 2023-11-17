
list.of.packages <- c("readxl","fields","ggplot2","sf","broom","dplyr","lubridate","sp")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)
remotes::install_github("NEFSC/NEFSC-Spatial")
library(NEFSCspatial)

load(paste0(basepath,"/data/ecomon_nutrients_2007_2023.RData"))
load(paste0(basepath,"/data/strata_nutrients.RData"))


regime_1_end = 2012
regime_2_end = 2018
regime_1_index = (which(dfj$year < regime_1_end))
regime_2_index = (which((dfj$year >= regime_1_end)&(dfj$year < regime_2_end)))
regime_3_index = (which(dfj$year >= regime_2_end))

dfj$regime <- NaN

dfj$regime[regime_1_index] = paste0("2006 - ",as.character(regime_1_end-1))
dfj$regime[regime_2_index] = paste0(as.character(regime_1_end)," - ",as.character(regime_2_end-1))
dfj$regime[regime_3_index] = paste0(as.character(regime_2_end)," - 2022")



#PLOTTING NUTRIENT PROFILES
strata_index = c(21,24,25,20)
dfj %>% filter(season=="Summer",nitrite_nitrate!=-999,STRATA %in% strata_index)  %>% ggplot() +
  geom_sf(data=EcoMon_Strata$geometry[strata_index])+
  geom_point(aes(x=lon,y=lat,color=nitrite_nitrate))

dfj %>% filter(season=="Summer",nitrite_nitrate!=-999,STRATA %in% c(21,24,25,20))  %>% ggplot() +
  geom_point(aes(x=nitrite_nitrate,y=depth_sampling)) + facet_grid(cols=vars(regime))+
  scale_y_reverse()+
  xlab(expression("Nitrite and Nitrate (umol kg"^-1*")"))+ylab("Depth (m)")

ggsave(filename=paste0(basepath,"/figures/environmental/nutrients/nitrite_nitrate_profiles_regime.png"),width=800,height=600,units="px",dpi=120)

dfj %>% filter(season=="Summer",silicate!=-999,STRATA %in% c(21,24,25,20))  %>% ggplot() +
  geom_point(aes(x=silicate,y=depth_sampling)) + facet_grid(cols=vars(regime))+
  scale_y_reverse()+
  xlab(expression("Silicate (umol kg"^-1*")"))+ylab("Depth (m)")

ggsave(filename=paste0(basepath,"/figures/environmental/nutrients/silicate_profiles_regime.png"),width=800,height=600,units="px",dpi=120)


dfj %>% filter(season=="Summer",phosphate!=-999,STRATA %in% c(21,24,25,20))  %>% ggplot() +
  geom_point(aes(x=phosphate,y=depth_sampling)) + facet_grid(cols=vars(regime))+
  scale_y_reverse()+
  xlab(expression("Phosphate (umol kg"^-1*")"))+ylab("Depth (m)")

ggsave(filename=paste0(basepath,"/figures/environmental/nutrients/phosphate_profiles_regime.png"),width=800,height=600,units="px",dpi=120)


dfj %>% filter(season=="Summer",ammonia!=-999,STRATA %in% c(21,24,25,20))  %>% ggplot() +
  geom_point(aes(x=ammonia,y=depth_sampling)) + facet_grid(cols=vars(regime))+
  scale_y_reverse()+
  xlab(expression("Ammonia (umol kg"^-1*")"))+ylab("Depth (m)")

ggsave(filename=paste0(basepath,"/figures/environmental/nutrients/ammonia_profiles_regime.png"),width=800,height=600,units="px",dpi=120)



dfj %>% filter(season=="Summer",nitrite_nitrate!=-999,STRATA %in% c(21,24,25,20))  %>% ggplot() +
  geom_point(aes(x=nitrite_nitrate,y=depth_sampling)) + facet_grid(cols=vars(year))+
  scale_y_reverse()+
  xlab(expression("Nitrite and Nitrate (umol kg"^-1*")"))+ylab("Depth (m)")



dfj %>% filter(season=="Summer",nitrite_nitrate!=-999,STRATA %in% c(21,24,25,20))  %>% ggplot() +
  geom_point(aes(x=nitrite_nitrate,y=depth_sampling)) + facet_grid(cols=vars(year))+
  scale_y_reverse()+
  xlab(expression("Silicate (umol kg"^-1*")"))+ylab("Depth (m)")

