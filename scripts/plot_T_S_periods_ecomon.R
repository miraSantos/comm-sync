
#PURPOSE:
list.of.packages <- c("readxl","fields","ggplot2","sf","broom","dplyr","lubridate","sp","tidyr","scales","formula.tools","ggpubr","DescTools","gsw")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

remotes::install_github("NEFSC/NEFSC-Spatial") #loads NEFSC Spatial Data
library(NEFSCspatial)

load(paste0(basepath,"/data/ecomon_nutrients_2007_2023.RData"))
load(paste0(basepath,"/data/strata_nutrients.RData"))
#PLOTTING NUTRIENT PROFILES
mvco_strata = c(21,24,25,20)
gom_basin = c(41,42,37,34)
mvco_offshore = c(19,23)
northeast_channel = c(38,39)
strata_index = mvco_strata
strata_name="mvco_nearshore"
strata_index = mvco_offshore
strata_name = "MVCO_offshore"


dfj_strata <- dfj %>% mutate(strata_group = if_else(STRATA %in% mvco_strata,"mvco_strata",
                             if_else(STRATA %in% mvco_offshore,"MVCO_offshore",
                                     if_else(STRATA %in% gom_basin,"GOM_basin",NA))))

dfj %>% filter(season=="Summer",STRATA %in% strata_index,
               nitrite_nitrate_flag==2,nitrite_nitrate!=-999,
               regime!=NaN) %>%
  mutate(lat=signif(lat,4),lon=signif(lon,4)) %>%
  group_by(date,lat,lon)%>%
  ggplot() +
  facet_grid(cols=vars(regime))+
  geom_sf(data=nes_shp)+
  geom_point(aes(x=lon,y=lat,color=nitrite_nitrate))+
  xlim(-75,-69)+
  ylim(39,43)


############################# GRIDDED PLOT
dfj %>% mutate(lat=signif(lat,4),lon=signif(lon,4),
                 depth_sampling_binned= cut(depth_sampling,breaks= seq(0,50,10),include.lowest=T)) %>%
  filter(season=="Summer",
         STRATA %in% strata_index,
         temp!=-999,salinity!=-999,
         depth_sampling >= 20,
         regime!=NaN)%>%
  group_by(date,lat,lon)%>%
  ggplot() + geom_point(aes(x=salinity,y=temp))+
  facet_grid(rows = vars(depth_sampling_binned),
             cols = vars(regime))+
  xlab("Salinity (psu)") + ylab("Temperature (Deg C)")


###############################################################
dfj %>% mutate(lat=signif(lat,4),lon=signif(lon,4),
               depth_sampling_binned= cut(depth_sampling,breaks= seq(0,50,10),include.lowest=T)) %>%
  filter(season=="Summer",
         STRATA %in% strata_index,
         temp!=-999,salinity!=-999,
         depth_sampling >= 20,
         regime!=NaN)%>%
  group_by(date,lat,lon)%>%
  ggplot() + geom_point(aes(x=salinity,y=temp,color=depth_sampling))+
  facet_grid(cols = vars(regime))+
  xlab("Salinity (psu)") + ylab("Temperature (Deg C)")+xlim(30,35)+ylim(5,20)

dfj_test <- dfj %>% mutate(lat=signif(lat,4),lon=signif(lon,4),
               depth_sampling_binned= cut(depth_sampling,breaks= seq(0,50,10),include.lowest=T))
  

ggsave(filename=paste0(basepath,"/figures/environmental/T_S_depth_strat_",strata_name,".png"),
       width=1000,height=600,units="px",dpi=120)

########################
dfj_strata %>% mutate(lat=signif(lat,4),lon=signif(lon,4),
               depth_sampling_binned= cut(depth_sampling,breaks= seq(0,50,10),include.lowest=T)) %>%
  filter(season=="Summer",
         temp!=-999,salinity!=-999,
        strata_group != NaN,
        depth_sampling_binned!="NA",
         regime!=NaN)%>%
  group_by(date,lat,lon)%>%
  ggplot() + 
  geom_point(aes(x=salinity,y=temp,color=strata_group,shape=strata_group),alpha=0.4)+
  facet_grid(cols = vars(regime),rows=vars(depth_sampling_binned))+
  xlab("Salinity (psu)") + ylab("Temperature (Deg C)")+
  xlim(30,35)+ylim(5,20)+
  scale_color_discrete(name="Strata Group",labels=c("GoM","MVCO Offshore","MVCO Nearshore"))

#################### GOM BASIN
strata_index = gom_basin
strata_name = "GOM_BASIN"

dfj %>% mutate(lat=signif(lat,4),lon=signif(lon,4),
               depth_sampling_binned= cut(depth_sampling,breaks= seq(0,300,10),include.lowest=T)) %>%
  filter(season=="Summer",
         STRATA %in% strata_index,
         temp!=-999,salinity!=-999,
         depth_sampling_binned!= "NA",
         salinity >=30,
         regime!=NaN)%>%
  group_by(date,lat,lon)%>%
  ggplot() + 
  geom_point(aes(x=salinity,y=temp,color=depth_sampling),alpha=0.3)+
  scale_color_gradient(low="purple",high="red") +
  facet_grid(cols = vars(regime))+ #columns for each period
  xlab("Salinity (psu)") + ylab("Temperature (Deg C)")+xlim(30,35)+ylim(5,20)

ggsave(filename=paste0(basepath,"/figures/environmental/T_S_depth_strat_",strata_name,".png"),
       width=1000,height=600,units="px",dpi=120)


  
