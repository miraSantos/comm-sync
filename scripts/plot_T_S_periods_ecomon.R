
#PURPOSE:
list.of.packages <- c("readxl","fields","ggplot2","sf","broom","dplyr",
                      "lubridate","sp","tidyr","scales","formula.tools",
                      "ggpubr","DescTools","gsw")
new.packages <- list.of.packages[!(list.of.packages %in%
                                     installed.packages()[,"Package"])]
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
gom_nearshore= c(36)
gom = c(36,34,41,42,37,33)

# # 
# strata_index = mvco_strata
# strata_name="mvco_nearshore"

strata_index = gom_basin
strata_name = "GoM_BASIN"

# strata_index = gom_nearshore
# strata_name = "GoM Nearshore"

# strata_index = gom
# strata_name = "GoM_FULL"

df_gom <-read.csv("/home/mira/MIT-WHOI/Week.2024.02.18-24/characteristic_gom_water.csv",header =T)
###########################################################
#generate TS PLOTS with YEAR as COLOR
dfj %>%
  filter(season=="Summer",
         temp!=-999,
         salinity!=-999,
         STRATA %in% strata_index,
         year>=2006)%>%
  ggplot()+
  geom_point(aes(x=salinity,
                 y=temp,
                 color=as.factor(year),shape=as.factor(year)),alpha=0.7)+
  geom_point(data = df_gom,aes(x=salinity,y=temperature))+
  geom_text(data=df_gom,aes(x=salinity, y =temperature, label = Name),hjust = 1.2,vjust =1)+
  scale_shape_manual(values=1:20) +
  xlab("Salinity (psu)") + 
  ylab(expression("Temperature ("*degree* "C)"))

#########################################
#generate data frame with column for strata_group i.e. column where each row is labeled with mvco_strat GOM_basin etc.
dfj_strata <- dfj %>% mutate(strata_group = if_else(STRATA %in% mvco_strata,"mvco_strata",
                             if_else(STRATA %in% mvco_offshore,"MVCO_offshore",
                                     if_else(STRATA %in% gom_basin,"GOM_basin",
                                             if_else(STRATA %in% gom_nearshore, "GOM_nearshore",NA)))))

############################# GRIDDED PLOT
#TS plot per year as each facet
dfj %>%
  filter(season=="Summer",
         STRATA %in% strata_index,
         temp!=-999,salinity!=-999,
         salinity > 25,
         regime!=NaN,!is.na(depth_sampling))%>% 
  ggplot() + geom_point(aes(x=salinity,y=temp,color=depth_sampling),alpha=0.7)+
  facet_grid(cols = vars(year))+
  xlab("Salinity (psu)") + ylab(expression("Temperature ("*degree*"C)"))+
  scale_color_gradient(trans="reverse")

ggsave(filename=paste0(basepath,"/figures/environmental/TS_plots/T_S_yearly_SUMMER_",strata_name,".png"),
       width=2000,height=400,units="px",dpi=120)

#TS plot per year as each facet
dfj %>%
  filter((season=="Fall"| season == "Winter"),
         STRATA %in% strata_index,
         temp!=-999,salinity!=-999,
         salinity > 25,
         regime!=NaN,!is.na(depth_sampling))%>% 
  ggplot() + geom_point(aes(x=salinity,y=temp,color=depth_sampling),alpha=0.7)+
  facet_grid(cols = vars(year))+
  xlab("Salinity (psu)") + ylab(expression("Temperature ("*degree*"C)"))+
  scale_color_gradient(trans="reverse")

ggsave(filename=paste0(basepath,"/figures/environmental/TS_plots/T_S_yearly_FALL WINTER_",strata_name,".png"),
       width=2000,height=400,units="px",dpi=120)

mvco_nearshore_lon = c(-73,-69)
mvco_nearshore_lat = c(40,42)

lon = mvco_nearshore_lon
lat = mvco_nearshore_lat

dfj %>% 
  filter((season=="Fall"| season == "Winter"),
         STRATA %in% strata_index,
         temp!=-999,salinity!=-999,
         salinity > 25,
         regime!=NaN,!is.na(depth_sampling))%>%
  ggplot() + geom_sf(data=nes_shp) + 
  geom_point(aes(x=lon,y=lat,color=temp))+facet_grid(cols=vars(regime))+
  xlim(lon)+ #longitude
  ylim(lat)+ #latitude
  scale_color_gradient(trans="reverse",high="green",low="darkblue")
###############################################################
#PLOTTING TS plot per period colored by sampling depth below 20m depth
dfj %>% mutate(lat=signif(lat,4),
               lon=signif(lon,4))%>%
  filter((season=="Fall"| season =="Winter"),
         STRATA %in% strata_index,
         temp!=-999,salinity!=-999,salinity >=20,
         regime!=NaN,!is.na(depth_sampling))%>% 
  ggplot() + geom_point(aes(x=salinity,y=temp,color=depth_sampling))+
  geom_point(data = df_gom,aes(x=salinity,y=temperature))+
  geom_text(data=df_gom,aes(x=salinity, y =temperature, label = Name),hjust = 1.2,vjust =1)+
  facet_grid(cols = vars(regime))+
  xlab("Salinity (psu)") + ylab(expression("Temperature ("*degree*"C)"))+
  scale_color_gradient(trans='reverse',name="Sampling Depth (m)")

ggsave(filename=paste0(basepath,"/figures/environmental/TS_plots/T_S_depth_strat_",strata_name,".png"),
       width=1000,height=600,units="px",dpi=120)

########################
#generate TS PLOTS OVER PERIOD and DEPTH FACETS
dfj_strata %>% mutate(lat=signif(lat,4),
                      lon=signif(lon,4),
               depth_sampling_binned= cut(depth_sampling,breaks= seq(0,50,10),
                                          include.lowest=T)) %>%
  filter(season=="Summer",
         temp!=-999,salinity!=-999,
        strata_group != NaN,
        depth_sampling_binned!="NA",
         regime!=NaN)%>%
  group_by(date,lat,lon)%>%
  ggplot() + 
  geom_point(aes(x=salinity,y=temp,color=strata_group,shape=strata_group),alpha=0.7)+
  facet_grid(cols = vars(regime),rows=vars(depth_sampling_binned))+
  xlab("Salinity (psu)") + ylab("Temperature (Deg C)")+
  xlim(30,35)+ylim(5,20)+
  scale_color_discrete(name="Strata Group",labels=c("GoM basin","GoM nearshore","MVCO Offshore","MVCO Nearshore"))+
  scale_shape_manual(values=c(1,2,3,4),name="Strata Group",labels=c("GoM basin","GoM nearshore","MVCO Offshore","MVCO Nearshore"))
  

ggsave(filename=paste0(basepath,"/figures/environmental/TS_plots/T_S_depth_strata_all.png"),
       width=1000,height=1000,units="px",dpi=120)
                     
#####################################################
#generate TS PLOTS OVER PERIOD and DEPTH FACETS
dfj_strata %>% mutate(lat=signif(lat,4),lon=signif(lon,4),
               depth_sampling_binned= cut(depth_sampling,breaks= seq(0,50,10),include.lowest=T)) %>%
  filter(season=="Summer",
         temp!=-999,salinity!=-999,
        strata_group != NaN,
        depth_sampling_binned!="NA",
         regime!=NaN)%>%
  group_by(date,lat,lon)%>%
  ggplot() + 
  geom_point(aes(x=salinity,y=temp,color=strata_group,shape=strata_group),alpha=0.7)+
  facet_grid(cols = vars(regime),rows=vars(depth_sampling_binned))+
  xlab("Salinity (psu)") + ylab("Temperature (Deg C)")+
  xlim(30,35)+ylim(5,20)+
  scale_color_discrete(name="Strata Group",labels=c("GoM basin","GoM nearshore","MVCO Offshore","MVCO Nearshore"))+
  scale_shape_manual(values=c(1,2,3,4),name="Strata Group",labels=c("GoM basin","GoM nearshore","MVCO Offshore","MVCO Nearshore"))
  
                    
                    
#############################################################

#plot boxplot of salinity by depth and period
dfj_strata %>% mutate(lat=signif(lat,4),lon=signif(lon,4),
                      depth_sampling_binned= cut(depth_sampling,breaks= seq(0,50,10))) %>%
  filter(season=="Summer",
         temp!=-999,salinity!=-999,
         salinity_flag==2,
         strata_group != NaN,
         depth_sampling_binned!="NA",
         regime!=NaN,(strata_group=="mvco_strata"|strata_group=="MVCO_offshore"))%>%
  group_by(date,lat,lon)%>%
  ggboxplot(x="strata_group",y="salinity",color="strata_group") + 
  facet_grid(cols=vars(regime),rows=vars(depth_sampling_binned)) +
  stat_compare_means(label = "p.signif",
                     method = "t.test",
                     label.y=33)+
  grids(linetype = "dashed")

ggsave(filename=paste0(basepath,"/figures/environmental/TS_plots/stats_T_S_depth_strata_mvco_nearshore_offshore.png"),
       width=1000,height=1000,units="px",dpi=120)


#BOXPLOT OF SALINITY OVER PERIOD AND DEPTH
dfj_strata %>% mutate(lat=signif(lat,4),lon=signif(lon,4),
                      depth_sampling_binned= cut(depth_sampling,breaks= seq(0,50,10),include.lowest=T)) %>%
  filter(season=="Summer",
         temp!=-999,salinity!=-999,
         strata_group != NaN,
         depth_sampling_binned!="NA",
         regime!=NaN)%>%
  group_by(date,lat,lon)%>%
  ggplot() + 
geom_boxplot(aes(x=salinity,y=strata_group,group=strata_group,color=strata_group))+
    facet_grid(cols = vars(regime),rows=vars(depth_sampling_binned))+
  xlab("Salinity (psu)") +
  xlim(30,33)+
  scale_color_discrete(name="Strata Group",labels=c("GoM basin","GoM nearshore","MVCO Offshore","MVCO Nearshore"))+
  scale_shape_manual(values=c(1,2,3,4),name="Strata Group",labels=c("GoM basin","GoM nearshore","MVCO Offshore","MVCO Nearshore"))


ggboxplot(data=df_temp,x="regime",y="nut_max",color="regime")+
  stat_compare_means(method="anova",label.y=max(df_temp$nut_max)+1)+
  xlab("Period") + ylab(expression("[Nitrite+Nitrate] (umol kg"^-1*")"))+
  stat_compare_means(label = "p.signif", method = "t.test",
                     ref.group = ".all.")+ labs(color="Period")+
  grids(linetype = "dashed")
ggsave(filename=paste0(basepath,"/figures/environmental/nutrients/nitrite_nitrate/nitrite_max_anova_regime_",strata_name,".png"),
       width=600,height=500,units="px",dpi=120)

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

###########################################################################
dfj$month = month(dfj$date)
dfj$year = year(dfj$date)

#generate TS PLOTS with YEAR as COLOR entire season, monhtly mean tempearture
dfj %>%
  filter(
         temp!=-999,
         salinity!=-999,
         STRATA %in% strata_index,
         depth_sampling <=150,
                  year>=2006)%>%
  group_by(month,year) %>% summarise(ym_sal = mean(salinity,na.rm=T),
                                     ym_temp=mean(temp,na.rm=T)) %>% ungroup() %>%
  ggplot()+
  geom_point(aes(x=ym_sal,y=ym_temp,color=as.factor(year)),size=4,alpha=0.5)+
  geom_point(data = df_gom,aes(x=salinity,y=temperature),shape=2)+
  geom_text(data=df_gom,aes(x=salinity, y =temperature, label = Name),hjust = 1.2,vjust =1)+
  scale_color_manual(values=rainbow(21))+
  xlab("Salinity (psu)")+ 
  ylab(expression("Temperature ("*degree* "C)"))+
  scale_x_continuous(breaks = seq(30,36,1),limits=c(30,36))+
  scale_y_continuous(breaks = seq(0,16,1),limits=c(0,16))


ggsave(filename=paste0(basepath,"/figures/environmental/TS_plots/T_S_depth_strata_yearly_",strata_name,".png"),
       width=2000,height=600,units="px",dpi=120)


dfj$week = week(dfj$date)
#MVCO
dfj %>%
  filter(season == "Summer",
    temp!=-999,
    salinity!=-999,
    STRATA %in% strata_index,
    depth_sampling > 10,
    year>=2006)%>%
  group_by(week,year) %>% summarise(ym_sal = mean(salinity,na.rm=T),
                                     ym_temp=mean(temp,na.rm=T)) %>% ungroup() %>%
  ggplot()+
  geom_point(aes(x=ym_sal,y=ym_temp,color=as.factor(year)),size=4,alpha=0.5)+
  geom_point(data = df_gom,aes(x=salinity,y=temperature),shape=2)+
  geom_text(data=df_gom,aes(x=salinity, y =temperature, label = Name),hjust = 1.2,vjust =1)+
  scale_color_manual(values=rainbow(21))+
  xlab("Salinity (psu)")+ 
  ylab(expression("Temperature ("*degree* "C)"))+
  scale_x_continuous(breaks = seq(30,36,1),limits=c(30,36))+
  scale_y_continuous(breaks = seq(0,16,1),limits=c(0,16))


ggsave(filename=paste0(basepath,"/figures/environmental/TS_plots/T_S_depth_SUMMER_strata_yearly_",strata_name,".png"),
       width=800,height=600,units="px",dpi=120)
