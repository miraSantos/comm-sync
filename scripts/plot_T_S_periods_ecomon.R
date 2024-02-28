
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

strata_index = mvco_strata
strata_name="mvco_nearshore"

# strata_index = gom_basin
# strata_name = "GoM_BASIN"

# strata_index = mvco_offshore
# strata_name = "MVCO_offshore"

# strata_index = gom_nearshore
# strata_name = "GoM Nearshore"

df_gom <-read.csv("/home/mira/MIT-WHOI/Week.2024.02.18-24/characteristic_gom_water.csv",header =T)
###########################################################
#generate TS PLOTS with YEAR as COLOR
dfj %>%
  filter(season=="Summer",
         temp!=-999,
         salinity!=-999,
         STRATA %in% strata_index,
         depth_sampling > 10,
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

df_gom %>%  ggplot(aes(x=salinity,y=temperature,label=Name)) + geom_point()+
  geom_text(hjust = 0, nudge_x = 0.05)

#########################################
#generate data frame with column for strata_group i.e. column where each row is labeled with mvco_strat GOM_basin etc.
dfj_strata <- dfj %>% mutate(strata_group = if_else(STRATA %in% mvco_strata,"mvco_strata",
                             if_else(STRATA %in% mvco_offshore,"MVCO_offshore",
                                     if_else(STRATA %in% gom_basin,"GOM_basin",
                                             if_else(STRATA %in% gom_nearshore, "GOM_nearshore",NA)))))

############################# GRIDDED PLOT
#TS plot per period and depth
dfj %>% mutate(lat=signif(lat,4),
               lon=signif(lon,4),
                 depth_sampling_binned=cut(depth_sampling,breaks= seq(0,200,25),
                                            include.lowest=T))%>%
  filter(season=="Summer",
         STRATA %in% strata_index,
         temp!=-999,salinity!=-999,
         regime!=NaN,!is.na(depth_sampling))%>% 
  ggplot() + geom_point(aes(x=salinity,y=temp))+
  facet_grid(cols = vars(regime))+
  xlab("Salinity (psu)") + ylab(expression("Temperature ("*degree*"C)"))


###############################################################
#PLOTTING TS plot per period colored by sampling depth below 20m depth
dfj %>% mutate(lat=signif(lat,4),
               lon=signif(lon,4),
               depth_sampling_binned=cut(depth_sampling,breaks= seq(0,200,25),
                                         include.lowest=T))%>%
  filter(season=="Summer",
         
         STRATA %in% strata_index,
         temp!=-999,salinity!=-999,
         regime!=NaN,!is.na(depth_sampling))%>% 
  ggplot() + geom_point(aes(x=salinity,y=temp,color=depth_sampling))+
  facet_grid(cols = vars(regime))+
  xlab("Salinity (psu)") + ylab("Temperature (Deg C)")

ggsave(filename=paste0(basepath,"/figures/environmental/TS_plots/T_S_depth_strat_",strata_name,".png"),
       width=1000,height=600,units="px",dpi=120)

########################
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
  scale_shape_manual(values=c(1,2,3,4),name="Strata Group",labels=c("GoM basin","GoM nearshore","MVCO Offshore","MVCO Nearshore")
  
                     
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
  scale_shape_manual(values=c(1,2,3,4),name="Strata Group",labels=c("GoM basin","GoM nearshore","MVCO Offshore","MVCO Nearshore")
  
                    
                    
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


  
