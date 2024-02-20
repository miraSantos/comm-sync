#PURPOSE:
list.of.packages <- c("readxl","fields","ggplot2","sf","broom","dplyr","lubridate","sp",
                      "tidyr","scales","formula.tools","ggpubr","DescTools","gsw","grDevices")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

remotes::install_github("NEFSC/NEFSC-Spatial") #loads NEFSC Spatial Data
library(NEFSCspatial)

load(paste0(basepath,"/data/ecomon_nutrients_2007_2023.RData"))
load(paste0(basepath,"/data/strata_nutrients.RData"))

strata_index = mvco_strata

#PLOTTING NUTRIENT PROFILES
mvco_strata = c(21,24,25,20)
gom_basin = c(41,42,37,34)
mvco_offshore = c(19,23)
northeast_channel = c(38,39)
gom_nearshore= c(36)

# strata_index = mvco_strata
# strata_name="mvco_nearshore"
# strata_index = mvco_offshore
# strata_name = "MVCO_offshore"
# strata_index = gom_basin
# strata_name = "GOM_BASIN"
strata_index = gom_nearshore
strata_name = "GOM_nearshore"
regime_name = "2006 - 2011"
# regime_name = "2012 - 2017"
# regime_name = "2018 - 2022"

#create data frame
temp <- dfj %>% filter(season=="Summer",STRATA %in% strata_index,
                       nitrite_nitrate_flag==2,nitrite_nitrate!=-999,
                       regime != NaN) %>%
  mutate(lat=signif(lat,4),lon=signif(lon,4)) %>%
  group_by(date,lat,lon)%>%
  mutate(profile_id = cur_group_id())

######################
#STATISTICS
#####################
nutrient_name= "Nitrite+Nitrate"
df_temp <- temp %>% group_by(profile_id) %>% mutate(nut_max = max(nitrite_nitrate))  %>%
  filter(depth_sampling >= 20,depth_sampling <= 40,nitrite_nitrate >=0.1) %>%
  mutate(nit_sum_deep =mean(nitrite_nitrate,na.rm=T))

ggboxplot(data=df_temp,x="regime",y="nut_max",color="regime")+
  stat_compare_means(method="anova",label.y=max(df_temp$nut_max)+1)+
  xlab("Period") + ylab(expression("[Nitrite+Nitrate] (umol kg"^-1*")"))+
  stat_compare_means(label = "p.signif", method = "t.test",
                     ref.group = ".all.")+ labs(color="Period")+
  grids(linetype = "dashed")
ggsave(filename=paste0(basepath,"/figures/environmental/nutrients/nitrite_nitrate/nitrite_max_anova_regime_",strata_name,".png"),
       width=600,height=500,units="px",dpi=120)

# Statistical comparison of nit deep mean
ggboxplot(data=df_temp,x="regime",y="nit_sum_deep",color="regime")+
  stat_compare_means(method="anova",label.y=max(df_temp$nit_sum_deep)+1)+
  xlab("Period") + ylab(expression("[Nitrite+Nitrate] (umol kg"^-1*")"))+
  stat_compare_means(label = "p.signif", method = "t.test",
                     ref.group = ".all.")+ labs(color="Period")+
  grids(linetype = "dashed")

ggsave(filename=paste0(basepath,"/figures/environmental/nutrients/nitrite_nitrate/nitrite_deep_mean_anova_regime_",strata_name,".png"),
       width=600,height=500,units="px",dpi=120)


####################################################
nutrient_name="Silicate"
df_temp <- temp %>% group_by(profile_id) %>% mutate(nut_max = max(silicate))   %>% filter(depth_sampling >= 10,silicate >= 0) %>% mutate(nut_sum_deep =mean(silicate,na.rm=T)) 

ggboxplot(data=df_temp,x="regime",y="nut_max",color="regime")+
  stat_compare_means(method="anova",label.y=max(df_temp$nut_max)+1)+
  xlab("Period") + ylab(expression("[Silicate] (umol kg"^-1*")"))+
  stat_compare_means(label = "p.signif", method = "t.test",
                     ref.group = ".all.")+ labs(color="Period")+
  grids(linetype = "dashed") + 
  ggtitle(paste0("Max ",nutrient_name," Level Across Regimes"))

ggsave(filename=paste0(basepath,"/figures/environmental/nutrients/silicate/silicate_max_anova_regime_",strata_name,".png"),
       width=600,height=500,units="px",dpi=120)

ggboxplot(data=df_temp,x="regime",y="nut_sum_deep",color="regime")+
  stat_compare_means(method="anova",label.y=max(df_temp$nut_sum_deep)+1)+
  xlab("Period") + ylab(expression("[Silicate] (umol kg"^-1*")"))+
  stat_compare_means(label = "p.signif", method = "t.test",
                     ref.group = ".all.")+ labs(color="Period")+
  grids(linetype = "dashed")

ggsave(filename=paste0(basepath,"/figures/environmental/nutrients/silicate/silicate_deep_mean_anova_regime_",strata_name,".png"),
       width=600,height=500,units="px",dpi=120)

###############################################################################################
nutrient_name="Phosphate"
df_temp <- temp %>% group_by(profile_id) %>% filter(phosphate < 7) %>% mutate(nut_max = max(phosphate))   %>% filter(depth_sampling >= 10,phosphate >= 0.5) %>% mutate(nut_sum_deep =mean(phosphate,na.rm=T)) 

ggboxplot(data=df_temp,x="regime",y="nut_max",color="regime")+
  stat_compare_means(method="anova",label.y=max(df_temp$nut_max)+1)+
  xlab("Period") + ylab(expression("[Phosphate] (umol kg"^-1*")"))+
  stat_compare_means(label = "p.signif", method = "t.test",
                     ref.group = ".all.")+ labs(color="Period")+
  grids(linetype = "dashed") + 
  ggtitle(paste0("Max ",nutrient_name," Level Across Regimes"))

ggsave(filename=paste0(basepath,"/figures/environmental/nutrients/silicate/silicate_max_anova_regime_",strata_name,".png"),
       width=600,height=500,units="px",dpi=120)

ggboxplot(data=df_temp,x="regime",y="nut_sum_deep",color="regime")+
  stat_compare_means(method="anova",label.y=max(df_temp$nut_sum_deep)+1)+
  xlab("Period") + ylab(expression("[Phosphate] (umol kg"^-1*")"))+
  stat_compare_means(label = "p.signif", method = "t.test",
                     ref.group = ".all.")+ labs(color="Period")+
  grids(linetype = "dashed")

ggsave(filename=paste0(basepath,"/figures/environmental/nutrients/silicate/silicate_deep_mean_anova_regime_",strata_name,".png"),
       width=600,height=500,units="px",dpi=120)

########################################################



strata_index = gom_basin
strata_name = "GOM_BASIN"

regime_name = "2006 - 2011"
# regime_name = "2012 - 2017"
# regime_name = "2018 - 2022"

#create data frame
temp <- dfj %>% filter((season=="Winter" | season == "Spring"),STRATA %in% strata_index,
                       nitrite_nitrate_flag==2,nitrite_nitrate!=-999,
                       regime != NaN) %>%
  mutate(lat=signif(lat,4),lon=signif(lon,4)) %>%
  group_by(date,lat,lon)%>%
  mutate(profile_id = cur_group_id())

#Plot an individual profile
temp %>% filter(profile_id==6)%>%
  ggplot() + geom_line(aes(y=nitrite_nitrate,x=depth_sampling)) + 
  geom_point(aes(y=nitrite_nitrate,x=depth_sampling),size=1) +
  scale_x_reverse()+coord_flip()

#plotting all three regimes 
dfj %>% filter((season=="Winter" | season == "Spring"),STRATA %in% strata_index,
               nitrite_nitrate_flag==2,nitrite_nitrate != -999,
               regime!=NaN) %>%
  mutate(lat=signif(lat,4),lon=signif(lon,4)) %>%
  group_by(date,lat,lon)%>%
  mutate(profile_id = cur_group_id()) %>%
  ggplot() + geom_line(aes(y=nitrite_nitrate,x=depth_sampling,color=as.factor(profile_id))) + 
  geom_point(aes(y=nitrite_nitrate,x=depth_sampling,color=as.factor(profile_id))) + 
  scale_x_reverse()+coord_flip()+
  facet_grid(cols=vars(regime))+
  ylim(0,6)+
  xlim(110,0)+
  scale_colour_discrete(name = "Profile ID")+
  xlab("Depth (m)") + ylab(expression("[Nitrite+Nitrate] (umol kg"^-1*")"))+
  guides(color = FALSE, size = FALSE)

ggsave(filename=paste0(basepath,"/figures/environmental/nutrients/nitrite_nitrate/nitrite_mean_regime_winter_spring_",strata_name,".png"),
       width=1000,height=600,units="px",dpi=120)

dfj %>% filter((season=="Winter" | season == "Spring"),STRATA %in% strata_index,
               nitrite_nitrate_flag==2,nitrite_nitrate != -999,
               regime!=NaN) %>%
  mutate(lat=signif(lat,4),lon=signif(lon,4)) %>%
  group_by(date,lat,lon)%>%
  mutate(profile_id = cur_group_id()) %>%
  ggplot() + geom_line(aes(y=silicate,x=depth_sampling,color=as.factor(profile_id))) + 
  geom_point(aes(y=silicate,x=depth_sampling,color=as.factor(profile_id))) + 
  scale_x_reverse()+coord_flip()+
  facet_grid(cols=vars(regime))+
  ylim(0,6)+
  scale_colour_discrete(name = "Profile ID")+
  xlab("Depth (m)") + ylab(expression("[Silicate] (umol kg"^-1*")"))+
  guides(color = FALSE, size = FALSE)

ggsave(filename=paste0(basepath,"/figures/environmental/nutrients/silicate/silicate_mean_regime_winter_spring_",strata_name,".png"),
       width=1000,height=600,units="px",dpi=120)

######################
nutrient_name= "Nitrite+Nitrate"
df_temp <- temp %>% group_by(profile_id) %>% mutate(nut_max = max(nitrite_nitrate))  %>% filter(depth_sampling >= 20,depth_sampling <= 40) %>% mutate(nit_sum_deep =mean(nitrite_nitrate,na.rm=T))

ggboxplot(data=df_temp,x="regime",y="nut_max",color="regime")+
  stat_compare_means(method="anova",label.y=max(df_temp$nut_max)+1)+
  xlab("Period") + ylab(expression("[Nitrite+Nitrate] (umol kg"^-1*")"))+
  stat_compare_means(label = "p.signif", method = "t.test",
                     ref.group = ".all.")+ labs(color="Period")+
  grids(linetype = "dashed")
ggsave(filename=paste0(basepath,"/figures/environmental/nutrients/nitrite_nitrate/nitrite_max_anova_regime_",strata_name,".png"),
       width=600,height=500,units="px",dpi=120)

ggboxplot(data=df_temp,x="regime",y="nit_sum_deep",color="regime")+
  stat_compare_means(method="anova",label.y=max(df_temp$nit_sum_deep)+1)+
  xlab("Period") + ylab(expression("[Nitrite+Nitrate] (umol kg"^-1*")"))+
  stat_compare_means(label = "p.signif", method = "t.test",
                     ref.group = ".all.")+ labs(color="Period")+
  grids(linetype = "dashed")

ggsave(filename=paste0(basepath,"/figures/environmental/nutrients/nitrite_nitrate/nitrite_deep_mean_anova_regime_",strata_name,".png"),
       width=600,height=500,units="px",dpi=120)
####################################################
nutrient_name="Silicate"
df_temp <- temp %>% group_by(profile_id) %>% mutate(nut_max = max(silicate))   %>% filter(depth_sampling >= 10,silicate >= 0) %>% mutate(nut_sum_deep =mean(silicate,na.rm=T)) 

ggboxplot(data=df_temp,x="regime",y="nut_max",color="regime")+
  stat_compare_means(method="anova",label.y=max(df_temp$nut_max)+1)+
  xlab("Period") + ylab(expression("[Silicate] (umol kg"^-1*")"))+
  stat_compare_means(label = "p.signif", method = "t.test",
                     ref.group = ".all.")+ labs(color="Period")+
  grids(linetype = "dashed") + 
  ggtitle(paste0("Max ",nutrient_name," Level Across Regimes"))

ggsave(filename=paste0(basepath,"/figures/environmental/nutrients/silicate/silicate_max_anova_regime_",strata_name,".png"),
       width=600,height=500,units="px",dpi=120)

ggboxplot(data=df_temp,x="regime",y="nut_sum_deep",color="regime")+
  stat_compare_means(method="anova",label.y=max(df_temp$nut_sum_deep)+1)+
  xlab("Period") + ylab(expression("[Silicate] (umol kg"^-1*")"))+
  stat_compare_means(label = "p.signif", method = "t.test",
                     ref.group = ".all.")+ labs(color="Period")+
  grids(linetype = "dashed")

ggsave(filename=paste0(basepath,"/figures/environmental/nutrients/silicate/silicate_deep_mean_anova_regime_",strata_name,".png"),
       width=600,height=500,units="px",dpi=120)
