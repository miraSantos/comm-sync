#PURPOSE:
list.of.packages <- c("readxl","fields","ggplot2","sf","broom","dplyr",
                      "lubridate","sp","tidyr","scales","formula.tools",
                      "ggpubr","DescTools","gsw")
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
gom_nearshore = c(36)
# strata_index = mvco_strata
# strata_name="mvco_nearshore"
# strata_index = mvco_offshore
# strata_name = "MVCO_offshore"
# strata_index = gom_basin
# strata_name = "GOM_BASIN"

strata_index = gom_nearshore
strata_name = "GoM Nearshore"

# regime_name = "2006 - 2011"
# regime_name = "2012 - 2017"
# regime_name = "2018 - 2022"

#create data frame
temp <- dfj %>% filter(season=="Summer",STRATA %in% strata_index,
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

#plotting all summer nutrient profiles periodly
################################
#NITRITE and NITRATE
dfj %>% filter(season=="Summer",STRATA %in% strata_index,
               nitrite_nitrate_flag==2,nitrite_nitrate != -999,
               regime!=NaN) %>%
  mutate(lat=signif(lat,4),lon=signif(lon,4)) %>%
  group_by(date,lat,lon)%>%
  mutate(profile_id = cur_group_id()) %>%
  ggplot() + geom_line(aes(y=nitrite_nitrate,x=depth_sampling,color=as.factor(profile_id))) + 
  geom_point(aes(y=nitrite_nitrate,x=depth_sampling,color=as.factor(profile_id))) + 
  scale_x_reverse()+coord_flip()+
  facet_grid(cols=vars(regime))+
  scale_colour_discrete(name = "Profile ID")+
  xlab("Depth (m)") + ylab(expression("[Nitrite+Nitrate] ("*mu*"mol kg"^-1*")"))+
  guides(color = FALSE, size = FALSE)


ggsave(filename=paste0(basepath,
                       "/figures/environmental/nutrients/nitrite_nitrate/periodly_profiles/nitrate_profile_regime_",
                       strata_name,".png"),
       width=1000,height=600,units="px",dpi=120)

#plot summer temperature profiles over the three periods
dfj %>% filter(season=="Summer",STRATA %in% strata_index,
               nitrite_nitrate_flag==2,nitrite_nitrate != -999,temp != -999,
               regime!=NaN) %>%
  mutate(lat=signif(lat,4),lon=signif(lon,4)) %>%
  group_by(date,lat,lon)%>%
  mutate(profile_id = cur_group_id()) %>%
  ggplot() + geom_line(aes(y=temp,x=depth_sampling,color=as.factor(profile_id))) + 
  geom_point(aes(y=temp,x=depth_sampling,color=as.factor(profile_id))) + 
  scale_x_reverse()+coord_flip()+
  facet_grid(cols=vars(regime))+
  scale_colour_discrete(name = "Profile ID")+
  xlab("Depth (m)") + ylab(expression("Temperature ("*degree*"C)"))+
  guides(color = FALSE, size = FALSE)

ggsave(filename=paste0(basepath,
                       "/figures/environmental/temperature/periodly_profiles/temperature_profile_regime_",
                       strata_name,".png"),
       width=1000,height=600,units="px",dpi=120)

#plot summer salinity profiles over the three periods
dfj %>% filter(season=="Summer",STRATA %in% strata_index,
               nitrite_nitrate_flag==2,salinity != -999,
               regime!=NaN) %>%
  mutate(lat=signif(lat,4),lon=signif(lon,4)) %>%
  group_by(date,lat,lon)%>%
  mutate(profile_id = cur_group_id()) %>%
  ggplot() + geom_line(aes(y=salinity,x=depth_sampling,color=as.factor(profile_id))) + 
  geom_point(aes(y=salinity,x=depth_sampling,color=as.factor(profile_id))) + 
  scale_x_reverse()+coord_flip()+
  facet_grid(cols=vars(regime))+
  scale_colour_discrete(name = "Profile ID")+
  xlab("Depth (m)") + ylab("Salinity (psu)")+
  guides(color = FALSE, size = FALSE)

ggsave(filename=paste0(basepath,
                       "/figures/environmental/salinity/periodly_profiles/salinity_profile_regime_",
                       strata_name,".png"),
       width=1000,height=600,units="px",dpi=120)


#plot silicate profiles
dfj %>% filter(season=="Summer",STRATA %in% strata_index,
               silicate_flag==2,silicate != -999,
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
  xlab("Depth (m)") + ylab(expression("[Silicate]  ("*mu*"mol kg"^-1*")"))+
  guides(color = FALSE, size = FALSE)

ggsave(filename=paste0(basepath,"/figures/environmental/nutrients/silicate/silicate_mean_regime_",strata_name,".png"),
       width=1000,height=600,units="px",dpi=120)


#plot phosphate profiles
dfj %>% filter(season=="Summer",STRATA %in% strata_index,
               phosphate_flag==2, phosphate != -999,
               regime!=NaN) %>%
  mutate(lat=signif(lat,4),lon=signif(lon,4)) %>%
  group_by(date,lat,lon)%>%
  mutate(profile_id = cur_group_id()) %>%
  ggplot() + geom_line(aes(y=phosphate,x=depth_sampling,color=as.factor(profile_id))) + 
  geom_point(aes(y=phosphate,x=depth_sampling,color=as.factor(profile_id))) + 
  scale_x_reverse()+coord_flip()+
  facet_grid(cols=vars(regime))+
  ylim(0,2.5) + 
  scale_colour_discrete(name = "Profile ID")+
  xlab("Depth (m)") + ylab(expression("[PO"[3]^-4*"]  ("*mu*"mol kg"^-1*")"))+
  guides(color = FALSE, size = FALSE)

ggsave(filename=paste0(basepath,"/figures/environmental/nutrients/phosphate/phosphate_mean_regime_",strata_name,".png"),
       width=1000,height=600,units="px",dpi=120)

########################################################
