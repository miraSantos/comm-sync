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
#Strata for mvco
mvco_strata = c(21,24,25,20)
mvco_nearshore_lon = c(-73,-69)
mvco_nearshore_lat = c(40,42)

gom_basin = c(41,42,37,34)
mvco_offshore = c(19,23)
northeast_channel = c(38,39)
gom_nearshore= c(36)


gom_nearshore_lon = c(-71,-69)
gom_nearshore_lat = c(41,43)

# strata_index = mvco_strata
# strata_name="mvco_nearshore"
# strata_index = mvco_offshore
# strata_name = "MVCO_offshore"
# strata_index = gom_basin
# strata_name = "GOM_BASIN"
strata_index = gom_nearshore
strata_name = "GOM_nearshore"
lon = gom_nearshore_lon
lat = gom_nearshore_lat
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

#plot map of deep nitrate mean
#MVCO area

df_temp <- temp %>% group_by(profile_id) %>% mutate(nut_max = max(nitrite_nitrate))  %>%
  filter(depth_sampling >= 20,depth_sampling <= 40,nitrite_nitrate >=0.1) %>%
  mutate(nit_sum_deep =mean(nitrite_nitrate,na.rm=T))


#map of deep nitrate mean
df_temp %>%
  ggplot() +
  facet_grid(cols=vars(regime))+
  geom_sf(data=nes_shp)+
  geom_point(aes(x=lon,y=lat,color=nit_sum_deep))+
  xlim(lon)+ #longitude
  ylim(lat)+ #latitude
  scale_color_gradient(low="red",high="green",name=expression("Deep Nitrate Mean ("*mu*"mol kg"^-1*")"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  xlab("Longitude") + ylab("Latitude")

ggsave(filename=paste0(basepath,"/figures/environmental/nutrients/nitrite_nitrate/nitrate_maps/nitrite_deep_mean_map_regime_",strata_name,".png"),
       width=1200,height=400,units="px",dpi=140)

#map of nutrient max
df_temp %>%
  ggplot() +
  facet_grid(cols=vars(regime))+
  geom_sf(data=nes_shp)+
  geom_point(aes(x=lon,y=lat,color=nut_max))+
  xlim(lon)+ #longitude
  ylim(lat)+ #latitude
  scale_color_gradient(low="red",high="green",
                       name=expression("Deep Nitrate Mean ("*mu*"mol kg"^-1*")")
                       )+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  xlab("Longitude") + ylab("Latitude")

ggsave(filename=paste0(basepath,"/figures/environmental/nutrients/nitrite_nitrate/nitrate_maps/nitrite_deep_max_map_regime_",strata_name,".png"),
       width=1200,height=400,units="px",dpi=140)
