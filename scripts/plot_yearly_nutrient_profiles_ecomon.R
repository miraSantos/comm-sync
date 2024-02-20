

list.of.packages <- c("readxl","fields","ggplot2","sf","broom","dplyr","lubridate","sp","tidyr","scales","formula.tools")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)
remotes::install_github("NEFSC/NEFSC-Spatial")
library(NEFSCspatial)

ggplot() + geom_sf(data=EcoMon_Strata)+
  geom_sf(data=EcoMon_Strata[which(EcoMon_Strata$STRATA%in%strata_index),],color="red")+
  geom_text(data=EcoMon_Strata,aes(label = STRATA, x = X, y = Y))+
  geom_text(data=EcoMon_Strata[which(EcoMon_Strata$STRATA%in%strata_index),],aes(label = STRATA, x = X, y = Y),color="red")
ggsave(paste0(basepath,"/figures/ecomon_strata_mvco.png"),width=800,height=800,units="px",dpi=120)


load(paste0(basepath,"/data/ecomon_nutrients_2007_2023.RData"))
load(paste0(basepath,"/data/strata_nutrients.RData"))

str(dfj)

#YEARLY
dfj %>% filter((season=="Summer"),
               year >=2006,
               nitrite_nitrate!=-999,
               STRATA %in% strata_index) %>% 
  complete(year=2006:2022) %>%
  ggplot() +
  geom_point(aes(x=nitrite_nitrate,y=depth_sampling)) + 
  facet_grid(cols=vars(year))+
  scale_y_reverse()+
  xlab(expression("Nitrite and Nitrate (umol kg"^-1*")"))+ylab("Depth (m)")

ggsave(filename=paste0(basepath,"/figures/environmental/nutrients/nitrite_nitrate_profiles_yearly_",strata_name,".png"),width=1200,height=600,units="px",dpi=120)


dfj %>% filter((season=="Summer"),
               year >=2006,
               silicate!=-999,
               STRATA %in% strata_index) %>% 
  complete(year=2006:2022) %>%
  ggplot() +
  geom_point(aes(x=silicate,y=depth_sampling)) + 
  facet_grid(cols=vars(year))+
  scale_y_reverse()+
  xlab(expression("Silicate (umol kg"^-1*")"))+ylab("Depth (m)")

ggsave(filename=paste0(basepath,"/figures/environmental/nutrients/silicate_profiles_yearly_",strata_name,".png"),width=1200,height=600,units="px",dpi=120)


dfj %>% filter((season=="Summer"),
               year >=2006,
               phosphate!=-999,
               STRATA %in% strata_index) %>% 
  complete(year=2006:2022) %>%
  ggplot() +
  geom_point(aes(x=phosphate,y=depth_sampling)) + 
  facet_grid(cols=vars(year))+
  scale_y_reverse()+
  xlab(expression("Phosphate (umol kg"^-1*")"))+ylab("Depth (m)")

ggsave(filename=paste0(basepath,"/figures/environmental/nutrients/phosphate_profiles_yearly_",strata_name,".png"),width=1200,height=600,units="px",dpi=120)

