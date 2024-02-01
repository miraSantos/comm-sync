
list.of.packages <- c("readxl","fields","ggplot2","sf","broom","dplyr","lubridate","sp","tidyr","scales","formula.tools")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)
remotes::install_github("NEFSC/NEFSC-Spatial")
library(NEFSCspatial)

load(paste0(basepath,"/data/ecomon_nutrients_2007_2023.RData"))
load(paste0(basepath,"/data/strata_nutrients.RData"))

str(dfj)
#PLOTTING NUTRIENT PROFILES
mvco_strata = c(21,24,25,20)
gom_basin = c(41,42,37,34)
northeast_channel = c(38,39)
strata_index = mvco_strata
strata_name = "MVCO"

##################################################################
# MVCO REGION
##################################################################
#######DATA POINTS
#1. Plot the number of data points per year for this region over time for
#nitrate nitrate data
dfj %>% select(date,year,nitrite_nitrate,STRATA,season,nitrite_nitrate_flag) %>% 
  filter(nitrite_nitrate>=-990,STRATA %in% strata_index) %>%
  count(year) %>% 
  ggplot() + geom_point(aes(x=year,y=n)) + ylab("Number of Data Points")+xlab("Year")

ggsave(filename=paste0(basepath,"/figures/environmental/nutrients/number_data_points_nitrate_",strata_name,".png"),width=600,height=500,units="px",dpi=120)
#2. Plot the number of data points per year for this region over time for
#phosphate data
dfj %>% select(date,year,STRATA,season,phosphate) %>%
  filter(phosphate>=-990,STRATA %in% strata_index) %>%
  count(year) %>%
  ggplot() + geom_point(aes(x=year,y=n)) + ylab("Number of Data Points")+
  ggtitle("Number of Data Points per Year for Phosphate")

#REGIME ORGANIZED PROFILES

dfj %>% filter((season=="Summer"),
               year >=2005,
               nitrite_nitrate!=-999,
               STRATA %in% strata_index)  %>% ggplot() +
  geom_point(aes(x=nitrite_nitrate,y=depth_sampling)) + facet_grid(cols=vars(regime))+
  scale_y_reverse()+
  xlab(expression("Nitrite and Nitrate (umol kg"^-1*")"))+ylab("Depth (m)")

ggsave(filename=paste0(basepath,"/figures/environmental/nutrients/nitrite_nitrate/nitrite_nitrate_profiles_summer_regime_",strata_name,".png"),width=800,height=600,units="px",dpi=120)


dfj %>% filter((season=="Summer"),
               year >=2005,
               nitrite_nitrate!=-999,
               STRATA %in% strata_index)  %>% ggplot() +
  geom_point(aes(x=nitrite_nitrate,y=depth_sampling)) + facet_grid(cols=vars(regime))+
  scale_y_reverse()+
  xlab(expression("Nitrite and Nitrate (umol kg"^-1*")"))+ylab("Depth (m)")

ggsave(filename=paste0(basepath,"/figures/environmental/nutrients/nitrite_nitrate/nitrite_nitrate_profiles_summer_regime_",strata_name,".png"),width=800,height=600,units="px",dpi=120)


dfj %>% filter((season=="Summer"),
               year >=2005,
               silicate!=-999,
               silicate_flag==2,
               STRATA %in% strata_index)  %>% ggplot() +
  geom_point(aes(x=silicate,y=depth_sampling)) + facet_grid(cols=vars(regime))+
  scale_y_reverse()+
  xlab(expression("Silicate (umol kg"^-1*")"))+ylab("Depth (m)")

ggsave(filename=paste0(basepath,"/figures/environmental/nutrients/silicate_profiles_summer_regime_",strata_name,".png"),width=800,height=600,units="px",dpi=120)

dfj %>% filter((season=="Summer"),
               year >=2005,
               phosphate!=-999,phosphate<=7.5,
               STRATA %in% strata_index)  %>% ggplot() +
  geom_point(aes(x=phosphate,y=depth_sampling)) + facet_grid(cols=vars(regime))+
  scale_y_reverse()+
  xlab(expression("Phosphate (umol kg"^-1*")"))+ylab("Depth (m)")

ggsave(filename=paste0(basepath,"/figures/environmental/nutrients/phosphate_profiles_summer_regime_",strata_name,".png"),width=800,height=600,units="px",dpi=120)


################YEAR TO YEAR
dfj %>% filter(nitrite_nitrate>-900,nitrite_nitrate_flag==2)  %>% ggplot() +
  geom_point(aes(x=date,y=nitrite_nitrate))+scale_x_date(date_breaks = "5 year",date_label="%Y")

dfj %>% filter(silicate!=-999,silicate_flag==2)  %>% ggplot() +
  geom_point(aes(x=date,y=silicate))+scale_x_date(date_breaks = "1 year",date_label="%Y")

dfj %>% filter(phosphate!=-999,phosphate_flag==2)  %>% ggplot() +
  geom_point(aes(x=date,y=phosphate))+scale_x_date(date_breaks = "1 year",date_label="%Y")


dfj %>% filter(nitrite_nitrate!=-999,nitrite_nitrate_flag==2,STRATA %in% strata_index)  %>% ggplot() +
  geom_point(aes(x=date,y=nitrite_nitrate))+scale_x_date(date_breaks = "1 year",date_label="%Y")

dfj %>% filter(silicate!=-999,silicate_flag==2,STRATA %in% strata_index)  %>% ggplot() +
  geom_point(aes(x=date,y=silicate))+scale_x_date(date_breaks = "1 year",date_label="%Y")

dfj %>% filter(phosphate!=-999,phosphate_flag==2,STRATA %in% strata_index)  %>% ggplot() +
  geom_point(aes(x=date,y=phosphate))+scale_x_date(date_breaks = "1 year",date_label="%Y")

dfj %>% filter(year>=2006,nitrite_nitrate!=-999,nitrite_nitrate_flag==2,STRATA %in% strata_index)  %>% ggplot() +
  geom_point(aes(x=nitrite_nitrate,y=depth_sampling)) + facet_grid(cols=vars(year))+
  scale_y_reverse()+
  xlab(expression("Nitrite and Nitrate (umol kg"^-1*")"))+ylab("Depth (m)")

dfj %>% filter(year>=2000,nitrite_nitrate!=-999,STRATA %in% strata_index)  %>% ggplot() +
  geom_point(aes(x=nitrite_nitrate,y=depth_sampling)) + facet_grid(cols=vars(year))+
  scale_y_reverse()+
  xlab(expression("Nitrite and Nitrate (umol kg"^-1*")"))+ylab("Depth (m)")


dfj %>% filter(season=="Summer",year >=2006,silicate!=-999,STRATA %in% strata_index)  %>% ggplot() +
  geom_point(aes(x=silicate,y=depth_sampling)) + facet_grid(cols=vars(year))+
  scale_y_reverse()+
  xlab(expression("Silicate (umol kg"^-1*")"))+ylab("Depth (m)")


dfj %>% filter(season=="Summer",year >=2006,silicate!=-999,STRATA %in% strata_index)  %>% ggplot() +
  geom_point(aes(x=silicate,y=depth_sampling)) + facet_grid(cols=vars(year))+
  scale_y_reverse()+
  xlab(expression("Silicate (umol kg"^-1*")"))+ylab("Depth (m)")


###############################################################################
#JORDAN AND WILKINSON BASIN
###############################################################################
gom_basin = c(41,42,37,34)
strata_index = gom_basin


#### NUMBER OF DATA PPOINTS
dfj %>% select(date,year,nitrite_nitrate,STRATA,season,nitrite_nitrate_flag) %>% 
  filter(nitrite_nitrate>=-990,STRATA %in% strata_index) %>%
  count(year) %>% 
  ggplot() + geom_point(aes(x=year,y=n)) + ylab("Number of Data Points")

dfj %>% select(date,year,nitrite_nitrate,STRATA,season,nitrite_nitrate_flag) %>% 
  filter(nitrite_nitrate>=-990,STRATA %in% strata_index,season=="Winter") %>%
  count(year) %>% 
  ggplot() + geom_point(aes(x=year,y=n)) + ylab("Number of Data Points")

############### MAPPED DATA
dfj %>% filter(nitrite_nitrate!=-999,STRATA %in% strata_index)  %>% ggplot() +
  geom_sf(data=EcoMon_Strata$geometry[strata_index])+
  geom_point(aes(x=lon,y=lat,color=nitrite_nitrate))+
  geom_point(aes(x=-70.5226,y=41.3620),color="red",shape=8)

dfj %>% filter(season=="Summer",nitrite_nitrate!=-999,STRATA %in% strata_index)  %>% ggplot() +
  geom_sf(data=EcoMon_Strata$geometry[strata_index])+
  geom_point(aes(x=lon,y=lat,color=nitrite_nitrate))+
  geom_point(aes(x=-70.5226,y=41.3620),color="red",shape=8)
dfj %>% filter(season=="Spring",nitrite_nitrate!=-999,STRATA %in% strata_index)  %>% ggplot() +
  geom_sf(data=EcoMon_Strata$geometry[strata_index])+
  geom_point(aes(x=lon,y=lat,color=nitrite_nitrate))+
  geom_point(aes(x=-70.5226,y=41.3620),color="red",shape=8)

#####NUTRIENT PROFILES
dfj %>% 
  filter((season=="Summer"),
         year >=2006,
         nitrite_nitrate!=-999,
         STRATA %in% gom_basin) %>%
  select(season,year,nitrite_nitrate,STRATA,depth_sampling,regime)%>%
  na.omit()%>%
  mutate(depth_sampling_binned = cut(depth_sampling, breaks = c(seq(0,400,10))),include.lowest=TRUE) %>% 
ggplot() + geom_boxplot(aes(y=depth_sampling_binned,x=nitrite_nitrate))+
  scale_y_discrete(limits=rev)+
  facet_grid(cols=vars(regime))+
  xlab(expression("Nitrite and Nitrate (umol kg"^-1*")"))+ylab("Depth (m)")

ggsave(filename=paste0(basepath,"/figures/environmental/nutrients/gom_basin/nitrate_regime_GOM_BASIN_summer_profiles.png"),width=600,height=500,units="px",dpi=120)

dfj %>% 
  filter((season=="Winter" | season =="Spring"),
         year >=2006,
         nitrite_nitrate!=-999,
         STRATA %in% strata_index) %>%
  select(season,year,nitrite_nitrate,STRATA,depth_sampling,regime)%>%
  na.omit()%>%
  mutate(depth_sampling_binned = cut(depth_sampling, breaks = c(seq(0,400,10))),include.lowest=TRUE) %>% 
  ggplot() + geom_boxplot(aes(y=depth_sampling_binned,x=nitrite_nitrate))+
  scale_y_discrete(limits=rev)+
  facet_grid(cols=vars(regime))+
  xlab(expression("Nitrite and Nitrate (umol kg"^-1*")"))+ylab("Depth (m)")

ggsave(filename=paste0(basepath,"/figures/environmental/nutrients/gom_basin/nitrate_yearly_colored_boxplots_winter_spring_GOM_BASIN.png"),width=600,height=500,units="px",dpi=120)

dfj %>% 
  filter((season=="Winter" | season =="Spring"),
         year >=2006,
         silicate!=-999,
         STRATA %in% strata_index) %>%
  mutate(depth_sampling_binned = cut(depth_sampling, breaks = c(seq(0,400,10)),include.lowest = TRUE)) %>% 
  ggplot() + geom_boxplot(aes(y=depth_sampling_binned,x=silicate))+
  scale_y_discrete(limits=rev)+
  facet_grid(cols=vars(regime))+
  xlab(expression("Silicate (umol kg"^-1*")"))+ylab("Depth (m)")

ggsave(filename=paste0(basepath,"/figures/environmental/nutrients/gom_basin/silicate_yearly_colored_boxplots_GOM_BASIN.png"),width=600,height=500,units="px",dpi=120)

dfj %>% 
  filter((season=="Winter"| season=="Spring"),
         year >=2006,
         phosphate!=-999,
         phosphate <= 6,
         STRATA %in% strata_index) %>%
  mutate(depth_sampling_binned = cut(depth_sampling, breaks = c(seq(0,400,10)))) %>% 
  ggplot() + geom_boxplot(aes(y=depth_sampling_binned,x=phosphate))+
  scale_y_discrete(limits=rev)+
  facet_grid(cols=vars(regime))+
  xlab(expression("Phosphate (umol kg"^-1*")"))+ylab("Depth (m)")

ggsave(filename=paste0(basepath,"/figures/environmental/nutrients/gom_basin/phosphate_yearly_colored_boxplots_GOM_BASIN.png"),width=600,height=500,units="px",dpi=120)


dfj %>% 
filter((season=="Winter"|season=="Spring"),
               nitrite_nitrate!=-999,
               STRATA %in% strata_index)  %>%
  mutate(depth_sampling_binned = cut(depth_sampling, breaks = c(seq(0,400,10)))) %>% 
  ggplot() +
  geom_boxplot(aes(x=nitrite_nitrate,y=depth_sampling_binned)) + 
  facet_grid(cols=vars(regime))+
  scale_y_discrete(limits=rev)+
  xlab(expression("Nitrite and Nitrate (umol kg"^-1*")"))+ylab("Depth (m)")
############# colored box plot year


dfj %>% filter((season=="Summer"),
               year >=1920,
               phosphate!=-999,
               phosphate <=4,
               STRATA %in% strata_index)  %>%
  select(season,year,STRATA,phosphate,depth_sampling) %>%
  na.omit()%>%
  mutate(depth_sampling_binned = factor(cut(depth_sampling, breaks = c(seq(0,400,20)),include.lowest=TRUE)),
         year_binned = factor(cut(year,breaks=c(seq(1930,2030,10)),dig.lab=4,include.lowest=T))) %>% 
  ggplot() + geom_boxplot(aes(x=phosphate,y=depth_sampling_binned,color=year_binned)) + 
  scale_y_discrete(limits=rev)+
  scale_colour_brewer(name="Year range",palette = "Accent")+
  xlab(expression("Phosphate (umol kg"^-1*")"))+ylab("Depth (m)")


########Year by year
dfj %>% filter((season=="Summer"),
               year >=1980,
               nitrite_nitrate!=-999,
               STRATA %in% strata_index)  %>%
  mutate(depth_sampling_binned = cut(depth_sampling, breaks = c(seq(0,400,20)))) %>% 
  ggplot() + geom_boxplot(aes(x=nitrite_nitrate,y=depth_sampling_binned,color=as.factor(year))) + 
  scale_y_discrete(limits=rev)+
  xlab(expression("Nitrite and Nitrate (umol kg"^-1*")"))+ylab("Depth (m)")

dfj %>% filter((season=="Winter"|season=="Spring"),
               year >=2006,
               nitrite_nitrate!=-999,
               STRATA %in% strata_index)  %>% ggplot() +
  geom_point(aes(x=nitrite_nitrate,y=depth_sampling)) + 
  facet_grid(cols=vars(year))+
  scale_y_reverse()+
  xlab(expression("Nitrite and Nitrate (umol kg"^-1*")"))+ylab("Depth (m)")


########dfd
dfj %>%
  filter((season=="Winter" | season=="Spring"),
         year >=1920,
         silicate!=-999,
         silicate < 40,
         STRATA %in% strata_index)  %>%
  select(season,year,STRATA,silicate,depth_sampling) %>%
  na.omit()%>%
  mutate(depth_sampling_binned = factor(cut(depth_sampling, breaks = c(seq(0,400,20)),include.lowest=TRUE)),
         year_binned = factor(cut(year,breaks=c(seq(1930,2030,10)),dig.lab=4,include.lowest=TRUE))) %>% 
  ggplot() + geom_boxplot(aes(x=silicate,y=depth_sampling_binned,color=year_binned)) + 
  scale_y_discrete(limits=rev)+
  scale_colour_brewer(name="Year range",palette = "Dark2")+
  xlab(expression("Silicate (umol kg"^-1*")"))+ylab("Depth (m)")

ggsave(filename=paste0(basepath,"/figures/environmental/nutrients/gom_basin/silicate_profiles_yearly_boxplot_colored_GOM_BASIN.png"),width=800,height=600,units="px",dpi=150)

dfj %>%
  filter((season=="Winter" | season=="Spring"),
         year >=1920,
         nitrite_nitrate!=-999,
         nitrite_nitrate<=50,
         STRATA %in% strata_index)  %>%
  select(season,year,STRATA,nitrite_nitrate,depth_sampling) %>%
  na.omit()%>%
  mutate(depth_sampling_binned = factor(cut(depth_sampling, breaks = c(seq(0,400,20)),include.lowest=TRUE)),
         year_binned = factor(cut(year,breaks=c(seq(1930,2030,10)),dig.lab=4,include.lowest=TRUE))) %>% 
  ggplot() + geom_boxplot(aes(x=nitrite_nitrate,y=depth_sampling_binned,color=year_binned)) + 
  scale_y_discrete(limits=rev)+
  scale_colour_brewer(name="Year range",palette = "Dark2")+
  xlab(expression("Nitrite and Nitrate (umol kg"^-1*")"))+ylab("Depth (m)")

ggsave(filename=paste0(basepath,"/figures/environmental/nutrients/gom_basin/nitrate_profiles_yearly_boxplot_colored_GOM_BASIN.png"),width=800,height=600,units="px",dpi=150)


dfj %>%
  filter((season=="Winter" | season=="Spring"),
         year >=1920,
         phosphate!=-999,
         phosphate<=15,
         STRATA %in% strata_index)  %>%
  select(season,year,STRATA,phosphate,depth_sampling) %>%
  na.omit()%>%
  mutate(depth_sampling_binned = factor(cut(depth_sampling, breaks = c(seq(0,400,20)),include.lowest=TRUE)),
         year_binned = factor(cut(year,breaks=c(seq(1930,2030,10)),dig.lab=4,include.lowest=TRUE))) %>% 
  ggplot() + geom_boxplot(aes(x=phosphate,y=depth_sampling_binned,color=year_binned)) + 
  scale_y_discrete(limits=rev)+
  scale_colour_brewer(name="Year range",palette = "Dark2")+
  xlab(expression("Phosphate (umol kg"^-1*")"))+ylab("Depth (m)")

ggsave(filename=paste0(basepath,"/figures/environmental/nutrients/gom_basin/phospahte_profiles_yearly_boxplot_colored_GOM_BASIN.png"),width=800,height=600,units="px",dpi=150)

dfj %>%
  filter((season=="Winter" | season=="Spring"),
         year >=1920,
         phosphate!=-999,
         phosphate<=15,
         nitrite_nitrate!=-999,
         STRATA %in% strata_index)  %>%
  select(season,year,STRATA,nitrite_nitrate,phosphate,depth_sampling) %>%
  na.omit()%>%
  mutate(depth_sampling_binned = factor(cut(depth_sampling, breaks = c(seq(0,400,20)),include.lowest=TRUE)),
         year_binned = factor(cut(year,breaks=c(seq(1930,2030,10)),dig.lab=4,include.lowest=TRUE))) %>% 
  ggplot() + geom_boxplot(aes(x=nitrite_nitrate/phosphate,y=depth_sampling_binned,color=year_binned)) + 
  scale_y_discrete(limits=rev)+
  scale_colour_brewer(name="Year range",palette = "Dark2")+
  xlab(expression("N:P Ratio"))+ylab("Depth (m)")

ggsave(filename=paste0(basepath,"/figures/environmental/nutrients/gom_basin/phospahte_profiles_yearly_boxplot_colored_GOM_BASIN.png"),width=800,height=600,units="px",dpi=150)

##################################################################################
# GOM - COAST
#################################################################################
gom_coast = c(45,40,36,35)
strata_index = gom_coast


#### NUMBER OF DATA PPOINTS
dfj %>% select(date,year,nitrite_nitrate,STRATA,season,nitrite_nitrate_flag) %>% 
  filter(nitrite_nitrate>=-990,STRATA %in% strata_index) %>%
  count(year) %>% 
  ggplot() + geom_point(aes(x=year,y=n)) + ylab("Number of Data Points")

dfj %>% select(date,year,nitrite_nitrate,STRATA,season,nitrite_nitrate_flag) %>% 
  filter(nitrite_nitrate>=-990,STRATA %in% strata_index,season=="Winter") %>%
  count(year) %>% 
  ggplot() + geom_point(aes(x=year,y=n)) + ylab("Number of Data Points")

dfj %>% filter(nitrite_nitrate!=-999,STRATA %in% strata_index)  %>% ggplot() +
  geom_sf(data=EcoMon_Strata$geometry[strata_index])+
  geom_point(aes(x=lon,y=lat,color=nitrite_nitrate))+
  geom_point(aes(x=-70.5226,y=41.3620),color="red",shape=8)

dfj %>% filter(season=="Summer",nitrite_nitrate!=-999,STRATA %in% strata_index)  %>% ggplot() +
  geom_sf(data=EcoMon_Strata$geometry[strata_index])+
  geom_point(aes(x=lon,y=lat,color=nitrite_nitrate))+
  geom_point(aes(x=-70.5226,y=41.3620),color="red",shape=8)
dfj %>% filter(season=="Spring",nitrite_nitrate!=-999,STRATA %in% strata_index)  %>% ggplot() +
  geom_sf(data=EcoMon_Strata$geometry[strata_index])+
  geom_point(aes(x=lon,y=lat,color=nitrite_nitrate))+
  geom_point(aes(x=-70.5226,y=41.3620),color="red",shape=8)

# Summer
dfj %>% 
  filter((season=="Summer"),
         year >=2006,
         nitrite_nitrate!=-999,
         STRATA %in% strata_index) %>%
  mutate(depth_sampling_binned = cut(depth_sampling, breaks = c(seq(0,400,10)))) %>% 
  ggplot() + geom_boxplot(aes(y=depth_sampling_binned,x=nitrite_nitrate))+
  scale_y_discrete(limits=rev)+
  facet_grid(cols=vars(regime))+
  xlab(expression("Nitrite and Nitrate (umol kg"^-1*")"))+
  ylab("Sampling Depth Bin (m)")

#
dfj %>% 
  filter((season=="Winter"|season=="Spring"),
         year >=2006,
         nitrite_nitrate!=-999,
         STRATA %in% strata_index)  %>%
  mutate(depth_sampling_binned = cut(depth_sampling, breaks = c(seq(0,400,10)))) %>% 
  ggplot() +
  geom_boxplot(aes(x=nitrite_nitrate,y=depth_sampling_binned)) + 
  facet_grid(cols=vars(regime))+
  scale_y_discrete(limits=rev)+
  xlab(expression("Nitrite and Nitrate (umol kg"^-1*")"))+
  ylab("Sampling Depth Bin (m)")

dfj %>% 
  filter((season=="Winter"|season=="Spring"),
         year >=2006,
         silicate!=-999,
         STRATA %in% strata_index)  %>%
  mutate(depth_sampling_binned = cut(depth_sampling, breaks = c(seq(0,400,10)))) %>% 
  ggplot() +
  geom_boxplot(aes(x=silicate,y=depth_sampling_binned)) + 
  facet_grid(cols=vars(regime))+
  scale_y_discrete(limits=rev)+
  xlab(expression("Silicate (umol kg"^-1*")"))+
  ylab("Sampling Depth Bin (m)")

#Year by year
dfj %>% filter((season=="Winter"),
               year >=2006,
               nitrite_nitrate!=-999,
               STRATA %in% strata_index)  %>% ggplot() +
  geom_point(aes(x=nitrite_nitrate,y=depth_sampling)) + 
  facet_grid(cols=vars(year))+
  scale_y_reverse()+
  xlab(expression("Nitrite and Nitrate (umol kg"^-1*")"))+ylab("Depth (m)")


dfj %>% filter((season=="Spring"),
               year >=2006,
               nitrite_nitrate!=-999,
               STRATA %in% strata_index)  %>% ggplot() +
  geom_point(aes(x=nitrite_nitrate,y=depth_sampling)) + 
  facet_grid(cols=vars(year))+
  scale_y_reverse()+
  xlab(expression("Nitrite and Nitrate (umol kg"^-1*")"))+ylab("Depth (m)")





##############################################################################
# MVCO OFFSHORE
################################################################################
mvco_offshore = c(19,23)
strata_index = mvco_offshore


#### NUMBER OF DATA PPOINTS
dfj %>% select(date,year,nitrite_nitrate,STRATA,season,nitrite_nitrate_flag) %>% 
  filter(nitrite_nitrate>=-990,STRATA %in% strata_index) %>%
  count(year) %>% 
  ggplot() + geom_point(aes(x=year,y=n)) + ylab("Number of Data Points")

dfj %>% select(date,year,nitrite_nitrate,STRATA,season,nitrite_nitrate_flag) %>% 
  filter(nitrite_nitrate>=-990,STRATA %in% strata_index,season=="Winter") %>%
  count(year) %>% 
  ggplot() + geom_point(aes(x=year,y=n)) + ylab("Number of Data Points")

dfj %>% filter(nitrite_nitrate!=-999,STRATA %in% strata_index)  %>% ggplot() +
  geom_sf(data=EcoMon_Strata$geometry[strata_index])+
  geom_point(aes(x=lon,y=lat,color=nitrite_nitrate))+
  geom_point(aes(x=-70.5226,y=41.3620),color="red",shape=8)

dfj %>% filter(season=="Summer",nitrite_nitrate!=-999,STRATA %in% strata_index)  %>% ggplot() +
  geom_sf(data=EcoMon_Strata$geometry[strata_index])+
  geom_point(aes(x=lon,y=lat,color=nitrite_nitrate))+
  geom_point(aes(x=-70.5226,y=41.3620),color="red",shape=8)
dfj %>% filter(season=="Spring",nitrite_nitrate!=-999,STRATA %in% strata_index)  %>% ggplot() +
  geom_sf(data=EcoMon_Strata$geometry[strata_index])+
  geom_point(aes(x=lon,y=lat,color=nitrite_nitrate))+
  geom_point(aes(x=-70.5226,y=41.3620),color="red",shape=8)


dfj %>% 
  filter((season=="Summer"),
         year >=2005,
         nitrite_nitrate!=-999,
         STRATA %in% strata_index) %>%
  select(season,year,nitrite_nitrate,depth_sampling,STRATA,regime) %>%
  na.omit()%>%
  mutate(depth_sampling_binned = cut(depth_sampling, breaks = c(seq(0,400,10)),include.lowest = T)) %>% 
  ggplot() + geom_boxplot(aes(y=depth_sampling_binned,x=nitrite_nitrate))+
  scale_y_discrete(limits=rev)+
  facet_grid(cols=vars(regime))+
  xlab(expression("Nitrite and Nitrate (umol kg"^-1*")"))+ylab("Depth (m)")

ggsave(filename=paste0(basepath,"/figures/environmental/nutrients/nitrite_nitrate_profiles_regime_mvco_offshore.png"),width=800,height=600,units="px",dpi=120)



dfj %>% 
  filter((season=="Summer"),
         year >=2005,
         silicate!=-999,
         STRATA %in% strata_index) %>%
  select(season,year,silicate,depth_sampling,STRATA,regime) %>%
  na.omit()%>%
  mutate(depth_sampling_binned = cut(depth_sampling, breaks = c(seq(0,400,10)),include.lowest = T)) %>% 
  ggplot() + geom_boxplot(aes(y=depth_sampling_binned,x=silicate))+
  scale_y_discrete(limits=rev)+
  facet_grid(cols=vars(regime))+
  xlab(expression("Silicate (umol kg"^-1*")"))+ylab("Depth (m)")

ggsave(filename=paste0(basepath,"/figures/environmental/nutrients/silicate_profiles_regime_mvco_offshore.png"),width=800,height=600,units="px",dpi=120)


dfj %>% filter((season=="Summer"),
               year >=2006,
               nitrite_nitrate!=-999,
               STRATA %in% strata_index)  %>%
  complete(year=2006:2022) %>%
  ggplot() +
  geom_point(aes(x=nitrite_nitrate,y=depth_sampling)) + 
  facet_grid(cols=vars(year))+
  scale_y_reverse()+
  xlab(expression("Nitrite and Nitrate (umol kg"^-1*")"))+ylab("Depth (m)")

ggsave(filename=paste0(basepath,"/figures/environmental/nutrients/nitrite_nitrate_profiles_yearly_MVCO_offshore.png"),width=1200,height=600,units="px",dpi=120)

dfj %>% filter(season=="Summer",nitrite_nitrate!=-999,STRATA %in% strata_index)  %>% ggplot() +
  geom_sf(data=EcoMon_Strata$geometry[strata_index])+
  geom_point(aes(x=lon,y=lat,color=nitrite_nitrate))+
  geom_point(aes(x=-70.5226,y=41.3620),color="red",shape=8)

dfj %>% filter(season=="Summer",year >=2006,nitrite_nitrate!=-999,STRATA %in% strata_index)  %>% ggplot() +
  geom_point(aes(x=nitrite_nitrate,y=depth_sampling)) + facet_grid(cols=vars(regime))+
  scale_y_reverse()+
  xlab(expression("Nitrite and Nitrate (umol kg"^-1*")"))+ylab("Depth (m)")

ggsave(filename=paste0(basepath,"/figures/environmental/nutrients/nitrite_nitrate_profiles_regime_offshore.png"),width=800,height=600,units="px",dpi=120)

dfj %>% filter(season=="Summer",year >=2006,silicate!=-999,STRATA %in% strata_index)  %>% ggplot() +
  geom_point(aes(x=silicate,y=depth_sampling)) + facet_grid(cols=vars(regime))+
  scale_y_reverse()+
  xlab(expression("Silicate (umol kg"^-1*")"))+ylab("Depth (m)")

ggsave(filename=paste0(basepath,"/figures/environmental/nutrients/silicate_profiles_regime_offshore.png"),width=800,height=600,units="px",dpi=120)


dfj %>% filter(season=="Summer",year >=2006,phosphate!=-999,STRATA %in% strata_index)  %>% ggplot() +
  geom_point(aes(x=phosphate,y=depth_sampling)) + facet_grid(cols=vars(regime))+
  scale_y_reverse()+
  xlab(expression("Phosphate (umol kg"^-1*")"))+ylab("Depth (m)")

ggsave(filename=paste0(basepath,"/figures/environmental/nutrients/phosphate_profiles_regime_offshore.png"),width=800,height=600,units="px",dpi=120)


dfj %>% filter(season=="Summer",year >=2006,phosphate!=-999,STRATA %in% strata_index)  %>% ggplot() +
  geom_point(aes(x=nitrite_nitrate/phosphate,y=depth_sampling)) + facet_grid(cols=vars(regime))+
  geom_vline(xintercept=16,color="red")+
  scale_y_reverse()+
  xlab(expression("N:P ratio"))+ylab("Depth (m)")

ggsave(filename=paste0(basepath,"/figures/environmental/nutrients/np_profiles_regime_37_34_33.png"),width=800,height=600,units="px",dpi=120)


dfj %>% filter(season=="Summer",ammonia!=-999,STRATA %in% strata_index)  %>% ggplot() +
  geom_point(aes(x=ammonia,y=depth_sampling)) + facet_grid(cols=vars(regime))+
  scale_y_reverse()+
  xlab(expression("Ammonia (umol kg"^-1*")"))+ylab("Depth (m)")

ggsave(filename=paste0(basepath,"/figures/environmental/nutrients/ammonia_profiles_regime.png"),width=800,height=600,units="px",dpi=120)


dfj %>% filter(season=="Summer",year>=2006,nitrite_nitrate!=-999,STRATA %in% strata_index)  %>% ggplot() +
  geom_point(aes(x=nitrite_nitrate,y=depth_sampling)) + facet_grid(cols=vars(year))+
  scale_y_reverse()+
  xlab(expression("Nitrite and Nitrate (umol kg"^-1*")"))+ylab("Depth (m)")


dfj %>% filter(season=="Summer",year >=2006,silicate!=-999,STRATA %in% strata_index)  %>% ggplot() +
  geom_point(aes(x=silicate,y=depth_sampling)) + facet_grid(cols=vars(year))+
  scale_y_reverse()+
  xlab(expression("Silicate (umol kg"^-1*")"))+ylab("Depth (m)")


dfj %>% filter(season=="Summer",year >=2006,silicate!=-999,STRATA %in% strata_index)  %>% ggplot() +
  geom_point(aes(x=silicate,y=depth_sampling)) + facet_grid(cols=vars(year))+
  scale_y_reverse()+
  xlab(expression("Silicate (umol kg"^-1*")"))+ylab("Depth (m)")


#PLOTTING NUTRIENT PROFILES
strata_index = c(41,42,37)
season_i = "Summer"
dfj %>% filter(season==season_i,nitrite_nitrate!=-999,STRATA %in% strata_index)  %>% ggplot() +
  geom_sf(data=EcoMon_Strata$geometry[strata_index])+
  geom_point(aes(x=lon,y=lat,color=nitrite_nitrate))+
  geom_point(aes(x=-70.5226,y=41.3620),color="red",shape=8)

dfj %>% filter(season==season_i,year >=2006,nitrite_nitrate!=-999,STRATA %in% strata_index)  %>% ggplot() +
  geom_point(aes(x=nitrite_nitrate,y=depth_sampling)) + facet_grid(cols=vars(regime))+
  scale_y_reverse()+
  xlab(expression("Nitrite and Nitrate (umol kg"^-1*")"))+ylab("Depth (m)")

ggsave(filename=paste0(basepath,"/figures/environmental/nutrients/nitrite_nitrate_profiles_regime_offshore.png"),width=800,height=600,units="px",dpi=120)

dfj %>% filter(season==season_i,year >=2006,silicate!=-999,STRATA %in% strata_index)%>%
  ggplot()+geom_point(aes(x=silicate,y=depth_sampling)) +
  facet_grid(cols=vars(regime))+
  scale_y_reverse()+
  xlab(expression("Silicate (umol kg"^-1*")"))+
  ylab("Depth (m)")

ggsave(filename=paste0(basepath,"/figures/environmental/nutrients/silicate_profiles_regime_offshore.png"),width=800,height=600,units="px",dpi=120)



dfj %>% filter(season==season_i,year >=2006,phosphate!=-999,
               STRATA %in% strata_index) %>% ggplot() +
  geom_point(aes(x=phosphate,y=depth_sampling)) +
  facet_grid(cols=vars(year))+
  scale_y_reverse()+
  xlab(expression("Phosphate (umol kg"^-1*")"))+ylab("Depth (m)")

ggsave(filename=paste0(basepath,"/figures/environmental/nutrients/phosphate_profiles_regime_offshore.png"),width=800,height=600,units="px",dpi=120)


dfj %>% filter(season==season_i,year >=2006,
               STRATA %in% strata_index)%>%
  ggplot() +
  geom_point(aes(x=nitrite_nitrate/phosphate,y=depth_sampling)) +
  facet_grid(cols=vars(regime))+
  geom_vline(xintercept=16,color="red")+
  scale_y_reverse()+
  xlab(expression("N:P ratio"))+ylab("Depth (m)")

ggsave(filename=paste0(basepath,"/figures/environmental/nutrients/np_profiles_regime_37_34_33.png"),width=800,height=600,units="px",dpi=120)



dfj %>% filter(season=="Summer",ammonia!=-999,STRATA %in% strata_index)  %>% ggplot() +
  geom_point(aes(x=ammonia,y=depth_sampling)) + facet_grid(cols=vars(regime))+
  scale_y_reverse()+
  xlab(expression("Ammonia (umol kg"^-1*")"))+ylab("Depth (m)")

ggsave(filename=paste0(basepath,"/figures/environmental/nutrients/ammonia_profiles_regime.png"),width=800,height=600,units="px",dpi=120)


#############################################################################
#NORTHEAST CHANNEL
#############################################################################
#REGIME ORGANIZED PROFILES
northeast_channel = c(38,39)
strata_index = northeast_channel

dfj %>% 
  filter((season=="Winter"|season=="Spring"),
         year >=2005,
         nitrite_nitrate!=-999,
         STRATA %in% strata_index) %>%
  select(season,year,nitrite_nitrate,depth_sampling,STRATA,regime) %>%
  na.omit()%>%
  mutate(depth_sampling_binned = cut(depth_sampling, breaks = c(seq(0,400,10)),include.lowest = T)) %>% 
  ggplot() + geom_boxplot(aes(y=depth_sampling_binned,x=nitrite_nitrate))+
  scale_y_discrete(limits=rev)+
  facet_grid(cols=vars(regime))+
  xlab(expression("Nitrite and Nitrate (umol kg"^-1*")"))+ylab("Depth (m)")


dfj %>% filter((season=="Summer"),
               year >=2005,
               nitrite_nitrate!=-999,
               STRATA %in% strata_index)  %>% ggplot() +
  geom_point(aes(x=nitrite_nitrate,y=depth_sampling)) + facet_grid(cols=vars(regime))+
  scale_y_reverse()+
  xlab(expression("Nitrite and Nitrate (umol kg"^-1*")"))+ylab("Depth (m)")

ggsave(filename=paste0(basepath,"/figures/environmental/nutrients/nitrite_nitrate_profiles_summer_regime.png"),width=800,height=600,units="px",dpi=120)


dfj %>% filter((season=="Summer"),
               year >=2005,
               silicate!=-999,
               STRATA %in% strata_index)  %>% ggplot() +
  geom_point(aes(x=silicate,y=depth_sampling)) + facet_grid(cols=vars(regime))+
  scale_y_reverse()+
  xlab(expression("Silicate (umol kg"^-1*")"))+ylab("Depth (m)")

ggsave(filename=paste0(basepath,"/figures/environmental/nutrients/silicate_profiles_summer_regime.png"),width=800,height=600,units="px",dpi=120)

dfj %>% filter((season=="Summer"),
               year >=2005,
               phosphate!=-999,phosphate<=7.5,
               STRATA %in% strata_index)  %>% ggplot() +
  geom_point(aes(x=phosphate,y=depth_sampling)) + facet_grid(cols=vars(regime))+
  scale_y_reverse()+
  xlab(expression("Phosphate (umol kg"^-1*")"))+ylab("Depth (m)")

ggsave(filename=paste0(basepath,"/figures/environmental/nutrients/phosphate_profiles_summer_regime.png"),width=800,height=600,units="px",dpi=120)



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

ggsave(filename=paste0(basepath,"/figures/environmental/nutrients/nitrite_nitrate_profiles_yearly.png"),width=1200,height=600,units="px",dpi=120)
