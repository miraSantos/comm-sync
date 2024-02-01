list.of.packages <- c("readxl","fields","ggplot2","sf","broom","dplyr","lubridate","sp","tidyr","scales")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)
remotes::install_github("NEFSC/NEFSC-Spatial")
library(NEFSCspatial)

load(paste0(basepath,"/data/ecomon_nutrients_2007_2023.RData"))
load(paste0(basepath,"/data/strata_nutrients.RData"))


dfj$year <- year(dfj$date)
regime_1_end = 2012
regime_2_end = 2018
regime_1_index = (which(dfj$year < regime_1_end))
regime_2_index = (which((dfj$year >= regime_1_end)&(dfj$year < regime_2_end)))
regime_3_index = (which(dfj$year >= regime_2_end))

dfj$regime <- NaN

dfj$regime[regime_1_index] = paste0("2006 - ",as.character(regime_1_end-1))
dfj$regime[regime_2_index] = paste0(as.character(regime_1_end)," - ",as.character(regime_2_end-1))
dfj$regime[regime_3_index] = paste0(as.character(regime_2_end)," - 2022")


metseasons <- c(
  "01" = "Winter", "02" = "Winter",
  "03" = "Spring", "04" = "Spring", "05" = "Spring",
  "06" = "Summer", "07" = "Summer", "08" = "Summer",
  "09" = "Fall", "10" = "Fall", "11" = "Fall",
  "12" = "Winter")

seasons = metseasons[format(dfj$date, "%m")]
dfj$season = seasons

str(dfj)

strata_index = mvco_strata

dfj %>% filter(year >=1900,
               nitrite_nitrate!=-999,
               STRATA %in% strata_index,depth_sampling>=30,
               depth_sampling<=40)  %>%
  select(season,year,STRATA,nitrite_nitrate,depth_sampling,date) %>%
  ggplot() + geom_point(aes(x=year,y=nitrite_nitrate)) + 
  scale_x_continuous(breaks=seq(1920,2020,10))+
  scale_y_continuous(breaks=seq(0,70,5))+
  ylab(expression("Nitrite and Nitrate (umol kg"^-1*")"))+xlab("Date")


########################################
dfj %>% filter(year >=1900,
               nitrite_nitrate!=-999,
               season=="Summer",
               STRATA %in% strata_index,
               depth_sampling>=20, depth_sampling<=30)  %>%
  complete(year=1933:2022) %>%
  ggplot() + geom_boxplot(aes(x=as.factor(year),y=nitrite_nitrate)) + 
  ylab(expression("Nitrite and Nitrate (umol kg"^-1*")"))+
  xlab("Year") +
  ylim(c(0,8)) +
  scale_x_discrete(breaks=seq(1930,2025,10))

ggsave(filename=paste0(basepath,"/figures/environmental/nutrients/nitrite_nitrate/summer_boxplot_nitrite_nitrate_20-30m_",strata_name,".png"),
       width=800,height=600,units="px",dpi=150)

dfj %>% filter(year >=1900,
               nitrite_nitrate!=-999,
               season=="Summer",
               STRATA %in% strata_index,
               depth_sampling>=30, depth_sampling<=40)  %>%
  complete(year=1933:2022) %>%
  ggplot() + geom_boxplot(aes(x=as.factor(year),y=nitrite_nitrate)) + 
  ylab(expression("Nitrite and Nitrate (umol kg"^-1*")"))+
  xlab("Year") +
  scale_x_discrete(breaks=seq(1930,2025,10))+
ylim(c(0,8))

ggsave(filename=paste0(basepath,"/figures/environmental/nutrients/nitrite_nitrate/summer_median_nitrite_nitrate_30-40m_",strata_name,".png"),
       width=800,height=600,units="px",dpi=150)

dfj %>% filter(year >=1900,
               nitrite_nitrate!=-999,
               season=="Summer",
               STRATA %in% strata_index,
               depth_sampling>=40, depth_sampling<=50)  %>%
  complete(year=1933:2022) %>%
  ggplot() + geom_boxplot(aes(x=as.factor(year),y=nitrite_nitrate)) + 
  ylab(expression("Nitrite and Nitrate (umol kg"^-1*")"))+
  xlab("Year") +
  scale_x_discrete(breaks=seq(1930,2025,10))+
  ylim(c(0,8))

ggsave(filename=paste0(basepath,"/figures/environmental/nutrients/nitrite_nitrate/summer_median_nitrite_nitrate_40-50m_",strata_name,".png"),
       width=800,height=600,units="px",dpi=150)



######################################################################################

dfj %>% filter(year >=1900,
               nitrite_nitrate!=-999,
               (season=="Winter" | season=="Spring"),
               STRATA %in% gom_basin,
               depth_sampling>=40, depth_sampling<=50)  %>%
  group_by(year)%>% 
  summarise(median_nit = median(nitrite_nitrate)) %>% ungroup() %>%
  ggplot() + geom_point(aes(x=year,y=median_nit)) + 
  ylab(expression("Nitrite and Nitrate (umol kg"^-1*")"))+
  xlab("Year") +
  scale_x_continuous(limits =c(1960,2025),breaks=seq(1930,2020,5))


dfj %>% filter(year >=1900,
               nitrite_nitrate!=-999,
               (season=="Winter" | season=="Spring"),
               STRATA %in% northeast_channel,
               depth_sampling>=40, depth_sampling<=50)  %>%
  group_by(year)%>% 
  summarise(median_nit = median(nitrite_nitrate)) %>% ungroup() %>%
  ggplot() + geom_point(aes(x=year,y=median_nit)) + 
  ylab(expression("Nitrite and Nitrate (umol kg"^-1*")"))+
  xlab("Year") +
  scale_x_continuous(limits =c(1960,2025),breaks=seq(1930,2020,5))

ggsave(filename=paste0(basepath,"/figures/environmental/nutrients/nitrite_nitrate/summer_median_nitrite_nitrate_40-50m_NORTHEAST_CHANNEL.png"),
       width=800,height=600,units="px",dpi=150)

dfj %>% filter(year >=1900,
               nitrite_nitrate!=-999,
               season=="Summer",
               STRATA %in% mvco_offshore,
               depth_sampling>=40, depth_sampling<=50)  %>%
  group_by(year)%>% 
  summarise(median_nit = median(nitrite_nitrate)) %>% ungroup() %>%
  ggplot() + geom_point(aes(x=year,y=median_nit)) + 
  ylab(expression("Nitrite and Nitrate (umol kg"^-1*")"))+
  xlab("Year") +
  scale_x_continuous(limits =c(1960,2025),breaks=seq(1930,2020,5))

ggsave(filename=paste0(basepath,"/figures/environmental/nutrients/nitrite_nitrate/summer_median_nitrite_nitrate_40-50m_MVCO_offshore.png"),
       width=800,height=600,units="px",dpi=150)

dfj %>% filter(year >=1900,
               nitrite_nitrate!=-999,
               (season=="Winter"| season=="Spring"),
               STRATA %in% gom_basin,
               nitrite_nitrate <=25,
               depth_sampling>=40, depth_sampling<=50)  %>%
  group_by(year)%>% 
  summarise(median_nit = median(nitrite_nitrate)) %>% ungroup() %>%
  ggplot() + geom_point(aes(x=year,y=median_nit)) + 
  ylab(expression("Nitrite and Nitrate (umol kg"^-1*")"))+
  xlab("Year") +
  scale_x_continuous(breaks=seq(1930,2020,10))

ggsave(filename=paste0(basepath,"/figures/environmental/nutrients/nitrite_nitrate/winter_spring_median_nitrite_nitrate_40-50m_gom_basin.png"),
       width=800,height=600,units="px",dpi=150)

#BOX PLOT DEPTH PROFILE YEARLY
dfj %>% filter((season=="Summer"),
               year >=1920,
               nitrite_nitrate!=-999,
               STRATA %in% strata_index)  %>%
  select(season,year,STRATA,nitrite_nitrate,depth_sampling) %>%
  na.omit()%>%
  mutate(depth_sampling_binned = factor(cut(depth_sampling, breaks = c(seq(0,60,10)),include.lowest=TRUE))) %>%
  group_by(year,depth_sampling_binned)%>% 
  summarise(median_nit = median(nitrite_nitrate)) %>% ungroup() %>%
ggplot(aes(x=year,y=median_nit,color=depth_sampling_binned,shape=depth_sampling_binned)) + geom_point()+
  ylab(expression("median Nitrite and Nitrate (umol kg"^-1*")"))+xlab("Year")+
  scale_x_continuous(breaks=seq(1930,2025,10))

