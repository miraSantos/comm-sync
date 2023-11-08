
list.of.packages <- c("readxl","fields","ggplot2","sf","broom","dplyr","lubridate")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

load(paste0(basepath,"/data/ecomon_nutrients_2007_2023.RData"))

nes_url <- paste0(basepath,"/data/shapefile/lme.shp") #shape file from https://www.marineregions.org/gazetteer.php?p=details&id=8551
nes_shp<- read_sf(nes_url)


  
df_nut %>% filter(nutrients_flag == 2,lat >40,lat <44,year==2012,((season == "Spring")| (season == "Summer"))) %>% ggplot() +
geom_sf(data=nes_shp) +
geom_point(aes(x=lon,y=lat,color=depth_sampling))+
ylim(40,44)+
xlim(-73,-66)+
scale_color_gradient(low="yellow",high="red")


df_nut %>% filter(nutrients_flag == 2,lat >40,lat <44,depth_sampling>30,((season == "Spring")| (season == "Summer"))) %>% ggplot() +
  geom_sf(data=nes_shp) +
  geom_point(aes(x=lon,y=lat,color=nitrite_nitrate))+
  ylim(40,44)+
  xlim(-73,-66)+
  facet_grid(cols=vars(year))+
  scale_color_gradient(low="yellow",high="red")

ggsave(filename=paste0(basepath,"/figures/environmental/nutrients/ecomon_nitrite_nitrate_yearly.png"),width=3000,height=500,units="px",dpi=100)


df_nut %>% filter(silicate != -999,nutrients_flag == 2,lat >40,lat <44,depth_sampling>30,((season == "Spring")| (season == "Summer"))) %>% ggplot() +
  geom_sf(data=nes_shp) +
  geom_point(aes(x=lon,y=lat,color=silicate))+
  ylim(40,44)+
  xlim(-73,-66)+
  facet_grid(cols=vars(year))+
  scale_color_gradient(low="yellow",high="red")

ggsave(filename=paste0(basepath,"/figures/environmental/nutrients/ecomon_silicate_yearly.png"),width=3000,height=500,units="px",dpi=100)


df_nut %>% filter(silicate != -999,nutrients_flag == 2,
                  lat >41,lat <42.5,lon> -71,lon < -69,
                  depth_sampling>30,
                  ((season == "Spring")| (season == "Summer"))) %>% ggplot() +
  geom_sf(data=nes_shp) +
  geom_point(aes(x=lon,y=lat,color=silicate))+
  ylim(40,44)+
  xlim(-73,-66)+
  facet_grid(cols=vars(year))+
  scale_color_gradient(low="yellow",high="red")

ggsave(filename=paste0(basepath,"/figures/environmental/nutrients/ecomon_silicate_yearly_mvco.png"),width=3000,height=500,units="px",dpi=100)

df_nut %>% filter(ammonia != -999,nutrients_flag == 2,
                  lat >41,lat <42.5,lon> -71,lon < -69,
                  depth_sampling>30,
                  ((season == "Spring")| (season == "Summer"))) %>% ggplot() +
  geom_sf(data=nes_shp) +
  geom_point(aes(x=lon,y=lat,color=ammonia))+
  ylim(40,44)+
  xlim(-73,-66)+
  facet_grid(cols=vars(year))+
  scale_color_gradient(low="yellow",high="red")

ggsave(filename=paste0(basepath,"/figures/environmental/nutrients/ecomon_ammonia_yearly_mvco.png"),width=3000,height=500,units="px",dpi=100)


df_nut %>% filter(phosphate != -999,nutrients_flag == 2,
                  lat >40,lat <44,lon> -73,lon < -66,
                  depth_sampling>30,
                  ((season == "Spring")| (season == "Summer"))) %>% ggplot() +
  geom_sf(data=nes_shp) +
  geom_point(aes(x=lon,y=lat,color=phosphate))+
  ylim(40,44)+
  xlim(-73,-66)+
  facet_grid(cols=vars(year))+
  scale_color_gradient(low="yellow",high="red")

ggsave(filename=paste0(basepath,"/figures/environmental/nutrients/ecomon_phosphate_yearly.png"),width=3000,height=500,units="px",dpi=100)


df_nut %>% filter(nitrite_nitrate != -999,nutrients_flag == 2,
                  lat >40,lat <44,lon> -73,lon < -66,
                  depth_sampling>30,
                  ((season == "Spring")| (season == "Summer"))) %>% ggplot() +
  geom_sf(data=nes_shp) +
  geom_point(aes(x=lon,y=lat,color=nitrite_nitrate))+
  ylim(40,44)+
  xlim(-73,-66)+
  facet_grid(cols=vars(year))+
  scale_color_gradient(low="yellow",high="red")

ggsave(filename=paste0(basepath,"/figures/environmental/nutrients/ecomon_nitrite_nitrate_yearly.png"),width=3000,height=500,units="px",dpi=100)



df_nut %>% filter(silicate != -999,nutrients_flag == 2,
                  lat >41,lat <42.5,lon> -71,lon < -69,
                  depth_sampling>40,
                   (season == "Summer")) %>% ggplot() +
  geom_point(aes(x=date,y=silicate))


df_nut %>% filter(nitrite_nitrate != -999,nutrients_flag == 2,
                  lat >41,lat <42.5,lon> -71,lon < -69,
                  depth_sampling>40) %>% ggplot() +
  geom_point(aes(x=date,y=nitrite_nitrate))+
  xlab("Time")+ylab("Nitrite+Nitrate (umol/kg)")+
  scale_x_date(date_breaks="1 year",date_labels = "%Y")
ggsave(filename=paste0(basepath,"/figures/environmental/nutrients/ecomon_nitrite_nitrate_mvco_timeseries.png"),width=800,height=300,units="px",dpi=120)


df_nut %>% filter(ammonia!= -999,nutrients_flag == 2,
                  lat >41,lat <42.5,lon> -71,lon < -69,
                  depth_sampling>40) %>% ggplot() +
  geom_point(aes(x=date,y=ammonia))+
  xlab("Time")+ylab("Ammonia (umol/kg)")+
  scale_x_date(date_breaks="1 year",date_labels = "%Y")
ggsave(filename=paste0(basepath,"/figures/environmental/nutrients/ecomon_ammonia_mvco_timeseries.png"),width=800,height=300,units="px",dpi=120)


df_nut %>% filter(phosphate!= -999,nutrients_flag == 2,
                  lat >41,lat <42.5,lon> -71,lon < -69,
                  depth_sampling>=30) %>% ggplot() +
  geom_point(aes(x=date,y=phosphate))+
  xlab("Time")+ylab("Phosphate (umol/kg)")+
  scale_x_date(date_breaks="1 year",date_labels = "%Y")
ggsave(filename=paste0(basepath,"/figures/environmental/nutrients/ecomon_phosphate_mvco_timeseries.png"),width=800,height=300,units="px",dpi=120)


obs_yr <- df_nut %>% filter(nitrite_nitrate != -999,nutrients_flag == 2,
                  lat >41,lat <42.5,lon> -71,lon < -69,
                  depth_sampling>40) %>% group_by(year) %>% summarise(n_obs = length(date))

ggplot(obs_yr) + geom_point(aes(x=year,y=n_obs)) +
  scale_x_continuous(breaks=seq(2010,2022,1))

df_nut %>% filter(nitrite_nitrite!=-999,nutrients_flag == 2,lat >40) %>% ggplot() +
  geom_sf(data=nes_shp) +
  geom_point(aes(x=lon,y=lat,color=nitrite_nitrate))+facet_grid(cols=vars(year))+
  scale_color_gradient(low="yellow",high="red")

ggsave(filename=paste0(basepath,"/figures/environmental/nutrients/summer_ecomon_nitrite_nitrate_yearly.png"),width=3000,height=500,units="px",dpi=100)


df_nut %>% filter(silicate!=-999,nutrients_flag == 2,lat >40,season=="Summer",depth_sampling > 20) %>% ggplot() +
  geom_sf(data=nes_shp)+
  geom_point(aes(x=lon,y=lat,color=silicate))+facet_grid(cols=vars(year))+
  ylim(40,44)+
  xlim(-73,-66)+
  scale_color_gradient2(low="red",mid="blue",high="purple",midpoint=15)

df_nut %>% filter(silicate!=-999,nutrients_flag == 2,lat >40,season=="Summer") %>% ggplot() +
  geom_point(aes(x=date,y=depth_sampling))


ggsave(filename=paste0(basepath,"/figures/environmental/nutrients/summer_ecomon_silicate_yearly.png"),width=3000,height=500,units="px",dpi=100)

df_nut %>% ggplot() + geom_point(aes(x=date,y=nitrite_nitrate))+
  scale_x_date(date_breaks="1 year",date_labels = "%Y")



ggplot(data=df_nut) + geom_point(aes(x=date,y=depth_bottom),color="black",shape="x",size=3)+
  geom_point(aes(x=date,y=depth_sampling),color="red",alpha=0.5)+
  scale_x_date(date_breaks="1 year",date_labels="%Y")+
  scale_y_continuous(breaks=seq(0,5000,500))
  

