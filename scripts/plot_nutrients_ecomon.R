
list.of.packages <- c("readxl","fields","ggplot2","sf","broom","dplyr","lubridate")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)


load(paste0(basepath,"/data/ecomon_nutrients_2007_2023.RData"))

df_nut %>% filter(nutrients_flag == 2,lat >40) %>% ggplot() +
  geom_sf(data=nes_shp) +
  geom_point(aes(x=lon,y=lat,color=nitrite_nitrate))+facet_grid(cols=vars(year))+
  scale_color_gradient(low="yellow",high="red")

ggsave(filename=paste0(basepath,"/figures/environmental/nutrients/ecomon_nitrite_nitrate_yearly.png"),width=3000,height=500,units="px",dpi=100)


df_nut %>% filter(silicate!=-999,nutrients_flag == 2,lat >40,season=="Summer") %>% ggplot() +
  geom_sf(data=nes_shp) +
  geom_point(aes(x=lon,y=lat,color=nitrite_nitrate))+facet_grid(cols=vars(year))+
  scale_color_gradient(low="yellow",high="red")

ggsave(filename=paste0(basepath,"/figures/environmental/nutrients/summer_ecomon_nitrite_nitrate_yearly.png"),width=3000,height=500,units="px",dpi=100)


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




