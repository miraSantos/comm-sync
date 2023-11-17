
list.of.packages <- c("readxl","fields","ggplot2","sf","broom","dplyr","lubridate","sp")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)
remotes::install_github("NEFSC/NEFSC-Spatial")
library(NEFSCspatial)

load(paste0(basepath,"/data/ecomon_nutrients_2007_2023.RData"))

#loading ecomon polygons
data("EcoMon_Strata")

pnts_sf <- st_as_sf(na.omit(df_nut), coords = c('lon', 'lat'), crs = st_crs(EcoMon_Strata),remove=F)
str(pnts_sf)
p <- st_make_valid(EcoMon_Strata)
dfj <- st_join(pnts_sf,p,left=T)

nes_url <- paste0(basepath,"/data/shapefile/lme.shp") #shape file from https://www.marineregions.org/gazetteer.php?p=details&id=8551
nes_shp<- read_sf(nes_url)

ggplot() + geom_sf(data=EcoMon_Strata)+
  geom_text(data=EcoMon_Strata,aes(label = STRATA, x = X, y = Y))
ggsave(paste0(basepath,"/figures/ecomon_strata_guide.png"),width=800,height=800,units="px",dpi=120)

ggplot() + geom_sf(data=EcoMon_Strata)+
geom_sf(data=EcoMon_Strata[which(EcoMon_Strata$STRATA%in%strata_index),],color="red")+
geom_text(data=EcoMon_Strata,aes(label = STRATA, x = X, y = Y))+
geom_text(data=EcoMon_Strata[which(EcoMon_Strata$STRATA%in%strata_index),],aes(label = STRATA, x = X, y = Y),color="red")
ggsave(paste0(basepath,"/figures/ecomon_strata_mvco.png"),width=800,height=800,units="px",dpi=120)


save(dfj,file=paste0(basepath,"/data/strata_nutrients.RData"))
