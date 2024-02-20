
list.of.packages <- c("readxl","fields","ggplot2","sf","broom","dplyr","lubridate","sp")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)
remotes::install_github("NEFSC/NEFSC-Spatial")
library(NEFSCspatial)

load(paste0(basepath,"/data/ecomon_nutrients_2007_2023.RData"))

#loading ecomon polygons
data("EcoMon_Strata")

df_nut<- df_nut %>% filter(!is.na(df_nut$lat))
pnts_sf <- st_as_sf(df_nut, coords = c('lon', 'lat'), crs = st_crs(EcoMon_Strata),remove=F)
str(pnts_sf)
p <- st_make_valid(EcoMon_Strata)
dfj <- st_join(pnts_sf,p,left=T)

nes_url <- paste0(basepath,"/data/shapefile/lme.shp") #shape file from https://www.marineregions.org/gazetteer.php?p=details&id=8551
nes_shp<- read_sf(nes_url)

ggplot() + geom_sf(data=EcoMon_Strata)+
  geom_text(data=EcoMon_Strata,aes(label = STRATA, x = X, y = Y))
ggsave(paste0(basepath,"/figures/ecomon_strata_maps/ecomon_strata_guide.png"),width=1200,height=1200,units="px",dpi=200)

ggplot() + geom_sf(data=EcoMon_Strata)+
geom_sf(data=EcoMon_Strata[which(EcoMon_Strata$STRATA%in%strata_index),],color="red")+
geom_text(data=EcoMon_Strata,aes(label = STRATA, x = X, y = Y))+
geom_text(data=EcoMon_Strata[which(EcoMon_Strata$STRATA%in%strata_index),],
          aes(label = STRATA, x = X, y = Y),color="red")+
  xlab("Longitude") + ylab("Latitude")

ggsave(paste0(basepath,"/figures/ecomon_strata_mvco.png"),width=800,height=800,units="px",dpi=120)


dfj$year <- year(dfj$date)
regime_1_start = 2006
regime_1_end = 2012
regime_2_end = 2018
regime_1_index = (which(dfj$year < regime_1_end)&(dfj$year>=regime_1_start))
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

save(dfj,file=paste0(basepath,"/data/strata_nutrients.RData"))



