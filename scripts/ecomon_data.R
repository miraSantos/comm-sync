list.of.packages <- c("ggplot2","remotes","knitr","rmarkdown",
                      "readxl","sf","lubridate")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
remotes::install_github("noaa-edab/ecodata",build_vignettes=TRUE)
lapply(list.of.packages, require, character.only = TRUE)

############### EXPLORING ecomon salinity data

load("C:\\Users\\Miraflor P Santos\\comm-sync\\ecomon_data\\seasonal_oisst_anom.rda")

ggplot(data= seasonal_oisst_anom[(seasonal_oisst_anom$Var=="Summer")&
                                   (seasonal_oisst_anom$EPU=="MAB"),]) +
  geom_point(aes(x = Time, y = Value))+
  ggtitle("Summer SST Anomaly")



dfeco<-read_excel("C:\\Users\\Miraflor P Santos\\comm-sync\\ecomon_data\\EcoMon_Plankton_Data_v3_7_dnd.xlsx",
                  sheet = 3)

dfeco$date <- as.Date(dfeco$date,format="%Y-%m-%d UTC")
dfeco$year <- year(dfeco$date)
dfeco$doy_numeric <- yday(dfeco$date)
dfeco$regime <- NaN


metseasons <- c(
  "01" = "Winter", "02" = "Winter",
  "03" = "Spring", "04" = "Spring", "05" = "Spring",
  "06" = "Summer", "07" = "Summer", "08" = "Summer",
  "09" = "Fall", "10" = "Fall", "11" = "Fall",
  "12" = "Winter")

seasons = metseasons[format(dfeco$date, "%m")]
dfeco$season = seasons)


regime_1_end = 2012
regime_2_end = 2018
regime_1_index = (which(dfeco$year < regime_1_end))
regime_2_index = (which((dfeco$year >= regime_1_end)&(dfeco$year < regime_2_end)))
regime_3_index = (which(dfeco$year >= regime_2_end))

dfeco$regime[regime_1_index] = paste0("2006 - ",as.character(regime_1_end-1))
dfeco$regime[regime_2_index] = paste0(as.character(regime_1_end)," - ",as.character(regime_2_end-1))
dfeco$regime[regime_3_index] = paste0(as.character(regime_2_end)," - 2022")

summer_index <- which((dfeco$doy_numeric >= 121)&(dfeco$doy_numeric <= 213))

lat_min = 41
lat_max = 41.4
lon_min =-71.5
lon_max = -70

lat_min = 40
lat_max = 44
lon_min =-71.5
lon_max = -69.5

mvco_index= which((dfeco$lat >= lat_min)&
                        (dfeco$lat<=lat_max)&
                        (dfeco$lon>=lon_min)&
                        (dfeco$lon<=lon_max))

year_depth_index = which((dfeco$year >= 2006) & (dfeco$year <= 2022) &
                           (dfeco$depth <= 100))

mvco_recent <- intersect(mvco_index,year_depth_index)

mvco_summer <- intersect(intersect(summer_index,mvco_index),year_depth_index)


ggplot() +
  geom_point(data=dfeco[mvco_summer,],
             aes(x = lon,y=lat,color = sfc_salt))+
  scale_color_gradient(low="blue", high="red")


nes_shp <- read_sf("C:\\Users\\Miraflor P Santos\\comm-sync\\ecomon_data\\lme.shp")


nes_shp_cropped <- st_crop(nes_shp, xmin = lon_min, xmax = lon_max,
                           ymin = lat_min, ymax = lat_max)

#
ggplot() +
   geom_sf(data = nes_shp_cropped)+
  geom_point(data = na.omit(dfeco[mvco_recent,]),
             aes(x = lon,y=lat,color = sfc_salt))+
  facet_grid(cols=vars(year),rows=vars(factor(season,levels=c("Winter","Spring","Summer","Fall"))))+
  scale_color_gradientn(colours = rainbow(5))+
  theme_bw()


# SALINITY SUMMER MVCO AREA TIME SERIES
ggplot(data = dfeco[mvco_recent,]) +
  geom_boxplot(aes(x = as.factor(doy_numeric), y = sfc_salt))+
  scale_x_discrete(breaks = seq(0,366,20))+
  facet_grid(cols=vars(year))

