
list.of.packages <- c("lubridate","dplyr","ggplot2","tidyverse")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

# penobscot bay
pen_bay <- "http://www.neracoos.org/erddap/tabledap/F01_sbe37_all.csv?station%2Ctime%2Cmooring_site_desc%2Cconductivity%2Cconductivity_qc%2Ctemperature%2Ctemperature_qc%2Csalinity%2Csalinity_qc%2Csigma_t%2Csigma_t_qc%2Clongitude%2Clatitude%2Cdepth&time%3E=2006-01-01T00%3A00%3A00Z&time%3C=2023-07-24T21%3A30%3A00Z"

#jordan basin station M01 (2006 - 2022)
jor_bas <- "http://www.neracoos.org/erddap/tabledap/M01_sbe37_all.csv?station%2Ctime%2Cmooring_site_desc%2Cconductivity%2Cconductivity_qc%2Ctemperature%2Ctemperature_qc%2Csalinity%2Csalinity_qc%2Csigma_t%2Csigma_t_qc%2Clongitude%2Clatitude%2Cdepth&time%3E=2006-07-17T00%3A00%3A00Z&time%3C=2023-07-24T17%3A00%3A00Z"

#western maine shelf station B01 (2006- 2022)
bo1_buoy <- "http://www.neracoos.org/erddap/tabledap/B01_sbe37_all.csv?station%2Ctime%2Cmooring_site_desc%2Cconductivity%2Cconductivity_qc%2Ctemperature%2Ctemperature_qc%2Csalinity%2Csalinity_qc%2Csigma_t%2Csigma_t_qc%2Clongitude%2Clatitude%2Cdepth&time%3E=2006-01-01T00%3A00%3A00Z&time%3C=2023-07-24T20%3A00%3A00Z"

#MA bay station A01 (2006-2022)
ma_bay <- "http://www.neracoos.org/erddap/tabledap/A01_sbe37_all.csv?station%2Ctime%2Cmooring_site_desc%2Cconductivity%2Cconductivity_qc%2Ctemperature%2Ctemperature_qc%2Csalinity%2Csalinity_qc%2Csigma_t%2Csigma_t_qc%2Clongitude%2Clatitude%2Cdepth&time%3E=2006-01-01T00%3A00%3A00Z&time%3C=2023-07-24T20%3A30%3A00Z"

#Northeast channel N01 
nor_chan <- "http://www.neracoos.org/erddap/tabledap/N01_sbe37_all.csv?station%2Ctime%2Cmooring_site_desc%2Cconductivity%2Cconductivity_qc%2Ctemperature%2Ctemperature_qc%2Csalinity%2Csalinity_qc%2Csigma_t%2Csigma_t_qc%2Clongitude%2Clatitude%2Cdepth&time%3E=2006-01-01T00%3A00%3A00Z&time%3C=2021-11-04T21%3A00%3A00Z"
#WMG 

ma_bay_chl <- "http://www.neracoos.org/erddap/tabledap/A01_optics_s_all.csv?station%2Cmooring_site_desc%2Ctime%2Cchlorophyll%2Cchlorophyll_qc%2Cturbidity%2Cturbidity_qc%2Clongitude%2Clatitude%2Cdepth&time%3E=2003-01-01T00%3A00%3A00Z&time%3C=2023-09-08T17%3A00%3A00Z"
wmg_chl<-"http://www.neracoos.org/erddap/tabledap/B01_optics_hist.csv?station%2Ctime%2Cmooring_site_desc%2Cwater_depth%2Csolar_zenith_angle%2Csolar_zenith_angle_qc%2CEd_PAR%2CEd_PAR_qc%2Cchlorophyll%2Cchlorophyll_qc%2Clongitude%2Clatitude%2Cdepth&time%3E=2005-01-01T00%3A00%3A00Z&time%3C=2023-09-23T19%3A00%3A00Z"
##################### EXTRA NERACOOS DATASETS
#gulf of maine central (2019-2021)
gom <-"http://www.neracoos.org/erddap/tabledap/E07_met_all.csv?station%2Cmooring_site_desc%2Cwater_depth%2Ctime%2Cair_temperature%2Cair_temperature_qc%2Cwind_gust%2Cwind_gust_qc%2Cwind_min%2Cwind_min_qc%2Cwind_speed%2Cwind_speed_qc%2Cwind_gust_1s%2Cwind_gust_1s_qc%2Cwind_direction%2Cwind_direction_qc%2Cwind_percent_good%2Cwind_percent_good_qc%2Ctime_created%2Ctime_modified%2Clongitude%2Clatitude%2Cdepth&time%3E=2006-01-01T00%3A00%3A00Z&time%3C=2021-11-05T10%3A00%3A00Z"
#gulf of maine station E01 (2021-2022)
gom_sal <- "http://www.neracoos.org/erddap/tabledap/E01_sbe37_all.csv?station%2Ctime%2Cmooring_site_desc%2Cconductivity%2Cconductivity_qc%2Ctemperature%2Ctemperature_qc%2Csalinity%2Csalinity_qc%2Csigma_t%2Csigma_t_qc%2Clongitude%2Clatitude%2Cdepth&time%3E=2006-01-01T00%3A00%3A00Z&time%3C=2023-07-24T20%3A00%3A00Z"
# MA Bay station A01 (2014-2017)
ma_bay2 <- "http://www.neracoos.org/erddap/tabledap/A01_sbe16_trans_all.csv?station%2Ctime%2Cmooring_site_desc%2Cwater_depth%2Ctransmissivity_voltage%2Ctransmissivity_voltage_qc%2Ctransmissivity%2Ctransmissivity_qc%2Cattenuation%2Cattenuation_qc%2Cconductivity%2Cconductivity_qc%2Ctemperature%2Ctemperature_qc%2Csalinity%2Csalinity_qc%2Csigma_t%2Csigma_t_qc%2Ctime_created%2Ctime_modified%2Clongitude%2Clatitude%2Cdepth&time%3E=2006-01-01T00%3A00%3A00Z&time%3C=2021-01-30T14%3A00%3A00Z"

# station e01 2015 - 2018
nitrate_gom <- "http://www.neracoos.org/erddap/tabledap/E01_corrected_nitrate_csv.csv?station%2Ctime%2CYear%2CMonth%2CDay%2CHour%2CMinute%2CSecond%2CNitrate_umol%2CStandard_Deviation%2CNitrate%2Clatitude%2Clongitude%2Cmooring_site_desc%2Cdepth%2Cwater_depth&time%3E=2006-11-19T00%3A00%3A00Z&time%3C=2022-11-26T12%3A45%3A00Z"


df_nit<- read.csv(url(nitrate_gom))
write.csv(df_nit,"/home/mira/MIT-WHOI/Week.2023.10.15-21/data_to_share/nutrients/central_maine_shelf_nitrate.csv")
basepath = "/home/mira/MIT-WHOI/github_repos/comm-sync/"
dfsal_headers <- read.csv(url(pen_bay))[1,]
write.csv(dfsal_headers,file=paste0(basepath,"/data/neracoos_salinity_headers.csv"))
dfsal <- read.csv(url(pen_bay))[-1,] 
dfsal_units <- read.csv(url(pen_bay))[1,]
dfsal <-dfsal%>% filter(salinity_qc == 0,temperature_qc==0,conductivity_qc==0)

head(dfsal)

nrow(dfsal %>% filter(salinity_qc == 0))
nrow(dfsal %>% filter(salinity_qc == 0,temperature_qc==0))
nrow(dfsal %>% filter(salinity_qc == 0,temperature_qc==0,conductivity_qc==0))


write.csv(dfsal,file="/home/mira/MIT-WHOI/Week.2023.10.15-21/data_to_share/penobscot_bay_salinity_temperature.csv")


dfsal <- read.csv(url(jor_bas))[-1,] 
dfsal_units <- read.csv(url(pen_bay))[1,]
dfsal <-dfsal%>% filter(salinity_qc == 0,temperature_qc==0,conductivity_qc==0)

head(dfsal)

nrow(dfsal %>% filter(salinity_qc == 0))
nrow(dfsal %>% filter(salinity_qc == 0,temperature_qc==0))
nrow(dfsal %>% filter(salinity_qc == 0,temperature_qc==0,conductivity_qc==0))


write.csv(dfsal,file="/home/mira/MIT-WHOI/Week.2023.10.15-21/data_to_share/jordan_basin_salinity_temperature.csv")



dfsal <- read.csv(url(bo1_buoy))[-1,] 
dfsal_units <- read.csv(url(bo1_buoy))[1,]
dfsal <-dfsal%>% filter(salinity_qc == 0,temperature_qc==0,conductivity_qc==0)

head(dfsal)

nrow(dfsal %>% filter(salinity_qc == 0))
nrow(dfsal %>% filter(salinity_qc == 0,temperature_qc==0))
nrow(dfsal %>% filter(salinity_qc == 0,temperature_qc==0,conductivity_qc==0))


write.csv(dfsal,file="/home/mira/MIT-WHOI/Week.2023.10.15-21/data_to_share/western_maine_shelf_salinity_temperature.csv")



dfsal <- read.csv(url(ma_bay))[-1,]
dfsal_units <- read.csv(url(ma_bay))[1,]
dfsal <-dfsal%>% filter(salinity_qc == 0,temperature_qc==0,conductivity_qc==0)

head(dfsal)

nrow(dfsal %>% filter(salinity_qc == 0))
nrow(dfsal %>% filter(salinity_qc == 0,temperature_qc==0))
nrow(dfsal %>% filter(salinity_qc == 0,temperature_qc==0,conductivity_qc==0))


write.csv(dfsal,file="/home/mira/MIT-WHOI/Week.2023.10.15-21/data_to_share/ma_bay_salinity_temperature.csv")


dfsal <- read.csv(url(nor_chan))[-1,]
dfsal_units <- read.csv(url(nor_chan))[1,]
dfsal <-dfsal%>% filter(salinity_qc == 0,temperature_qc==0,conductivity_qc==0)

head(dfsal)


nrow(dfsal %>% filter(salinity_qc == 0))
nrow(dfsal %>% filter(salinity_qc == 0,temperature_qc==0))
nrow(dfsal %>% filter(salinity_qc == 0,temperature_qc==0,conductivity_qc==0))


write.csv(dfsal,file="/home/mira/MIT-WHOI/Week.2023.10.15-21/data_to_share/northeast_channel_salinity_temperature.csv")


dfsal <- read.csv(url(ma_bay_chl))[-1,]
dfsal_units <- read.csv(url(ma_bay_chl))[1,]
dfsal <-dfsal%>% filter(chlorophyll_qc == 0,turbidity_qc==0)

head(dfsal)

nrow(dfsal %>% filter(chlorophyll_qc == 0))
nrow(dfsal %>%  filter(chlorophyll_qc == 0,turbidity_qc==0))

dfsal$chlorophyll <- as.numeric(dfsal$chlorophyll)
dfsal$turbidity <- as.numeric(dfsal$turbidity)
dfsal$longitude <- as.numeric(dfsal$longitude)
dfsal$latitude <- as.numeric(dfsal$latitude)
dfsal$depth <- as.numeric(dfsal$depth)

dfsal$date <- as.Date(dfsal$time,format="%Y-%m-%dT%H:%M:%SZ")
head(dfsal)
dfsal%>% group_by(date) %>% summarise(mean_chl = mean(chlorophyll,na.rm=T),mean_turbidity=mean(turbidity,na.rm=T),
                                  longitude=mean(longitude,na.rm=T),latitude=mean(latitude),depth=mean(depth),
                                  mooring_site = mooring_site_desc,station=station)

write.csv(dfsal,file="/home/mira/MIT-WHOI/Week.2023.10.15-21/data_to_share/ma_bay_chl_turbidity.csv")


dfsal <- read.csv(url(wmg_chl))[-1,]
dfsal_units <- read.csv(url(wmg_chl))[1,]
dfsal <-dfsal%>% filter(chlorophyll_qc == 0)

head(dfsal)

nrow(dfsal %>% filter(chlorophyll_qc == 0))
nrow(dfsal %>%  filter(chlorophyll_qc == 0))

dfsal$chlorophyll <- as.numeric(dfsal$chlorophyll)
dfsal$longitude <- as.numeric(dfsal$longitude)
dfsal$latitude <- as.numeric(dfsal$latitude)
dfsal$depth <- as.numeric(dfsal$depth)

dfsal$date <- as.Date(dfsal$time,format="%Y-%m-%dT%H:%M:%SZ")
head(dfsal)
dfsal%>% group_by(date) %>% summarise(mean_chl = mean(chlorophyll,na.rm=T),
                                      longitude=mean(longitude,na.rm=T),latitude=mean(latitude),depth=mean(depth),
                                      mooring_site = mooring_site_desc,station=station)

write.csv(dfsal,file="/home/mira/MIT-WHOI/Week.2023.10.15-21/data_to_share/western_maine_shelf_chl.csv")

