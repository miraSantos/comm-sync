
list.of.packages <- c("lubridate","dplyr","ggplot2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

#penobscot bay
pen_bay <- "http://www.neracoos.org/erddap/tabledap/F01_sbe37_all.csv?station%2Ctime%2Cmooring_site_desc%2Cconductivity%2Cconductivity_qc%2Ctemperature%2Ctemperature_qc%2Csalinity%2Csalinity_qc%2Csigma_t%2Csigma_t_qc%2Clongitude%2Clatitude%2Cdepth&time%3E=2006-01-01T00%3A00%3A00Z&time%3C=2023-07-24T21%3A30%3A00Z"

#jordan basin station M01 (2006 - 2022)
jor_bas <- "http://www.neracoos.org/erddap/tabledap/M01_sbe37_all.csv?station%2Ctime%2Cmooring_site_desc%2Cconductivity%2Cconductivity_qc%2Ctemperature%2Ctemperature_qc%2Csalinity%2Csalinity_qc%2Csigma_t%2Csigma_t_qc%2Clongitude%2Clatitude%2Cdepth&time%3E=2006-07-17T00%3A00%3A00Z&time%3C=2023-07-24T17%3A00%3A00Z"

#western maine shelf station B01 (2006- 2022)
bo1_buoy <- "http://www.neracoos.org/erddap/tabledap/B01_sbe37_all.csv?station%2Ctime%2Cmooring_site_desc%2Cconductivity%2Cconductivity_qc%2Ctemperature%2Ctemperature_qc%2Csalinity%2Csalinity_qc%2Csigma_t%2Csigma_t_qc%2Clongitude%2Clatitude%2Cdepth&time%3E=2006-01-01T00%3A00%3A00Z&time%3C=2023-07-24T20%3A00%3A00Z"

#MA bay station A01 (2006-2022)
ma_bay <- "http://www.neracoos.org/erddap/tabledap/A01_sbe37_all.csv?station%2Ctime%2Cmooring_site_desc%2Cconductivity%2Cconductivity_qc%2Ctemperature%2Ctemperature_qc%2Csalinity%2Csalinity_qc%2Csigma_t%2Csigma_t_qc%2Clongitude%2Clatitude%2Cdepth&time%3E=2006-01-01T00%3A00%3A00Z&time%3C=2023-07-24T20%3A30%3A00Z"

#gulf of maine central (2019-2021)
gom <-"http://www.neracoos.org/erddap/tabledap/E07_met_all.csv?station%2Cmooring_site_desc%2Cwater_depth%2Ctime%2Cair_temperature%2Cair_temperature_qc%2Cwind_gust%2Cwind_gust_qc%2Cwind_min%2Cwind_min_qc%2Cwind_speed%2Cwind_speed_qc%2Cwind_gust_1s%2Cwind_gust_1s_qc%2Cwind_direction%2Cwind_direction_qc%2Cwind_percent_good%2Cwind_percent_good_qc%2Ctime_created%2Ctime_modified%2Clongitude%2Clatitude%2Cdepth&time%3E=2006-01-01T00%3A00%3A00Z&time%3C=2021-11-05T10%3A00%3A00Z"

#gulf of maine station E01 (2021-2022)
gom_sal <- "http://www.neracoos.org/erddap/tabledap/E01_sbe37_all.csv?station%2Ctime%2Cmooring_site_desc%2Cconductivity%2Cconductivity_qc%2Ctemperature%2Ctemperature_qc%2Csalinity%2Csalinity_qc%2Csigma_t%2Csigma_t_qc%2Clongitude%2Clatitude%2Cdepth&time%3E=2006-01-01T00%3A00%3A00Z&time%3C=2023-07-24T20%3A00%3A00Z"
# MA Bay station A01 (2014-2017)
ma_bay2 <- "http://www.neracoos.org/erddap/tabledap/A01_sbe16_trans_all.csv?station%2Ctime%2Cmooring_site_desc%2Cwater_depth%2Ctransmissivity_voltage%2Ctransmissivity_voltage_qc%2Ctransmissivity%2Ctransmissivity_qc%2Cattenuation%2Cattenuation_qc%2Cconductivity%2Cconductivity_qc%2Ctemperature%2Ctemperature_qc%2Csalinity%2Csalinity_qc%2Csigma_t%2Csigma_t_qc%2Ctime_created%2Ctime_modified%2Clongitude%2Clatitude%2Cdepth&time%3E=2006-01-01T00%3A00%3A00Z&time%3C=2021-01-30T14%3A00%3A00Z"

# station e01 2015 - 2018
nitrate_gom <- "http://www.neracoos.org/erddap/tabledap/E01_corrected_nitrate_csv.csv?station%2Ctime%2CYear%2CMonth%2CDay%2CHour%2CMinute%2CSecond%2CNitrate_umol%2CStandard_Deviation%2CNitrate%2Clatitude%2Clongitude%2Cmooring_site_desc%2Cdepth%2Cwater_depth&time%3E=2006-11-19T00%3A00%3A00Z&time%3C=2022-11-26T12%3A45%3A00Z"


#remove first row bc it describes data units

load_data <- function(dataset,depth){
dfsal <- read.csv(url(dataset))[-1,]
#reset index
rownames(dfsal) <- NULL

#create date columns
dfsal$date <- as.Date(dfsal$time,format="%Y-%m-%dT%H:%M:%SZ")
dfsal$year <- year(dfsal$date)
dfsal$doy_numeric <- yday(dfsal$date)
dfsal$week <- week(dfsal$date)

dfsal$salinity = as.numeric(dfsal$salinity)
dfsal$depth = as.numeric(dfsal$depth)

# QUALITY CONTROL
dfsal = dfsal[dfsal$salinity_qc == 0,]

#compute daily mean salinity
dfsal_daily_mean = dfsal[dfsal$depth == depth,] %>% group_by(date)%>% 
  summarize(salinity_daily_mean = mean(salinity,na.rm=TRUE))%>% as.data.frame()

#create date columns again for new daily mean dataframe
dfsal_daily_mean$year <- year(dfsal_daily_mean$date)
dfsal_daily_mean$doy_numeric <- yday(dfsal_daily_mean$date)
dfsal_daily_mean$week <- factor(week(dfsal_daily_mean$date))
dfsal_daily_mean <- na.omit(dfsal_daily_mean)



regime_1_end = 2012
regime_2_end = 2018
regime_1_index = (which(dfsal_daily_mean$year < regime_1_end))
regime_2_index = (which((dfsal_daily_mean$year >= regime_1_end)&(dfsal_daily_mean$year < regime_2_end)))
regime_3_index = (which(dfsal_daily_mean$year >= regime_2_end))
dfsal_daily_mean$regime <- NaN

dfsal_daily_mean$regime[regime_1_index] = paste0("2006 - ",as.character(regime_1_end-1))
dfsal_daily_mean$regime[regime_2_index] = paste0(as.character(regime_1_end)," - ",as.character(regime_2_end-1))
dfsal_daily_mean$regime[regime_3_index] = paste0(as.character(regime_2_end)," - 2022")
return(list(dfsal_daily_mean,regime_1_index,regime_2_index,regime_3_index))
}

a = load_data(bo1_buoy,50)

#checking date range of data at specific depth
range(dfsal$depth)
range(dfsal[dfsal$depth==50,]$date)

#plot daily mean of salinity
ggplot(data = dfsal_daily_mean) +
  geom_point(aes(x = date,y = salinity_daily_mean))+
  scale_x_date(date_breaks="1 year", date_labels="%Y")


ggplot(data=dfsal_daily_mean,aes(x = week,y = salinity_daily_mean)) + 
  geom_boxplot(color="darkgreen")+ 
  facet_grid(cols=vars(regime))+
  xlab("Week of Year")+
  ylab("Salinity (psu)")+
  theme(text = element_text(size = 15))+
  scale_x_discrete(breaks=seq(1,52,5))

ggplot(data = dfsal_daily_mean[regime_1_index,]) +
  geom_point(aes(x = date,y = salinity_daily_mean))+
  scale_x_date(date_breaks="1 year", date_labels="%Y")



