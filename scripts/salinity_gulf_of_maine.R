
list.of.packages <- c("lubridate","dplyr","ggplot2","ggformula")
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

ma_bay_chl <- "http://www.neracoos.org/erddap/tabledap/A01_optics_s_all.csv?station%2Cmooring_site_desc%2Ctime%2Cchlorophyll%2Cchlorophyll_qc%2Cturbidity%2Cturbidity_qc%2Clongitude%2Clatitude%2Cdepth&time%3E=2003-01-01T00%3A00%3A00Z&time%3C=2023-09-08T17%3A00%3A00Z"

#Northeast channel N01 
nor_chan <- "http://www.neracoos.org/erddap/tabledap/N01_sbe37_all.csv?station%2Ctime%2Cmooring_site_desc%2Cconductivity%2Cconductivity_qc%2Ctemperature%2Ctemperature_qc%2Csalinity%2Csalinity_qc%2Csigma_t%2Csigma_t_qc%2Clongitude%2Clatitude%2Cdepth&time%3E=2006-01-01T00%3A00%3A00Z&time%3C=2021-11-04T21%3A00%3A00Z"
#WMG 
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

dfsal_daily_mean = dfsal_daily_mean %>% group_by(regime,doy_numeric) %>% 
  mutate(median_salinity = median(salinity_daily_mean)) %>%
  ungroup()

metseasons <- c(
  "01" = "Winter", "02" = "Winter",
  "03" = "Spring", "04" = "Spring", "05" = "Spring",
  "06" = "Summer", "07" = "Summer", "08" = "Summer",
  "09" = "Fall", "10" = "Fall", "11" = "Fall",
  "12" = "Winter")

seasons = metseasons[format(dfsal_daily_mean$date, "%m")]
dfsal_daily_mean$season = seasons
dfsal_daily_mean$month = month(dfsal_daily_mean$date)



return(list("dfsal"=dfsal_daily_mean,
            "regime_1_index"=regime_1_index,
            "regime_2_index"=regime_2_index,
            "regime_3_index"=regime_3_index,
            "station"= dfsal$station[1]))
}

basepath = "/home/mira/MIT-WHOI/github_repos/comm-sync/"

wmg = load_data(bo1_buoy,depth = 50)

mbay = load_data(ma_bay,depth = 20)

jorb = load_data(jor_bas,depth = 1)

pbay = load_data(pen_bay,depth = 1)

norchan=load_data(nor_chan,depth = 9.96921e+36)

ggplot(data = norchan$dfsal[norchan$dfsal$season == "Summer",])+
  geom_boxplot(aes(x=week,y=salinity_daily_mean))+
  facet_grid(cols=vars(year))+
  scale_x_discrete(breaks=seq(23,36,5))+
  ylab("Salinity")+
  xlab("Week of Year")+
  ggtitle(paste0("Summer Salinity at "," Northeast Channel ",as.character(norchan$station)))

ggsave(filename=paste0(basepath,"figures/environmental/salinity/summer-salinity-norchan-N01.png"),
       width = 2000,height=700,units="px",dpi =175)


ggplot(data = norchan$dfsal[norchan$dfsal$season == "Spring",])+
  geom_boxplot(aes(x=week,y=salinity_daily_mean))+
  facet_grid(cols=vars(year))+
  scale_x_discrete(breaks=seq(23,36,5))+
  ylab("Salinity")+
  xlab("Week of Year")+
  ggtitle(paste0("Spring Salinity at "," Northeast Channel ",as.character(norchan$station)))

ggsave(filename=paste0(basepath,"figures/environmental/salinity/spring-salinity-norchan-N01.png"),
       width = 2000,height=700,units="px",dpi =175)

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


ggplot(data = wmg$dfsal[wmg$dfsal$season == "Spring",])+
  geom_boxplot(aes(x=week,y=salinity_daily_mean))+
  facet_grid(cols=vars(year))+
  scale_x_discrete(breaks=seq(23,36,5))+
  ylim(31,33)+
  ylab("Salinity")+
  xlab("Week of Year")+
  ggtitle(paste0("Spring Salinity at "," Western Maine Gulf ",as.character(wmg$station)))


ggplot(data = wmg$dfsal[wmg$dfsal$season == "Summer",])+
  geom_boxplot(aes(x=week,y=salinity_daily_mean))+
  facet_grid(cols=vars(year))+
  scale_x_discrete(breaks=seq(23,36,5))+
  ylim(31.3,32.7)+
  ylab("Salinity")+
  xlab("Week of Year")+
  ggtitle(paste0("Summer Salinity at "," Western Maine Gulf ",as.character(wmg$station)))

ggsave(filename=paste0(basepath,"figures/environmental/salinity/summer-salinity-WMG.png"),
       width = 2000,height=500,units="px",dpi =175)


ggplot(data = mbay$dfsal[mbay$dfsal$season == "Summer",])+
  geom_boxplot(aes(x=week,y=salinity_daily_mean))+
  facet_grid(cols=vars(year))+
  scale_x_discrete(breaks=seq(23,36,5))+
  ylab("Salinity")+
  xlab("Week of Year")+
  ggtitle(paste0("Summer Salinity at "," MA Bay ",as.character(mbay$station)))

f
ggsave(filename="C:\\Users\\Miraflor P Santos\\comm-sync\\figures\\environmental\\salinity\\summer-salinity-MA-bay.png",
       width = 2000,height=500,units="px",dpi =175)



ggplot()+
  geom_point(data = wmg$dfsal,aes(x = date,y=salinity_daily_mean),color=cbbPalette[1],size=0.8)+
  geom_point(data = ma_bay$dfsal,aes(x = date,y=salinity_daily_mean),color=cbbPalette[2],size=0.8,alpha=0.5)+
  geom_point(data = dfcarbon_group,aes(x = date,y=salinity_beam),color=cbbPalette[4],size=0.8,alpha=0.5)
  geom_point(data = ,aes(),color=cbbPalette[3],size=0.8,alpha=0.5)+
    

print(sal$station)

#plot daily mean of salinity time series
ggplot(data = sal$dfsal) +
  geom_point(aes(x = date,y = salinity_daily_mean))+
  scale_x_date(date_breaks="1 year", date_labels="%Y")+
  ggtitle(paste0("Salinity at ",as.character(sal$station)))


#plotting regime wise weekly salinity
ggplot(data=sal$dfsal,aes(x = week,y = salinity_daily_mean)) + 
  geom_boxplot(color="darkgreen")+ 
  facet_grid(cols=vars(regime))+
  xlab("Week of Year")+
  ylab("Salinity (psu)")+
  theme(text = element_text(size = 15))+
  scale_x_discrete(breaks=seq(1,52,5))+
  ggtitle(paste0("Salinity at ",as.character(sal$station)))

#plotting year wise montly salinity
ggplot(data=sal$dfsal,aes(x = as.factor(month),y = salinity_daily_mean)) + 
  geom_boxplot(color="darkgreen")+ 
  facet_grid(cols=vars(year),rows=vars(factor(season,levels=c("Winter","Spring","Summer","Fall"))))+
  xlab("Week of Year")+
  ylab("Salinity (psu)")+
  theme(text = element_text(size = 15))+
  scale_x_discrete(breaks=seq(1,52,5))+
  ggtitle(paste0("Salinity at ",as.character(sal$station)))


# Median salinity by day of year grouped by each regime (color) 
ggplot(data = sal$dfsal) +
  geom_spline(aes(x = doy_numeric,y = median_salinity,color=regime),size = 1)+
  scale_x_continuous(breaks=seq(0,366,30))+
  xlab("Day of Year")+
  ylab("Median Salinity (psu)")+
  ggtitle(paste0("Median Salinity at ",as.character(sal$station)))
  
ggsave(filename=paste0("figures\\environmental\\salinity\\",
              "median-salinity-regime-",as.character(sal$station),".png"),
       width=800,height=400,units="px",dpi=150)

# montly salinity by year and season
ggplot() +
  geom_boxplot(data = na.omit(sal$dfsal),
             aes(x = month,y=salinity_daily_mean))+
  facet_grid(cols=vars(year),rows=vars(factor(season,levels=c("Winter","Spring","Summer","Fall"))))+
  theme_bw()


############################
#MA BAY CHL STATION A01

df_chl <- read.csv(url(ma_bay_chl))[-1,]

rownames(df_chl) <- NULL

#create date columns
df_chl$date <- as.Date(df_chl$time,format="%Y-%m-%dT%H:%M:%SZ")
df_chl$year <- year(df_chl$date)
df_chl$doy_numeric <- yday(df_chl$date)
df_chl$week <- week(df_chl$date)
df_chl$month<- month(df_chl$date)
df_chl$depth = as.numeric(df_chl$depth)
df_chl$chlorophyll = as.numeric(df_chl$chlorophyll)

metseasons <- c(
  "01" = "Winter", "02" = "Winter",
  "03" = "Spring", "04" = "Spring", "05" = "Spring",
  "06" = "Summer", "07" = "Summer", "08" = "Summer",
  "09" = "Fall", "10" = "Fall", "11" = "Fall",
  "12" = "Winter")

seasons = metseasons[format(df_chl$date, "%m")]
df_chl$season = seasons
df_chl$season_numeric = NaN
df_chl$season_numeric[df_chl$season == "Winter"] = 1
df_chl$season_numeric[df_chl$season == "Spring"] = 2
df_chl$season_numeric[df_chl$season == "Summer"] = 3
df_chl$season_numeric[df_chl$season == "Fall"] = 4

flag_index = which((df_chl$chlorophyll_qc == 0)&(df_chl$season=="Summer"))

ggplot(data = df_chl[flag_index,]) + 
  geom_point(aes(x = week, y = chlorophyll^(1/3)))+
  facet_grid(cols=vars(year))+
  scale_x_discrete(breaks=seq(23,36,15))+
  ylab("Chl^(1/3) (ug/L)")+
  ggtitle("Summer Chlorophyll Concentrations at MA Bay (A01 Station)")
  
ggplot(data = df_chl[flag_index,]) + 
  geom_boxplot(aes(x = as.factor(month), y = chlorophyll^(1/3)))+
  facet_grid(cols=vars(year))+
  scale_x_discrete(breaks=seq(6,8,1))+
  ylab("Chl^(1/3) (ug/L)")+
  xlab("Month")+
  ggtitle("Summer Chlorophyll Concentrations at MA Bay (A01 Station)")

ggsave(filename="/home/mira/MIT-WHOI/github_repos/comm-sync/figures/environmental/summer_chl_MA_bay.png",
       width = 2000,height=800,units="px",dpi =175)


df_chl <- read.csv(url(wmg_chl))[-1,]

rownames(df_chl) <- NULL

#create date columns
df_chl$date <- as.Date(df_chl$time,format="%Y-%m-%dT%H:%M:%SZ")
df_chl$year <- year(df_chl$date)
df_chl$doy_numeric <- yday(df_chl$date)
df_chl$week <- week(df_chl$date)
df_chl$month<- month(df_chl$date)
df_chl$depth = as.numeric(df_chl$depth)
df_chl$chlorophyll = as.numeric(df_chl$chlorophyll)

metseasons <- c(
  "01" = "Winter", "02" = "Winter",
  "03" = "Spring", "04" = "Spring", "05" = "Spring",
  "06" = "Summer", "07" = "Summer", "08" = "Summer",
  "09" = "Fall", "10" = "Fall", "11" = "Fall",
  "12" = "Winter")

seasons = metseasons[format(df_chl$date, "%m")]
df_chl$season = seasons
df_chl$season_numeric = NaN
df_chl$season_numeric[df_chl$season == "Winter"] = 1
df_chl$season_numeric[df_chl$season == "Spring"] = 2
df_chl$season_numeric[df_chl$season == "Summer"] = 3
df_chl$season_numeric[df_chl$season == "Fall"] = 4

flag_index = which((df_chl$chlorophyll_qc == 0)&(df_chl$season=="Summer"))

ggplot(data = df_chl[flag_index,]) + 
  geom_point(aes(x = week, y = chlorophyll^(1/3)))+
  facet_grid(cols=vars(year))+
  scale_x_discrete(breaks=seq(23,36,15))+
  ylab("Chl^(1/3) (ug/L)")+
  ggtitle("Summer Chlorophyll Concentrations at Western Maine Gulf (B01 Station)")

ggplot(data = df_chl[flag_index,]) + 
  geom_boxplot(aes(x = as.factor(month), y = chlorophyll^(1/3)))+
  facet_grid(cols=vars(year))+
  scale_x_discrete(breaks=seq(6,8,1))+
  ylab("Chl^(1/3) (ug/L)")+
  xlab("Month")+
  ggtitle("Summer Chlorophyll Concentrations at Western Maine Gulf (B01 Station)")

ggsave(filename="/home/mira/MIT-WHOI/github_repos/comm-sync/figures/environmental/summer_chl_wmg.png",
       width = 2000,height=800,units="px",dpi =175)



#NORTHEAST CHANNEL

dfsal <- read.csv(url(norchan))[-1,]
#reset index
rownames(dfsal) <- NULL

#create date columns
dfsal$date <- as.Date(dfsal$time,format="%Y-%m-%dT%H:%M:%SZ")
dfsal$year <- year(dfsal$date)
dfsal$doy_numeric <- yday(dfsal$date)
dfsal$week <- week(dfsal$date)

dfsal$salinity = as.numeric(dfsal$salinity)
dfsal$depth = as.numeric(dfsal$depth)
