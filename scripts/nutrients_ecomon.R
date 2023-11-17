
list.of.packages <- c("readxl","fields","ggplot2","sf","broom","dplyr","lubridate")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

#data are very bad and don't have nutrients (will exclude)
df_nut_2006_1 <- read.csv(paste0(basepath,"/data/ecomon_data/nutrients/2006/sko612.csv"))
head(df_nut_2006_1)

df_nut_2007 <- read_excel(paste0(basepath,"/data/ecomon_data/nutrients/2007/E01_GOMECC1R.xlsx"))
df_nut_2007$date <- as.Date(with(df_nut_2007,paste(Year,Month,Day,sep="-")),"%Y-%m-%d")

df_nut_2007$nitrite_nitrate <- df_nut_2007$`NITRAT_UMOL/KG` + df_nut_2007$`NITRIT_UMOL/KG`
df_nut_2007 <- df_nut_2007 %>% rename("lat" = "LATITUDE_DEC_DEG",
                                      "lon" = "LONGITUDE_DEC_DEG",
                                      "depth_bottom"="DEPTH_bottom_METERS",         
                                      "temp"="CTDTMP_ITS-90_DEG_C",
                                      "salinity"="SALNTY",  
                                      "salinity_flag"="SALNTY_FLAG_W",
                                      "oxy"="CTDOXY_corrected_UMOL/KG",
                                      "silicate"="SILCAT_UMOL/KG",
                                      "ammonia"="NH4_UMOL/KG",
                                      "phosphate"="PHSPHT_UMOL/KG",
                                      "nutrients_flag" = "SILCAT_FLAG_W") %>%
  select(date,lat,lon,depth_bottom,temp,salinity,salinity_flag,oxy,silicate,nitrite_nitrate,ammonia,phosphate,nutrients_flag)
df_nut_2007$depth_sampling = NaN


df_nut_2009$salinity_flag = 2
df_nut_2009$nutrients_flag = 2
df_nut_2009 <- read_excel(paste0(basepath,"/data/ecomon_data/nutrients/2009/NEFSC_nutrient.xlsx"))
df_nut_2009$date <- as.Date(with(df_nut_2009,paste(Year,Month,Day,sep="-")),"%Y-%m-%d")

df_nut_2009 <- df_nut_2009 %>% rename("lat" = "Latitude_DEC_DEG",
                                      "lon" = "Longitude_DEC_DEG",
                                      "depth_bottom"="Depth_bottom_METERS",         
                                      "depth_sampling" ="Depth_sampling_METERS",
                                      "temp"="CTDTMP_ITS-90_DEG_C",
                                      "salinity"="CTDSAL_PSS-78",  
                                      "oxy"="CTDOXY_UMOL/KG",
                                      "silicate"="SILCAT_micromoles/Kilogram",
                                      "nitrite_nitrate"="NITRIT+NITRAT_micromoles/kilogam",
                                      "ammonia"="AMMONIA_micromoles/kilogram",
                                      "phosphate"="PHSPHT_micromoles/kilogram") %>%
  select(date,lat,lon,depth_bottom,depth_sampling,temp,salinity,oxy,silicate,nitrite_nitrate,ammonia,phosphate)
df_nut_2009$salinity_flag = 2
df_nut_2009$nutrients_flag = 2


df_nut_2012_1 <- read.csv(paste0(basepath,"/data/ecomon_data/nutrients/2012/334B20121026-PC1207_data.csv"))
df_nut_2012_2 <- read.csv(paste0(basepath,"/data/ecomon_data/nutrients/2012/316G20120202-DE1202_data.csv"))
df_nut_2012_3 <- read.csv(paste0(basepath,"/data/ecomon_data/nutrients/2012/33HH20120531-HB1202_data.csv"))

df_nut_2012 <- rbind(df_nut_2012_1,df_nut_2012_2,df_nut_2012_3)
df_nut_2012$date<-as.Date(with(df_nut_2012,paste(Year_UTC,Month_UTC,Day_UTC,sep="-")),"%Y-%m-%d")

str(df_nut_2012)
df_nut_2012 <- df_nut_2012 %>% rename("lat" = "Latitude_Dec_Deg",
                                      "lon" = "Longitude_Dec_Deg",
                                      "depth_bottom"="Depth_Bottom_meters",         
                                      "depth_sampling" ="Depth_meters",
                                      "temp"="CTDTEMP_ITS90",
                                      "salinity"="Salinity_PSS78", 
                                      "salinity_flag" ="Salinity_Flag",
                                      "oxy"="CTDOXY_umol.kg",
                                      "silicate"="Silicate_umol.kg",
                                      "nitrite_nitrate"="Nitrite_and_Nitrate_umol.kg",
                                      "ammonia"="Ammonium_umol.kg",
                                      "phosphate"="Phosphate_umol.kg",
                                      "nutrients_flag"="Nutrients_Flag")%>%
  select(date,lat,lon,depth_bottom,depth_sampling,temp,salinity,salinity_flag,oxy,silicate,nitrite_nitrate,ammonia,phosphate,nutrients_flag)





df_nut_2013_1 <- read.csv(paste0(basepath,"data/ecomon_data/nutrients/2013/33GG20130609-GU1302_data.csv"))
df_nut_2013_2 <- read.csv(paste0(basepath,"data/ecomon_data/nutrients/2013/33GG20131113-GU1305_data.csv"))
df_nut_2013 <- rbind(df_nut_2013_1,df_nut_2013_2)
df_nut_2013$date<-as.Date(with(df_nut_2013,paste(Year_UTC,Month_UTC,Day_UTC,sep="-")),"%Y-%m-%d")
df_nut_2013 <- df_nut_2013 %>% rename("lat" = "Latitude_Dec_Deg",
                                      "lon" = "Longitude_Dec_Deg",
                                      "depth_bottom"="Depth_Bottom_meters",         
                                      "depth_sampling" ="Depth_meters",
                                      "temp"="CTDTEMP_ITS90",
                                      "salinity"="Salinity_PSS78", 
                                      "salinity_flag" ="Salinity_Flag",
                                      "oxy"="CTDOXY_umol.kg",
                                      "silicate"="Silicate_umol.kg",
                                      "nitrite_nitrate"="Nitrite_and_Nitrate_umol.kg",
                                      "ammonia"="Ammonium_umol.kg",
                                      "phosphate"="Phosphate_umol.kg",
                                      "nutrients_flag"="Nutrients_Flag")%>%
  select(date,lat,lon,depth_bottom,depth_sampling,temp,salinity,salinity_flag,oxy,silicate,nitrite_nitrate,ammonia,phosphate,nutrients_flag)



df_nut_2014_1 <- read.csv(paste0(basepath,"/data/ecomon_data/nutrients/2014/334B20141103-PC1405_data.csv"))
df_nut_2014_2 <- read.csv(paste0(basepath,"data/ecomon_data/nutrients/2014/33GG20140301-GU1401-Leg2_data.csv"))
df_nut_2014 <- rbind(df_nut_2014_1,df_nut_2014_2)
df_nut_2014$date<-as.Date(with(df_nut_2014,paste(Year_UTC,Month_UTC,Day_UTC,sep="-")),"%Y-%m-%d")

df_nut_2014 <- df_nut_2014 %>% rename("lat" = "Latitude_Dec_Deg",
                                      "lon" = "Longitude_Dec_Deg",
                                      "depth_bottom"="Depth_Bottom_meters",         
                                      "depth_sampling" ="Depth_meters",
                                      "temp"="CTDTEMP_ITS90",
                                      "salinity"="Salinity_PSS78", 
                                      "salinity_flag" ="Salinity_Flag",
                                      "oxy"="CTDOXY_umol.kg",
                                      "silicate"="Silicate_umol.kg",
                                      "nitrite_nitrate"="Nitrite_and_Nitrate_umol.kg",
                                      "ammonia"="Ammonium_umol.kg",
                                      "phosphate"="Phosphate_umol.kg",
                                      "nutrients_flag"="Nutrients_Flag")%>%
  select(date,lat,lon,depth_bottom,depth_sampling,temp,salinity,salinity_flag,oxy,silicate,nitrite_nitrate,ammonia,phosphate,nutrients_flag)

df_nut_2015_1 <- read.csv(paste0(basepath,"/data/ecomon_data/nutrients/2015/33HH20150519-HB1502_data.csv"))
df_nut_2015_2 <- read.csv(paste0(basepath,"data/ecomon_data/nutrients/2015/33GG20151012-GU1506-Leg2_data.csv"))
df_nut_2015 <- rbind(df_nut_2015_1,df_nut_2015_2)
df_nut_2015$date<-as.Date(with(df_nut_2015,paste(Year_UTC,Month_UTC,Day_UTC,sep="-")),"%Y-%m-%d")
df_nut_2015 <- df_nut_2015 %>% rename("lat" = "Latitude_Dec_Deg",
                                      "lon" = "Longitude_Dec_Deg",
                                      "depth_bottom"="Depth_Bottom_meters",         
                                      "depth_sampling" ="Depth_meters",
                                      "temp"="CTDTEMP_ITS90",
                                      "salinity"="Salinity_PSS78", 
                                      "salinity_flag" ="Salinity_Flag",
                                      "oxy"="CTDOXY_umol.kg",
                                      "silicate"="Silicate_umol.kg",
                                      "nitrite_nitrate"="Nitrite_and_Nitrate_umol.kg",
                                      "ammonia"="Ammonium_umol.kg",
                                      "phosphate"="Phosphate_umol.kg",
                                      "nutrients_flag"="Nutrients_Flag")%>%
  select(date,lat,lon,depth_bottom,depth_sampling,temp,salinity,salinity_flag,oxy,silicate,nitrite_nitrate,ammonia,phosphate,nutrients_flag)


df_nut_2016_1 <- read.csv(paste0(basepath,"/data/ecomon_data/nutrients/2016/33GG20160521-GU1608-Leg1_data.csv"))
df_nut_2016_2 <- read.csv(paste0(basepath,"data/ecomon_data/nutrients/2016/33GG20160607-GU1608-Leg2_data.csv"))
df_nut_2016_3 <- read.csv(paste0(basepath,"data/ecomon_data/nutrients/2016/334B20160807-PC1607_data.csv"))
df_nut_2016_4 <- read.csv(paste0(basepath,"data/ecomon_data/nutrients/2016/334B20161018-PC1609_data.csv"))

df_nut_2016 <- rbind(df_nut_2016_1,df_nut_2016_2,df_nut_2016_3,df_nut_2016_4)
df_nut_2016$date<-as.Date(with(df_nut_2016,paste(Year_UTC,Month_UTC,Day_UTC,sep="-")),"%Y-%m-%d")
df_nut_2016 <- df_nut_2016 %>% rename("lat" = "Latitude_Dec_Deg",
                                      "lon" = "Longitude_Dec_Deg",
                                      "depth_bottom"="Depth_Bottom_meters",         
                                      "depth_sampling" ="Depth_meters",
                                      "temp"="CTDTEMP_ITS90",
                                      "salinity"="Salinity_PSS78", 
                                      "salinity_flag" ="Salinity_Flag",
                                      "oxy"="CTDOXY_umol.kg",
                                      "silicate"="Silicate_umol.kg",
                                      "nitrite_nitrate"="Nitrite_and_Nitrate_umol.kg",
                                      "ammonia"="Ammonium_umol.kg",
                                      "phosphate"="Phosphate_umol.kg",
                                      "nutrients_flag"="Nutrients_Flag")%>%
  select(date,lat,lon,depth_bottom,depth_sampling,temp,salinity,salinity_flag,oxy,silicate,nitrite_nitrate,ammonia,phosphate,nutrients_flag)



df_nut_2017_1 <- read.csv(paste0(basepath,"/data/ecomon_data/nutrients/2017/33GG20170516-GU1701-Leg1_data.csv"))
df_nut_2017_2 <- read.csv(paste0(basepath,"data/ecomon_data/nutrients/2017/33GG20170530-GU1701-Leg2_data.csv"))
df_nut_2017_3 <- read.csv(paste0(basepath,"data/ecomon_data/nutrients/2017/33GG20170610-GU1702_data.csv"))
df_nut_2017_4 <- read.csv(paste0(basepath,"data/ecomon_data/nutrients/2017/33GG20171031-GU1706_data.csv"))
df_nut_2017_5 <- read.csv(paste0(basepath,"data/ecomon_data/nutrients/2017/33HH20170211-HB1701_data.csv"))

df_nut_2017 <- rbind(df_nut_2017_1,df_nut_2017_2,df_nut_2017_3,df_nut_2017_4,df_nut_2017_5)
df_nut_2017$date<-as.Date(with(df_nut_2017,paste(Year_UTC,Month_UTC,Day_UTC,sep="-")),"%Y-%m-%d")
df_nut_2017 <- df_nut_2017 %>% rename("lat" = "Latitude_Dec_Deg",
                                      "lon" = "Longitude_Dec_Deg",
                                      "depth_bottom"="Depth_Bottom_meters",         
                                      "depth_sampling" ="Depth_meters",
                                      "temp"="CTDTEMP_ITS90",
                                      "salinity"="Salinity_PSS78", 
                                      "salinity_flag" ="Salinity_Flag",
                                      "oxy"="CTDOXY_umol.kg",
                                      "silicate"="Silicate_umol.kg",
                                      "nitrite_nitrate"="Nitrite_and_Nitrate_umol.kg",
                                      "ammonia"="Ammonium_umol.kg",
                                      "phosphate"="Phosphate_umol.kg",
                                      "nutrients_flag"="Nutrients_Flag")%>%
  select(date,lat,lon,depth_bottom,depth_sampling,temp,salinity,salinity_flag,oxy,silicate,nitrite_nitrate,ammonia,phosphate,nutrients_flag)


df_nut_2018_1 <- read.csv(paste0(basepath,"/data/ecomon_data/nutrients/2018/33HH20180523-HB1803_data.csv"))
df_nut_2018_2 <- read.csv(paste0(basepath,"data/ecomon_data/nutrients/2018/33GG20180822-GU1804_data.csv"))
df_nut_2018_3 <- read.csv(paste0(basepath,"data/ecomon_data/nutrients/2018/33H520181102-S11802_data.csv"))
df_nut_2018_4 <- read_excel(paste0(basepath,"data/ecomon_data/nutrients/2018/E04_ECOA2RR.xlsx"))

df_nut_2018_4$date<-as.Date(with(df_nut_2018_4,paste(Year,Month,Day,sep="-")),"%Y-%m-%d")

df_nut_2018_4 <- df_nut_2018_4 %>% rename("lat" = "Latitude_DEC_DEG",
                                          "lon" = "Longitude_DEC_DEG",
                                          "depth_sampling" ="Depth_Meters",
                                          "temp"="CTDTMP1_ITS-90-DEG_C_use",
                                          "salinity"="CTDSAL1_PSS-78", 
                                          "salinity_flag" = "CTDSAL1_Flag",
                                          "oxy"="OXYGEN_UMOL/KG",
                                          "silicate"="Silicate_UMOL/KG",
                                          "nitrite_nitrate"="NO2+NO3_UMOL/KG",
                                          "phosphate"="PO4_UMOIL/KG",
                                          "nutrients_flag" = "Silicate_FLAG") %>%
  select(date,lat,lon,depth_sampling,temp,salinity,salinity_flag,oxy,silicate,nitrite_nitrate,phosphate,nutrients_flag)
df_nut_2018_4$ammonia = NaN
df_nut_2018_4$depth_bottom = NaN

df_nut_2018 <- rbind(df_nut_2018_1,df_nut_2018_2,df_nut_2018_3)
df_nut_2018$date<-as.Date(with(df_nut_2018,paste(Year_UTC,Month_UTC,Day_UTC,sep="-")),"%Y-%m-%d")
df_nut_2018 <- df_nut_2018 %>% rename("lat" = "Latitude_Dec_Deg",
                                      "lon" = "Longitude_Dec_Deg",
                                      "depth_bottom"="Depth_Bottom_meters",         
                                      "depth_sampling" ="Depth_meters",
                                      "temp"="CTDTEMP_ITS90",
                                      "salinity"="Salinity_PSS78", 
                                      "salinity_flag" ="Salinity_Flag",
                                      "oxy"="CTDOXY_umol.kg",
                                      "silicate"="Silicate_umol.kg",
                                      "nitrite_nitrate"="Nitrite_and_Nitrate_umol.kg",
                                      "ammonia"="Ammonium_umol.kg",
                                      "phosphate"="Phosphate_umol.kg",
                                      "nutrients_flag"="Nutrients_Flag")%>%
  select(date,lat,lon,depth_bottom,depth_sampling,temp,salinity,salinity_flag,oxy,silicate,nitrite_nitrate,ammonia,phosphate,nutrients_flag)


df_nut_2018_merged <- rbind(df_nut_2018,df_nut_2018_4)


df_nut_2019_1 <- read.csv(paste0(basepath,"/data/ecomon_data/nutrients/2019/33HH20190522-HB1902_data.csv"))
df_nut_2019_2 <- read.csv(paste0(basepath,"data/ecomon_data/nutrients/2019/33GG20190815-GU1902_data.csv"))

df_nut_2019 <- rbind(df_nut_2019_1,df_nut_2019_2)
df_nut_2019$date<-as.Date(with(df_nut_2019,paste(Year_UTC,Month_UTC,Day_UTC,sep="-")),"%Y-%m-%d")
df_nut_2019 <- df_nut_2019 %>% rename("lat" = "Latitude_Dec_Deg",
                                      "lon" = "Longitude_Dec_Deg",
                                      "depth_bottom"="Depth_Bottom_meters",         
                                      "depth_sampling" ="Depth_meters",
                                      "temp"="CTDTEMP_ITS90",
                                      "salinity"="Salinity_PSS78", 
                                      "salinity_flag" ="Salinity_Flag",
                                      "oxy"="CTDOXY_umol.kg",
                                      "silicate"="Silicate_umol.kg",
                                      "nitrite_nitrate"="Nitrite_and_Nitrate_umol.kg",
                                      "ammonia"="Ammonium_umol.kg",
                                      "phosphate"="Phosphate_umol.kg",
                                      "nutrients_flag"="Nutrients_Flag")%>%
  select(date,lat,lon,depth_bottom,depth_sampling,temp,salinity,salinity_flag,oxy,silicate,nitrite_nitrate,ammonia,phosphate,nutrients_flag)

df_nut_2021_1 <- read.csv(paste0(basepath,"/data/ecomon_data/nutrients/2021/334B20211015-PC2106_Data.csv"))
df_nut_2021_2 <- read.csv(paste0(basepath,"data/ecomon_data/nutrients/2021/334B20210805-PC2104_Data.csv"))

df_nut_2021 <- rbind(df_nut_2021_1,df_nut_2021_2)
df_nut_2021$date<-as.Date(with(df_nut_2021,paste(Year_UTC,Month_UTC,Day_UTC,sep="-")),"%Y-%m-%d")
df_nut_2021 <- df_nut_2021 %>% rename("lat" = "Latitude_Dec_Deg",
                                      "lon" = "Longitude_Dec_Deg",
                                      "depth_bottom"="Depth_Bottom_meters",         
                                      "depth_sampling" ="Depth_meters",
                                      "temp"="CTDTEMP_ITS90",
                                      "salinity"="Salinity_PSS78", 
                                      "salinity_flag" ="Salinity_Flag",
                                      "oxy"="CTDOXY_umol.kg",
                                      "silicate"="Silicate_umol.kg",
                                      "nitrite_nitrate"="Nitrite_and_Nitrate_umol.kg",
                                      "ammonia"="Ammonium_umol.kg",
                                      "phosphate"="Phosphate_umol.kg",
                                      "nutrients_flag"="Nutrients_Flag")%>%
  select(date,lat,lon,depth_bottom,depth_sampling,temp,salinity,salinity_flag,oxy,silicate,nitrite_nitrate,ammonia,phosphate,nutrients_flag)

df_nut_2022_1 <- read.csv(paste0(basepath,"data/ecomon_data/nutrients/2022/33HH20220531_HB2204_Data.csv"))
df_nut_2022_2 <- read_excel(paste0(basepath,"/data/ecomon_data/nutrients/2022/ECOA_3_CTD_MasterDataSheet_09_26_2023.xlsx"))

df_nut_2022_1$date<-as.Date(with(df_nut_2022_1,paste(Year_UTC,Month_UTC,Day_UTC,sep="-")),"%Y-%m-%d")
df_nut_2022_1 <- df_nut_2022_1 %>% rename("lat" = "Latitude_Dec_Deg",
                                          "lon" = "Longitude_Dec_Deg",
                                          "depth_bottom"="Depth_Bottom_meters",         
                                          "depth_sampling" ="Depth_meters",
                                          "temp"="CTDTEMP_ITS90",
                                          "salinity"="Salinity_PSS78", 
                                          "salinity_flag" ="Salinity_Flag",
                                          "oxy"="CTDOXY_umol.kg",
                                          "silicate"="Silicate_umol.kg",
                                          "nitrite_nitrate"="Nitrite_and_Nitrate_umol.kg",
                                          "ammonia"="Ammonium_umol.kg",
                                          "phosphate"="Phosphate_umol.kg",
                                          "nutrients_flag"="Nutrients_Flag")%>%
  select(date,lat,lon,depth_bottom,depth_sampling,temp,salinity,salinity_flag,oxy,silicate,nitrite_nitrate,ammonia,phosphate,nutrients_flag)

df_nut_2022_2$date<-as.Date(with(df_nut_2022_2,paste(Year_UTC,Month_UTC,Day_UTC,sep="-")),"%Y-%m-%d")
df_nut_2022_2 <- df_nut_2022_2 %>% rename("lat" = "Latitude",
                                          "lon" = "Longitude",
                                          "depth_sampling" ="Depth",
                                          "temp"="CTDTEMP_ITS90",
                                          "salinity"="CTDSAL_PSS78", 
                                          "salinity_flag" ="CTDSAL_PSS78_flag",
                                          "oxy"="CTDOXY",
                                          "silicate"="Silicate",
                                          "nitrite_nitrate"="Nitrate",
                                          "ammonia"="Ammonium",
                                          "phosphate"="Phosphate",
                                          "nutrients_flag"="Nitrate_flag")%>%
  select(date,lat,lon,depth_sampling,temp,salinity,salinity_flag,oxy,silicate,nitrite_nitrate,ammonia,phosphate,nutrients_flag)

df_nut_2022_2$depth_bottom = NaN

df_nut_2022 <- rbind(df_nut_2022_1,df_nut_2022_2)

df_nut = rbind(df_nut_2007,df_nut_2009,df_nut_2012,df_nut_2013,df_nut_2014,df_nut_2015,df_nut_2016,df_nut_2017,df_nut_2018_merged,df_nut_2019,df_nut_2021,df_nut_2022)
df_nut <- df_nut %>% filter(nitrite_nitrate != -999) %>%mutate_if(is.character, as.numeric)

df_nut$year <- year(df_nut$date)
metseasons <- c(
  "01" = "Winter", "02" = "Winter",
  "03" = "Spring", "04" = "Spring", "05" = "Spring",
  "06" = "Summer", "07" = "Summer", "08" = "Summer",
  "09" = "Fall", "10" = "Fall", "11" = "Fall",
  "12" = "Winter")
seasons = metseasons[format(df_nut$date, "%m")]
df_nut$season = seasons

save(df_nut,file=paste0(basepath,"/data/ecomon_nutrients_2007_2023.RData"))
write.csv(df_nut,file=paste0(basepath,"/data/ecomon_nutrients_2007_2023.csv"))

