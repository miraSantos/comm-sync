#PURPOSE: Read in nutrient data from ecomon cruises
#data are housed in 

list.of.packages <- c("readxl","fields","ggplot2","sf","broom","dplyr","lubridate","purrr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

basepath = "/home/mira/MIT-WHOI/github_repos/comm-sync/"

# David Townsend datasets
# http://grampus.umeoce.maine.edu/nutrients/
df1= read.delim(paste0(basepath,"/data/ecomon_data/raw_townsend/RebuckGoMaineNutrients.txt"),
                sep = "\t",header=F)[,1:16]

df2 =  read.delim(paste0(basepath,"/data/ecomon_data/raw_townsend/gomregion.txt"),
                  sep = "\t",header=F)[,1:16]
headers = c("month","day","year","lon", "lat",
            "depth_bottom", "depth_sampling","temp","salinity",
            "nitrite_nitrate","silicate","phosphate", "extracted_chlorophyll",
            "phosphate_flag","silicate_flag","nitrite_nitrate_flag")
colnames(df1) <- headers
colnames(df2) <- headers

df1$date <- as.character(as.Date(with(df1,paste(year,month,day,sep="-")),"%Y-%m-%d"))
df2$date <- as.character(as.Date(with(df2,paste(year,month,day,sep="-")),"%Y-%m-%d"))

dft <- rbind(df1,df2)
dft$date <- as.Date(dft$date,format = "%Y-%m-%d")
dft$cruise_id = as.character(NaN)
dft$oxy = NaN
dft$nutrients_flag = NaN
dft$salinity_flag = NaN
dft$ctd_pressure = dft$depth_sampling

length(dft[dft$phosphate_flag==0,]$phosphate)
length(dft[dft$silicate_flag==0,]$phosphate)
length(dft[dft$nitrite_nitrate==0,]$phosphate)
length(dft[dft$nitrite_nitrate==0,]$nitrite_nitrate)
range(((dft[dft$nitrite_nitrate==0,]$date)))

#recoding 0 as 2 to match green flag of other datasets
dft <- dft %>% mutate(phosphate_flag = recode(phosphate_flag,`0` = 2),
               silicate_flag = recode(silicate_flag,`0` = 2),
               nitrite_nitrate_flag = recode(nitrite_nitrate_flag,`0` = 2))
               


df_nut_2007 <- read_excel(paste0(basepath,"/data/ecomon_data/nutrients/2007/E01_GOMECC1R.xlsx"))
df_nut_2007$date <- as.Date(with(df_nut_2007,paste(Year,Month,Day,sep="-")),"%Y-%m-%d")
df_nut_2007%>% filter(`NITRAT_FLAG_W`==2,`NITRIT_FLAG_W`==2) %>% ggplot()+
  geom_point(aes(x=date,y=`NITRAT_UMOL/KG`,color="Nitrate"))+
  geom_point(aes(x=date,y=`NITRIT_UMOL/KG`,color="Nitrite"))+
  xlab("Date")+ylab("Concentration (umol/kg)")
df_nut_2007 <- df_nut_2007 %>% 
  filter(NITRAT_FLAG_W==2,NH4_FLAG_W==2) %>%
  mutate(nitrite_nitrate = `NITRAT_UMOL/KG`+`NITRIT_UMOL/KG`,nitrite_nitrate_flag= 2) %>%
  rename("lat" = "LATITUDE_DEC_DEG",
                                      "lon" = "LONGITUDE_DEC_DEG",
                                      "depth_bottom"="DEPTH_bottom_METERS", 
                                      "ctd_pressure" = "CTDPRS_DBARS",
                                      "temp"="CTDTMP_ITS-90_DEG_C",
                                      "salinity"="SALNTY",  
                                      "salinity_flag"="SALNTY_FLAG_W",
                                      "oxy"="CTDOXY_corrected_UMOL/KG",
                                      "silicate"="SILCAT_UMOL/KG",
                                      "silicate_flag" = "SILCAT_FLAG_W",
                                      "ammonia"="NH4_UMOL/KG",
                                      "ammonia_flag"="NH4_FLAG_W",
                                      "phosphate"="PHSPHT_UMOL/KG",
                                      "phosphate_flag" = "PHSPHT_FLAG_W",
                                      "cruise_id" = "Cruise ID") %>%
    mutate(cruise_id = "gomec_2007",depth_sampling=NaN)%>%
  select(date,lat,lon,cruise_id,depth_bottom,depth_sampling,temp,salinity,salinity_flag,
         oxy,silicate,silicate_flag,nitrite_nitrate,nitrite_nitrate_flag,
         ammonia,ammonia_flag,phosphate,phosphate_flag,cruise_id,ctd_pressure)

#flag is -999
df_nut_2009 <- read_excel(paste0(basepath,"/data/ecomon_data/nutrients/2009/NEFSC_nutrient.xlsx"))
df_nut_2009$date <- as.Date(with(df_nut_2009,paste(Year,Month,Day,sep="-")),"%Y-%m-%d")

df_nut_2009 <- df_nut_2009 %>% rename("lat" = "Latitude_DEC_DEG",
                                      "lon" = "Longitude_DEC_DEG",
                                      "cruise_id" = "Cruise_ID",
                                      "ctd_pressure" = "CTDPRS_DBARS",
                                      "depth_bottom"="Depth_bottom_METERS",         
                                      "depth_sampling" ="Depth_sampling_METERS",
                                      "temp"="CTDTMP_ITS-90_DEG_C",
                                      "salinity"="CTDSAL_PSS-78",  
                                      "oxy"="CTDOXY_UMOL/KG",
                                      "silicate"="SILCAT_micromoles/Kilogram",
                                      "nitrite_nitrate"="NITRIT+NITRAT_micromoles/kilogam",
                                      "ammonia"="AMMONIA_micromoles/kilogram",
                                      "phosphate"="PHSPHT_micromoles/kilogram") %>%
  mutate(silicate_flag =case_when(silicate != -999 ~ 2),
         nitrite_nitrate_flag=case_when(nitrite_nitrate != -999 ~ 2),
         ammonia_flag=case_when(ammonia != -999 ~ 2),
         phosphate_flag=case_when(phosphate != -999 ~ 2),
         salinity_flag=case_when(salinity!=-999 ~ 2))%>%
  select(date,lat,lon,cruise_id,depth_bottom,depth_sampling,temp,salinity,salinity_flag,oxy,
         silicate,nitrite_nitrate,ammonia,phosphate,
         phosphate_flag,nitrite_nitrate_flag,ammonia_flag,silicate_flag,ctd_pressure)


df_nut_2012_1 <- read.csv(paste0(basepath,"/data/ecomon_data/nutrients/2012/334B20121026-PC1207_data.csv"))
df_nut_2012_2 <- read.csv(paste0(basepath,"/data/ecomon_data/nutrients/2012/316G20120202-DE1202_data.csv"))
df_nut_2012_3 <- read.csv(paste0(basepath,"/data/ecomon_data/nutrients/2012/33HH20120531-HB1202_data.csv"))

df_nut_2012 <- rbind(df_nut_2012_1,df_nut_2012_2,df_nut_2012_3)
df_nut_2012$date<-as.Date(with(df_nut_2012,paste(Year_UTC,Month_UTC,Day_UTC,sep="-")),"%Y-%m-%d")

str(df_nut_2012)
df_nut_2012 <- df_nut_2012 %>% rename("lat" = "Latitude_Dec_Deg",
                                      "lon" = "Longitude_Dec_Deg",
                                      "cruise_id" = "Cruise_ID",
                                      "ctd_pressure" = "CTDPRES_dbar",
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
  mutate(phosphate_flag = nutrients_flag,
         nitrite_nitrate_flag = nutrients_flag,
         ammonia_flag = nutrients_flag,
         silicate_flag=nutrients_flag)%>%
  select(date,lat,lon,cruise_id,depth_bottom,depth_sampling,temp,salinity,salinity_flag,
         oxy,silicate,nitrite_nitrate,ammonia,phosphate,nutrients_flag,
         phosphate_flag,nitrite_nitrate_flag,ammonia_flag,silicate_flag,ctd_pressure)



df_nut_2013_1 <- read.csv(paste0(basepath,"data/ecomon_data/nutrients/2013/33GG20130609-GU1302_data.csv"))
df_nut_2013_2 <- read.csv(paste0(basepath,"data/ecomon_data/nutrients/2013/33GG20131113-GU1305_data.csv"))
df_nut_2013 <- rbind(df_nut_2013_1,df_nut_2013_2)
df_nut_2013$date<-as.Date(with(df_nut_2013,paste(Year_UTC,Month_UTC,Day_UTC,sep="-")),"%Y-%m-%d")
df_nut_2013 <- df_nut_2013 %>% rename("lat" = "Latitude_Dec_Deg",
                                      "lon" = "Longitude_Dec_Deg",
                                      "cruise_id" = "Cruise_ID",
                                      "depth_bottom"="Depth_Bottom_meters", 
                                      "ctd_pressure"= "CTDPRES_dbar",
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
  mutate(phosphate_flag = nutrients_flag,
         nitrite_nitrate_flag = nutrients_flag,
         ammonia_flag = nutrients_flag,
         silicate_flag=nutrients_flag)%>%
  select(date,lat,lon,cruise_id,depth_bottom,depth_sampling,temp,salinity,salinity_flag,
         oxy,silicate,nitrite_nitrate,ammonia,phosphate,nutrients_flag,
         phosphate_flag,nitrite_nitrate_flag,ammonia_flag,silicate_flag,ctd_pressure)



df_nut_2014_1 <- read.csv(paste0(basepath,"/data/ecomon_data/nutrients/2014/334B20141103-PC1405_data.csv"))
df_nut_2014_2 <- read.csv(paste0(basepath,"data/ecomon_data/nutrients/2014/33GG20140301-GU1401-Leg2_data.csv"))
df_nut_2014 <- rbind(df_nut_2014_1,df_nut_2014_2)
df_nut_2014$date<-as.Date(with(df_nut_2014,paste(Year_UTC,Month_UTC,Day_UTC,sep="-")),"%Y-%m-%d")

df_nut_2014 <- df_nut_2014 %>% rename("lat" = "Latitude_Dec_Deg",
                                      "lon" = "Longitude_Dec_Deg",
                                      "cruise_id" = "Cruise_ID",
                                      "depth_bottom"="Depth_Bottom_meters",         
                                      "depth_sampling" ="Depth_meters",
                                      "ctd_pressure" = "CTDPRES_dbar",
                                      "temp"="CTDTEMP_ITS90",
                                      "salinity"="Salinity_PSS78", 
                                      "salinity_flag" ="Salinity_Flag",
                                      "oxy"="CTDOXY_umol.kg",
                                      "silicate"="Silicate_umol.kg",
                                      "nitrite_nitrate"="Nitrite_and_Nitrate_umol.kg",
                                      "ammonia"="Ammonium_umol.kg",
                                      "phosphate"="Phosphate_umol.kg",
                                      "nutrients_flag"="Nutrients_Flag")%>%
  mutate(phosphate_flag = nutrients_flag,
         nitrite_nitrate_flag = nutrients_flag,
         ammonia_flag = nutrients_flag,
         silicate_flag=nutrients_flag)%>%
  select(date,lat,lon,cruise_id,depth_bottom,depth_sampling,temp,salinity,
         salinity_flag,oxy,silicate,nitrite_nitrate,ammonia,phosphate,nutrients_flag,
         phosphate_flag,nitrite_nitrate_flag,ammonia_flag,silicate_flag,ctd_pressure)

df_nut_2015_1 <- read.csv(paste0(basepath,"/data/ecomon_data/nutrients/2015/33HH20150519-HB1502_data.csv"))
df_nut_2015_2 <- read.csv(paste0(basepath,"data/ecomon_data/nutrients/2015/33GG20151012-GU1506-Leg2_data.csv"))
df_nut_2015 <- rbind(df_nut_2015_1,df_nut_2015_2)
df_nut_2015$date<-as.Date(with(df_nut_2015,paste(Year_UTC,Month_UTC,Day_UTC,sep="-")),"%Y-%m-%d")
df_nut_2015 <- df_nut_2015 %>% rename("lat" = "Latitude_Dec_Deg",
                                      "lon" = "Longitude_Dec_Deg",
                                      "cruise_id" = "Cruise_ID",
                                      "depth_bottom"="Depth_Bottom_meters",         
                                      "depth_sampling" ="Depth_meters",
                                      "ctd_pressure" = "CTDPRES_dbar",
                                      "temp"="CTDTEMP_ITS90",
                                      "salinity"="Salinity_PSS78", 
                                      "salinity_flag" ="Salinity_Flag",
                                      "oxy"="CTDOXY_umol.kg",
                                      "silicate"="Silicate_umol.kg",
                                      "nitrite_nitrate"="Nitrite_and_Nitrate_umol.kg",
                                      "ammonia"="Ammonium_umol.kg",
                                      "phosphate"="Phosphate_umol.kg",
                                      "nutrients_flag"="Nutrients_Flag")%>%
  mutate(phosphate_flag = nutrients_flag,
         nitrite_nitrate_flag = nutrients_flag,
         ammonia_flag = nutrients_flag,
         silicate_flag=nutrients_flag)%>%
  select(date,lat,lon,cruise_id,depth_bottom,depth_sampling,temp,salinity,salinity_flag,
         oxy,silicate,nitrite_nitrate,ammonia,phosphate,nutrients_flag,
         phosphate_flag,nitrite_nitrate_flag,ammonia_flag,silicate_flag,ctd_pressure)


df_nut_2016_1 <- read.csv(paste0(basepath,"/data/ecomon_data/nutrients/2016/33GG20160521-GU1608-Leg1_data.csv"))
df_nut_2016_2 <- read.csv(paste0(basepath,"data/ecomon_data/nutrients/2016/33GG20160607-GU1608-Leg2_data.csv"))
df_nut_2016_3 <- read.csv(paste0(basepath,"data/ecomon_data/nutrients/2016/334B20160807-PC1607_data.csv"))
df_nut_2016_4 <- read.csv(paste0(basepath,"data/ecomon_data/nutrients/2016/334B20161018-PC1609_data.csv"))

df_nut_2016 <- rbind(df_nut_2016_1,df_nut_2016_2,df_nut_2016_3,df_nut_2016_4)
df_nut_2016$date<-as.Date(with(df_nut_2016,paste(Year_UTC,Month_UTC,Day_UTC,sep="-")),"%Y-%m-%d")
df_nut_2016 <- df_nut_2016 %>% rename("lat" = "Latitude_Dec_Deg",
                                      "lon" = "Longitude_Dec_Deg",
                                      "cruise_id"="Cruise_ID",
                                      "depth_bottom"="Depth_Bottom_meters",         
                                      "depth_sampling" ="Depth_meters",
                                      "temp"="CTDTEMP_ITS90",
                                      "ctd_pressure" = "CTDPRES_dbar",
                                      "salinity"="Salinity_PSS78", 
                                      "salinity_flag" ="Salinity_Flag",
                                      "oxy"="CTDOXY_umol.kg",
                                      "silicate"="Silicate_umol.kg",
                                      "nitrite_nitrate"="Nitrite_and_Nitrate_umol.kg",
                                      "ammonia"="Ammonium_umol.kg",
                                      "phosphate"="Phosphate_umol.kg",
                                      "nutrients_flag"="Nutrients_Flag")%>%
  mutate(phosphate_flag = nutrients_flag,
         nitrite_nitrate_flag = nutrients_flag,
         ammonia_flag = nutrients_flag,
         silicate_flag=nutrients_flag)%>%
  select(date,lat,lon,cruise_id,depth_bottom,depth_sampling,temp,salinity,salinity_flag,oxy,
         silicate,nitrite_nitrate,ammonia,phosphate,nutrients_flag,
         phosphate_flag,nitrite_nitrate_flag,ammonia_flag,silicate_flag,ctd_pressure)

df_nut_2017_1 <- read.csv(paste0(basepath,"/data/ecomon_data/nutrients/2017/33GG20170516-GU1701-Leg1_data.csv"))
df_nut_2017_2 <- read.csv(paste0(basepath,"data/ecomon_data/nutrients/2017/33GG20170530-GU1701-Leg2_data.csv"))
df_nut_2017_3 <- read.csv(paste0(basepath,"data/ecomon_data/nutrients/2017/33GG20170610-GU1702_data.csv"))
df_nut_2017_4 <- read.csv(paste0(basepath,"data/ecomon_data/nutrients/2017/33GG20171031-GU1706_data.csv"))
df_nut_2017_5 <- read.csv(paste0(basepath,"data/ecomon_data/nutrients/2017/33HH20170211-HB1701_data.csv"))

df_nut_2017 <- rbind(df_nut_2017_1,df_nut_2017_2,df_nut_2017_3,df_nut_2017_4,df_nut_2017_5)
df_nut_2017$date<-as.Date(with(df_nut_2017,paste(Year_UTC,Month_UTC,Day_UTC,sep="-")),"%Y-%m-%d")
df_nut_2017 <- df_nut_2017 %>% rename("lat" = "Latitude_Dec_Deg",
                                      "lon" = "Longitude_Dec_Deg",
                                      "cruise_id" = "Cruise_ID",
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
                                      "nutrients_flag"="Nutrients_Flag",
                                      "ctd_pressure" = "CTDPRES_dbar")%>%
  mutate(phosphate_flag = nutrients_flag,
         nitrite_nitrate_flag = nutrients_flag,
         ammonia_flag = nutrients_flag,
         silicate_flag=nutrients_flag)%>%
  select(date,lat,lon,cruise_id,depth_bottom,depth_sampling,temp,salinity,salinity_flag,
         oxy,silicate,nitrite_nitrate,ammonia,phosphate,nutrients_flag,
         phosphate_flag,nitrite_nitrate_flag,ammonia_flag,silicate_flag,ctd_pressure)


df_nut_2018_1 <- read.csv(paste0(basepath,"/data/ecomon_data/nutrients/2018/33HH20180523-HB1803_data.csv"))
df_nut_2018_2 <- read.csv(paste0(basepath,"data/ecomon_data/nutrients/2018/33GG20180822-GU1804_data.csv"))
df_nut_2018_3 <- read.csv(paste0(basepath,"data/ecomon_data/nutrients/2018/33H520181102-S11802_data.csv"))
df_nut_2018_4 <- read_excel(paste0(basepath,"data/ecomon_data/nutrients/2018/E04_ECOA2RR.xlsx"))

df_nut_2018_4$date<-as.Date(with(df_nut_2018_4,paste(Year,Month,Day,sep="-")),"%Y-%m-%d")

df_nut_2018_4 <- df_nut_2018_4 %>% rename("lat" = "Latitude_DEC_DEG",
                                          "lon" = "Longitude_DEC_DEG",
                                          "cruise_id" = "Cruise_ID",
                                          "depth_sampling" ="Depth_Meters",
                                          "ctd_pressure" = "Pressure_DBARS",
                                          "temp"="CTDTMP1_ITS-90-DEG_C_use",
                                          "salinity"="CTDSAL1_PSS-78", 
                                          "salinity_flag" = "CTDSAL1_Flag",
                                          "oxy"="OXYGEN_UMOL/KG",
                                          "silicate"="Silicate_UMOL/KG",
                                          "nitrite_nitrate"="NO2+NO3_UMOL/KG",
                                          "phosphate"="PO4_UMOIL/KG",
                                          "nutrients_flag" = "Silicate_FLAG") %>%
  mutate(phosphate_flag = nutrients_flag,
         nitrite_nitrate_flag = nutrients_flag,
         ammonia_flag = nutrients_flag,
         silicate_flag=nutrients_flag)%>%
  select(date,lat,lon,cruise_id,depth_sampling,temp,salinity,salinity_flag,
         oxy,silicate,nitrite_nitrate,phosphate,nutrients_flag,
         phosphate_flag,nitrite_nitrate_flag,ammonia_flag,silicate_flag,ctd_pressure)
df_nut_2018_4$ammonia = NaN
df_nut_2018_4$depth_bottom = NaN

df_nut_2018 <- rbind(df_nut_2018_1,df_nut_2018_2,df_nut_2018_3)
df_nut_2018$date<-as.Date(with(df_nut_2018,paste(Year_UTC,Month_UTC,Day_UTC,sep="-")),"%Y-%m-%d")
df_nut_2018 <- df_nut_2018 %>% rename("lat" = "Latitude_Dec_Deg",
                                      "lon" = "Longitude_Dec_Deg",
                                      "cruise_id" = "Cruise_ID",
                                      "depth_bottom"="Depth_Bottom_meters",         
                                      "depth_sampling" ="Depth_meters",
                                      "temp"="CTDTEMP_ITS90",
                                      "ctd_pressure" = "CTDPRES_dbar",
                                      "salinity"="Salinity_PSS78", 
                                      "salinity_flag" ="Salinity_Flag",
                                      "oxy"="CTDOXY_umol.kg",
                                      "silicate"="Silicate_umol.kg",
                                      "nitrite_nitrate"="Nitrite_and_Nitrate_umol.kg",
                                      "ammonia"="Ammonium_umol.kg",
                                      "phosphate"="Phosphate_umol.kg",
                                      "nutrients_flag"="Nutrients_Flag")%>%
  mutate(phosphate_flag = nutrients_flag,
         nitrite_nitrate_flag = nutrients_flag,
         ammonia_flag = nutrients_flag,
         silicate_flag=nutrients_flag)%>%
  select(date,lat,lon,cruise_id,depth_bottom,depth_sampling,temp,salinity,salinity_flag,oxy,
         silicate,nitrite_nitrate,ammonia,phosphate,nutrients_flag,
         phosphate_flag,nitrite_nitrate_flag,ammonia_flag,silicate_flag,ctd_pressure)


df_nut_2018_merged <- rbind(df_nut_2018,df_nut_2018_4)


df_nut_2019_1 <- read.csv(paste0(basepath,"/data/ecomon_data/nutrients/2019/33HH20190522-HB1902_data.csv"))
df_nut_2019_2 <- read.csv(paste0(basepath,"data/ecomon_data/nutrients/2019/33GG20190815-GU1902_data.csv"))

df_nut_2019 <- rbind(df_nut_2019_1,df_nut_2019_2)
df_nut_2019$date<-as.Date(with(df_nut_2019,paste(Year_UTC,Month_UTC,Day_UTC,sep="-")),"%Y-%m-%d")
df_nut_2019 <- df_nut_2019 %>% rename("lat" = "Latitude_Dec_Deg",
                                      "lon" = "Longitude_Dec_Deg",
                                      "cruise_id" = "Cruise_ID",
                                      "depth_bottom"="Depth_Bottom_meters",         
                                      "depth_sampling" ="Depth_meters",
                                      "temp"="CTDTEMP_ITS90",
                                      "ctd_pressure" = "CTDPRES_dbar",
                                      "salinity"="Salinity_PSS78", 
                                      "salinity_flag" ="Salinity_Flag",
                                      "oxy"="CTDOXY_umol.kg",
                                      "silicate"="Silicate_umol.kg",
                                      "nitrite_nitrate"="Nitrite_and_Nitrate_umol.kg",
                                      "ammonia"="Ammonium_umol.kg",
                                      "phosphate"="Phosphate_umol.kg",
                                      "nutrients_flag"="Nutrients_Flag")%>%
  mutate(phosphate_flag = nutrients_flag,
         nitrite_nitrate_flag = nutrients_flag,
         ammonia_flag = nutrients_flag,
         silicate_flag=nutrients_flag)%>%
  select(date,lat,lon,cruise_id,depth_bottom,depth_sampling,temp,salinity,salinity_flag,oxy,
         silicate,nitrite_nitrate,ammonia,phosphate,nutrients_flag,
         phosphate_flag,nitrite_nitrate_flag,ammonia_flag,silicate_flag,ctd_pressure)

df_nut_2021_1 <- read.csv(paste0(basepath,"/data/ecomon_data/nutrients/2021/334B20211015-PC2106_Data.csv"))
df_nut_2021_2 <- read.csv(paste0(basepath,"data/ecomon_data/nutrients/2021/334B20210805-PC2104_Data.csv"))

df_nut_2021 <- rbind(df_nut_2021_1,df_nut_2021_2)
df_nut_2021$date<-as.Date(with(df_nut_2021,paste(Year_UTC,Month_UTC,Day_UTC,sep="-")),"%Y-%m-%d")
df_nut_2021 <- df_nut_2021 %>% rename("lat" = "Latitude_Dec_Deg",
                                      "lon" = "Longitude_Dec_Deg",
                                      "cruise_id" = "Cruise_ID",
                                      "depth_bottom"="Depth_Bottom_meters",         
                                      "depth_sampling" ="Depth_meters",
                                      "temp"="CTDTEMP_ITS90",
                                      "ctd_pressure" = "CTDPRES_dbar",
                                      "salinity"="Salinity_PSS78", 
                                      "salinity_flag" ="Salinity_Flag",
                                      "oxy"="CTDOXY_umol.kg",
                                      "silicate"="Silicate_umol.kg",
                                      "nitrite_nitrate"="Nitrite_and_Nitrate_umol.kg",
                                      "ammonia"="Ammonium_umol.kg",
                                      "phosphate"="Phosphate_umol.kg",
                                      "nutrients_flag"="Nutrients_Flag")%>%
  mutate(phosphate_flag = nutrients_flag,
         nitrite_nitrate_flag = nutrients_flag,
         ammonia_flag = nutrients_flag,
         silicate_flag=nutrients_flag)%>%
  select(date,lat,lon,cruise_id,depth_bottom,depth_sampling,temp,salinity,salinity_flag,
         oxy,silicate,nitrite_nitrate,ammonia,phosphate,nutrients_flag,
         phosphate_flag,nitrite_nitrate_flag,ammonia_flag,silicate_flag,ctd_pressure)

df_nut_2022_1 <- read.csv(paste0(basepath,"data/ecomon_data/nutrients/2022/33HH20220531_HB2204_Data.csv"))

df_nut_2022_1$date<-as.Date(with(df_nut_2022_1,paste(Year_UTC,Month_UTC,Day_UTC,sep="-")),"%Y-%m-%d")
df_nut_2022_1 <- df_nut_2022_1 %>% rename("lat" = "Latitude_Dec_Deg",
                                          "lon" = "Longitude_Dec_Deg",
                                          "cruise_id" = "Cruise_ID",
                                          "depth_bottom"="Depth_Bottom_meters", 
                                          "ctd_pressure" = "CTDPRES_dbar",
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
  mutate(phosphate_flag = nutrients_flag,
         nitrite_nitrate_flag = nutrients_flag,
         ammonia_flag = nutrients_flag,
         silicate_flag=nutrients_flag)%>%
  select(date,lat,lon,cruise_id,depth_bottom,depth_sampling,temp,salinity,salinity_flag,
         oxy,silicate,nitrite_nitrate,ammonia,phosphate,nutrients_flag,
         phosphate_flag,nitrite_nitrate_flag,ammonia_flag,silicate_flag,ctd_pressure)


df_nut_2022_2 <- read.table(paste0(basepath,"data/ecomon_data/nutrients/2022/334B20221101_PC2205_Data.csv"),sep=",",header=T)
df_nut_2022_2$date<-as.Date(df_nut_2022_2$Date_UTC,"%Y-%m-%d")

df_nut_2022_2 <- df_nut_2022_2 %>% rename("lat" = "Latitude",
                                          "lon" = "Longitude",
                                          "cruise_id" = "Cruise_ID",
                                          "depth_bottom"="Depth_station..M.", 
                                          "ctd_pressure" = "CTDPRS..DBARS.",
                                          "depth_sampling" ="Depth_sampling..M.",
                                          "temp"="CTDTMP..ITS.90.",
                                          "salinity"="CTDSAL..PSS.78.", 
                                          "oxy"="CTDOXY..mol.kg.",
                                          "silicate"="SILCAT..mol.kg.",
                                          "nitrite_nitrate"="NITRIT.NITRAT..mol.kg.",
                                          "ammonia"="AMMONIA..mol.kg.",
                                          "phosphate"="PHSPHT..mol.kg.",
                                          "nutrients_flag"="QC.Flag.Nut")%>%
  mutate(phosphate_flag = nutrients_flag,
         nitrite_nitrate_flag = nutrients_flag,
         ammonia_flag = nutrients_flag,
         silicate_flag=nutrients_flag)%>%
  select(date,lat,lon,cruise_id,depth_bottom,depth_sampling,temp,salinity,
         oxy,silicate,nitrite_nitrate,ammonia,phosphate,nutrients_flag,
         phosphate_flag,nitrite_nitrate_flag,ammonia_flag,silicate_flag,ctd_pressure)

df_nut_2022_2$salinity_flag = df_nut_2022_2$salinity

df_nut_2022 <- rbind(df_nut_2022_1,df_nut_2022_2)
df_nut_2022 <- df_nut_2022%>%mutate(across(-c(cruise_id,date), as.numeric))


#FINAL MERGE
df_nut <- list(dft,df_nut_2007,df_nut_2009,df_nut_2012,df_nut_2013,df_nut_2014,
               df_nut_2015,df_nut_2016,df_nut_2017,df_nut_2018_merged,
               df_nut_2019,df_nut_2021,df_nut_2022) %>% 
  reduce(full_join,by=c("date","lon","lat","cruise_id","depth_bottom",
                        "depth_sampling","temp",
                        "salinity","salinity_flag",
                        "nitrite_nitrate","phosphate","silicate",
                        "nitrite_nitrate_flag","phosphate_flag","silicate_flag","ctd_pressure"
                        )) %>%
  select(c("date","lon","lat","cruise_id","depth_bottom",
           "depth_sampling","temp",
           "salinity","salinity_flag",
           "nitrite_nitrate","phosphate","silicate",
           "nitrite_nitrate_flag","phosphate_flag","silicate_flag","ctd_pressure"))


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

