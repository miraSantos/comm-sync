df_light <- read.csv(paste0(basepath,"/data/mvco/mvco_daily_2023.csv"))
head(df_light)
df_light <- df_light %>% mutate(date = as.Date(df_light$days,format="%d-%b-%Y"),
                    week = week(date),year=year(date),
                    wyear = paste0(week,"-",year))


df_light_wyear_mean <-df_light %>%
  group_by(wyear) %>%
  mutate_at(c("AvgSolar"),mean,na.rm=T) %>%
  distinct(wyear, .keep_all=TRUE) %>%
  ungroup()

df_light %>% ggplot() + geom_point(aes(x=date,y=AvgSolar),size=0.3)+
  labs(x="Date",y="Average Solar Irradiance") +
  scale_x_date(date_breaks="2 year",date_labels=format("%Y"),
               limits=c(as.Date("2006-01-01"),as.Date("2020-12-31")))

ggsave(filename=paste0(basepath,"/figures/light_timeseries_",Sys.Date(),".png"),
       width=1500,height=400,units="px",dpi=150)

week_medians <- df_light %>% 
  group_by(week) %>%
  summarize_at(c("AvgSolar"),median,na.rm=T)

week <- sprintf("%02d",seq(1,53,1))
years <- seq(min(df_light$year),max(df_light$year),1)
week_list <- rep(week,length(years))
year_list <- rep(years,53)[order(rep(years,53))]

#average weekly annual cycle across entire time series
dfweek <- data.frame(wyear=paste0(week_list,"-",year_list),week=week_list,year=year_list)
week_medians_long <- week_medians %>% gather(key="env_var",value="mean_value",-c("week"))  %>% mutate(week = sprintf("%02d",week))
week_medians_long_merged <- week_medians_long %>% full_join(dfweek,.,by="week",relationship="many-to-many")
df_wyear_long <- df_light_wyear_mean %>% select(-c("days","Beam_temperature_corrected","Node_temperature","Beam_temperature")) %>% 
  gather(key="env_var",value="value",-c("week","year","wyear","date")) %>% mutate(week = sprintf("%02d",week))
head(df_wyear_long)
head(week_medians_long_merged)
df_merged_anomaly_long <-full_join(week_medians_long_merged,df_wyear_long,by=c("wyear","env_var","week","year")) %>% 
  drop_na() %>% mutate(anomaly = value-mean_value)
head(df_merged_anomaly_long)

df_merged_anomaly_long %>% ggplot() + geom_histogram(aes(x=anomaly)) +
  geom_vline(aes(xintercept=0),color="red")+
  labs(x="Average Solar Anomaly",y="Count")

ggsave(filename=paste0(basepath,"/figures/anomaly/histograms/histogram_light_anomaly",Sys.Date(),".png"),
       width=800,height=600,units="px",dpi=150)

skewness(df_merged_anomaly_long$anomaly)
kurtosis(df_merged_anomaly_long$anomaly)

#create short version
df_merged_short_anomaly <- df_merged_long %>%
  pivot_wider(names_from=taxa,values_from=anomaly,id_cols=c("wyear","week","date","year")) %>%
  arrange(date) %>% mutate(week=as.numeric(week))
head(df_merged_short_anomaly