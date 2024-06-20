basepath = "/home/mira/MIT-WHOI/github_repos/comm-sync/"
list.of.packages <- c("RColorBrewer", "lubridate",
                      "ggplot2","tibbletime","dplyr","tidyr","zoo","stringr",
                      "ggsignif","plotly","rlang","dtw","scales","patchwork",
                      "moments","stats")

#find new packages and install them. require all packages in list
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

#load environmental variable

df_env <- read.csv(paste0(basepath,"/data/mvco/mvco_daily_2023.csv")) %>% 
  mutate(date= as.Date(days,format ="%d-%b-%Y"),
         year=year(date),week=sprintf("%02d",week(date)),
         wyear=paste0(year,"-",week))%>%
  group_by(wyear) %>% summarise(mean_weekly_temp = mean(Beam_temperature_corrected,na.rm=T),
                                year=first(year),week=first(week),date=first(date))
head(df_env)

df_env_filled <- df_env %>% 
  group_by(week)%>%
  #replace nans for living things with yearly mean
  mutate(temp_filled=replace_na(mean_weekly_temp,
                                mean(mean_weekly_temp,na.rm=T))) %>%
  filter(year >=2001,year<=2022)

df_env_filled %>% ggplot() + geom_point(aes(x=wyear,y=temp_filled))

plot(df_env_filled$temp_filled,type="l")

head(df_env_filled)
tail(df_env_filled)
ts_temp<- ts(df_env_filled$temp_filled,start=c(2002,1),end=c(2022,53),frequency=52) #53 indicates weekly
decom_temp_add <- decompose(ts_temp,type="additive")
decom_temp_mult <- decompose(ts_temp,type="multiplicative")
plot(ts_temp)
str(decom_temp_mult)

plot(decom_temp_add)
plot(decom_temp_mult)

str(decom_temp_mult)

plot(df_env$week,df_env$mean_weekly_temp)

plot(decom_temp_add$figure)

plot(decom_temp_add$seasonal)
plot(decom_temp_add$seasonal+decom_temp_add$trend)
plot(decom_temp_add$seasonal+decom_temp_add$trend+decom_temp_add$random)
### CONCLUSION DECOMPOSING ISNT VERY USEFUL FOR THIS SITE losing alot of information


#########################################################

