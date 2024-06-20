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
  mutate(date= as.Date(days,format ="%d-%b-%Y"),wyear=paste0(year(date),"-",week(date)))%>%
  group_by(wyear) %>% summarise(mean_weekly_temp = mean(Beam_temperature_corrected,na.rm=T)) %>%
  drop_na()


head(df_env)
length(df_env$wyear)
tail(df_env)

plot(df_env$mean_weekly_temp)

ts_temp<- ts(df_env$mean_weekly_temp,start=c(2001,9),end=c(2021,9),frequency=53) #53 indicates weekly
decom_temp_add <- decompose(na.StructTS(ts_temp),type="additive")
decom_temp_mult <- decompose(na.StructTS(ts_temp),type="multiplicative")

plot(decom_temp_add)
plot(decom_temp_mult)
