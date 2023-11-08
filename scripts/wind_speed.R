list.of.packages <- c("lubridate","ggplot2",
                      "tibbletime","dplyr","sets",
                      "reshape2","tidyr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)


df_env <- read.csv(paste0(basepath,"/data/mvco/mvco_env_table_2023.csv"))
#do not use temp_beam use beam_temp_corrected from the separate folder
head(df_env)
df_env$date <- as.Date(df_env$time_local,format="%d-%b-%Y %H:%M:%S")
df_env$wind_speed

df_env %>% group_by(date) %>%
  summarise(mean_wind = mean(wind_speed,na.rm=T))

ggplot() + geom_point(data=df_env, aes(x=date,y=wind_speed))

