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
df_env <- read.csv(paste0(basepath,"/data/mvco/mvco_daily_2023.csv"))
head(df_env)
df_env$date <- as.Date(df_env$days,format ="%d-%b-%Y")

temp_raw <- drop_na(df_env[,c("date","Beam_temperature_corrected")])$Beam_temperature_corrected
temp_raw
ts_temp<- ts(temp_raw,start=c(2001,170),end=c(2021,305),frequency=365)
decompose(na.StructTS(ts_temp))
