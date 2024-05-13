df_env <- read.csv("/home/mira/MIT-WHOI/github_repos/comm-sync/data/mvco/mvco_env.csv")
df_env$date <- as.Date(df_env$days,format ="%d-%b-%Y")
df_env$year <- year(df_env$date)

df_env %>% filter(year>=2006) %>% ggplot() + geom_line(aes(x=date,y=Beam_temperature_corrected),size=1.2)+
  xlab("Time")+ylab("Temperature (Deg C)")+
  scale_x_date(date_breaks = "2 years",date_labels = "%Y")
