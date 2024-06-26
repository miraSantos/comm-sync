basepath = "/home/mira/MIT-WHOI/github_repos/comm-sync/"
list.of.packages <- c("RColorBrewer", "lubridate",
                      "ggplot2","tibbletime","dplyr","tidyr","zoo","stringr",
                      "ggsignif","plotly","rlang","dtw","scales","patchwork",
                      "moments","stats","ggfortify")

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
                                mean_weekly_light = mean(AvgSolar,na.rm=T),
                                year=first(year),week=first(week),date=first(date))
head(df_env)

df_env_filled <- df_env %>% 
  group_by(week)%>%
  #replace nans for living things with yearly mean
  mutate(temp_filled=replace_na(mean_weekly_temp,
                                mean(mean_weekly_temp,na.rm=T)),
         light_filled = replace_na(mean_weekly_light,
                                   mean(mean_weekly_light,na.rm=T))) %>%
  filter(year >=2001,year<=2022) %>% ungroup()

df_env_filled %>% ggplot() + geom_point(aes(x=wyear,y=temp_filled))

plot(df_env_filled$temp_filled,type="l")

head(df_env_filled)
tail(df_env_filled)
ts_temp<- ts(df_env_filled$temp_filled,start=c(2002,1),end=c(2022,53),frequency=53) #53 indicates weekly
decom_temp_add <- decompose(ts_temp,type="additive")
decom_temp_mult <- decompose(ts_temp,type="multiplicative")
plot(ts_temp)
str(decom_temp_mult)

qqnorm(decom_temp_mult$random)
qqline(decom_temp_mult$random)

#shapiro test
shapiro.test(ts_temp)

plot(decom_temp_add)


#################################################
light = df_env_filled %>% filter(year >=2005,year<=2020) %>% select(light_filled)
ts_light<- ts(light$light_filled,start=c(2005,1),end=c(2020,53),frequency=53) #53 indicates weekly
decom_light_add <- decompose(ts_light,type="additive")
decom_light_mult <- decompose(ts_light,type="multiplicative")
plot(ts_light)
str(decom_light_mult)

qqnorm(decom_temp_mult$random)
qqline(decom_temp_mult$random)

#shapiro test
shapiro.test(ts_temp)

plot(decom_light_add)


################################################################################
#################################################################################
Time = attributes(ts_temp)[[1]]
Time = seq(Time[1],Time[2], length.out=(Time[2]-Time[1])*Time[3])

dat = cbind(Time, with(decom_temp_add, data.frame(Observed=x,
                                                  Trend=trend,
                                                  Seasonal=seasonal,
                                                  Random=random)))

df_long <- gather(dat, component, value, -Time)
df_long$component_f <- factor(df_long$component,levels=c("Observed","Trend","Seasonal","Random"))

ggplot(df_long, aes(Time, value)) +
  facet_grid(component_f ~ ., scales="free_y") +
  geom_line() +
  theme_bw() +
  labs(y=expression("Temperature ("*degree*"C)"), x="Year") +
  scale_x_continuous(breaks=seq(2002,2023,2),limits=c(2002,2023))+
  theme(plot.title=element_text(hjust=0.5))

ggsave(filename=paste0(basepath,"/figures/decomposition/temperature_decomposition_",Sys.Date(),".png"),
       width=1500,height=1500,units="px",dpi=300)

plot(df_env$week,df_env$mean_weekly_temp)

#looking at seasoanlity
plot(decom_temp_add$figure)
plot(decom_temp_add$seasonal)
plot(decom_temp_add$seasonal+decom_temp_add$trend)
plot(decom_temp_add$seasonal+decom_temp_add$trend+decom_temp_add$random)

shapiro.test(decom_temp_add$random)
### CONCLUSION DECOMPOSING ISNT VERY USEFUL FOR THIS SITE losing alot of information

#stl fit
stl_temp <- stl(ts_temp,s.window=53)
autoplot(stl_temp) + scale_x_continuous(breaks=seq(2002,2023,3))+theme_bw()+
  labs(x="Time",y=expression("Temperature ("*degree*"C)"))

#plot residuals qq quantiles  to see if fit is good
png(filename=paste0(basepath,"/figures/decomposition/temperature/qqplot_temp_",Sys.Date(),".png"),
    width=1500,height=1500,units="px",res=300)
qqnorm(stl_temp$time.series[, "remainder"])
qqline(stl_temp$time.series[, "remainder"])
dev.off()

#shapiro test
shapiro.test(stl_temp$time.series[, "remainder"])
#########################################################

