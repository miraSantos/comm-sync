
basepath = "/home/mira/MIT-WHOI/github_repos/comm-sync/"
list.of.packages <- c("RColorBrewer", "lubridate",
                      "ggplot2","tibbletime","dplyr","tidyr","zoo","stringr",
                      "ggsignif","plotly","dtw","scales","patchwork")

#find new packages and install them. require all packages in list
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)


load(paste0(basepath,"data/r_objects/df_stat_opt_thresh.RData"))
load(paste0(basepath,"/data/r_objects/2024-06-04_df_carbonC_filled_super_res_paul.RData"))
load(paste0(basepath,"data/r_objects/c_index_df_cor_2024-06-13.RData"))



df_env <- read.csv(paste0(basepath,"/data/mvco/mvco_daily_2023.csv"))
head(df_env)
df_env$date <- as.Date(df_env$days,format ="%d-%b-%Y")
df_env$year <- year(df_env$date)
df_env$week <- week(df_env$date)

df_env_weekly <-df_env %>% mutate(wyear=paste0(year(date),"-",week(date))) %>% 
  group_by(wyear) %>%
  summarise(mean_temp = mean(Beam_temperature_corrected,na.rm=T),
            mean_light = mean(AvgSolar,na.rm=T),
            week=week,year=year)




week_clim <- df_env %>% group_by(week) %>% summarise(mean_temp=mean(Beam_temperature_corrected,na.rm=T),
                                                     mean_light = mean(AvgSolar,na.rm=T),
                                                     week=week)


df_env_weekly %>% ggplot + geom_line(aes(x=week,y=mean_temp,
                                   color=as.factor(year)),alpha=0.5)+
  geom_line(data=week_clim,aes(x=week,y=mean_temp),linetype="dashed")+
  scale_x_continuous(breaks=seq(1,53,4))+
  labs(color="Year",x="Week",y=expression("Temperature ("*degree*"C)"))+
  guides(color=guide_legend(ncol=2))

ggsave(filename=paste0(basepath,"/figures/temperature_time_series_",Sys.Date(),".png"),
       width=1800,height=1300,units="px",dpi=300)

df_env_weekly %>% ggplot + geom_line(aes(x=week,y=mean_light,
                                         color=as.factor(year)),alpha=0.5)+
  geom_line(data=week_clim,aes(x=week,y=mean_light),linetype="dashed")+
  scale_x_continuous(breaks=seq(1,53,4))+
  labs(color="Year",x="Week",y=expression("Average Daily Integrated Solar Irradiance (Wm"^-2*")"))




df_env= df_env %>% filter(year>=2006)
week <- seq(1,53,1)
years <- seq(min(df_env$year),max(df_env$year),1)
week_list <- rep(week,length(years))
year_list <- rep(years,53)[order(rep(years,53))]
dfweek <- data.frame(wyear=paste0(year_list,"-",week_list),week=week_list,year=year_list)
df_env$year <- year(df_env$date)
df_env$week<-week(df_env$date)


week_means <- df_env %>% 
  group_by(week) %>%
  mutate_at(c("Beam_temperature_corrected","AvgSolar"),mean,na.rm=T) %>%
  rename("mean_temp" = "Beam_temperature_corrected")

#compute mean temperature for each week of the year
df_env_wyear_mean <-df_env_weekly %>%
  select(wyear,mean_temp,mean_light,year,week) %>%
  group_by(wyear) %>%
  mutate_at(c("mean_temp","mean_light"),mean,na.rm=T) %>%
  distinct(wyear, .keep_all=TRUE) %>%
  ungroup()

#approximate annual cycle with interpolation 

ref_year_interp_env <-  df_env_wyear_mean%>%
  left_join(dfweek,.,by=c("wyear","week","year"))%>%
  group_by(year) %>%
  mutate_at(c("mean_temp"),
            list(~na.approx(object=.,x=week,xout=seq(1,53,1),
                            rule=2,ties=mean,method="linear")))

#average weekly annual cycle across entire time series
week_climatology = week_means %>% select(mean_temp,week) %>% distinct()

#create dataframe to store correlations
df_cor <- as.data.frame(matrix(nrow=0,ncol=2))
df_max_xcorr <- data.table::copy(df_cor)
df_lag_xcorr <- data.table::copy(df_cor)

df_dtw <- as.data.frame(matrix(nrow=0,ncol=2))
names(df_cor) <- c("year","temp_interpolated")
names(df_dtw) <- c("year","temp_interpolated")
annual_peak <- as.data.frame(matrix(NaN,nrow = 0,ncol=length("temperature_merge")+1))
names(annual_peak) <- c("year","temperature_merge")

#loop through the years and store correlation
for(y in 1:length(years)){
  print(years[y])
  #extract week year means of a specific year
  individual_year <- ref_year_interp_env %>% ungroup() %>% filter(year == years[y]) %>% 
    select(mean_temp,year)
  #extract diagonal of the correlation matrix
  correlation= diag(cor(week_climatology,individual_year))
  compute_lag <- function(x){if(is.na(max(x$acf))==FALSE){x$lag[which.max(x$acf)]}else{NaN}}
  compute_xcorr<-function(x,y){ccf(x,y,na.action=na.pass,pl=FALSE)}
  #compute cross correation
  xcorr = mapply(compute_xcorr,week_climatology,individual_year,SIMPLIFY=FALSE)
  #extract max correlation
  max_xcorr= lapply(xcorr,function(x)max(x$acf))
  #extract lag at max correlation
  lag_xcorr = lapply(xcorr,compute_lag)
  append_this_cor <- as.data.frame(t(c(year=years[y],correlation)))
  df_cor <- rbind(df_cor,append_this_cor)
  append_this_xcorr <- as.data.frame(do.call(cbind, max_xcorr)) %>% mutate(year=years[y])
  append_this_lag_xcorr <-  as.data.frame(do.call(cbind, lag_xcorr)) %>% mutate(year=years[y])
  df_max_xcorr <- rbind(df_max_xcorr, append_this_xcorr)
  df_lag_xcorr <-rbind(df_lag_xcorr, append_this_lag_xcorr)
  for(i in 1){
  } 
  append_this_cor <- as.data.frame(t(c(year=years[y],correlation)))
  #append to dataframe
  df_cor <- rbind(df_cor,append_this_cor)
}
names(df_cor) <- c("year","mean_temp")

x = seq(1,40,1)
noise = rnorm(length(x),0,sd=0.1)
y=sin(x)
z= sin(x) + 1
dtw(y,z)$normalizedDistance

#take mean of cyclic index
c_index_median = median(df_cor[,"mean_temp"])
c_index_sd <- sd(df_cor[,"mean_temp"])
c_index_max_xcorr <-apply(df_max_xcorr[,"mean_temp"], 2, mean,na.rm=T)
c_index_max_xcorr_sd <-apply(df_max_xcorr[,"mean_temp"], 2, sd,na.rm=T)
c_index_lag_corr <-apply(df_lag_xcorr[,"mean_temp"], 2, mean,na.rm=T)

df_max_xcorr %>% filter(year!=2019) %>% summarise(mean(mean_temp))
df_lag_xcorr %>% filter(year!=2019) %>% summarise(mean(mean_temp))

df_lag_xcorr$var <- "temperature"
df_lag_xcorr %>% ggplot() + 
  geom_tile(aes(x = year, y=var,fill = mean_temp))+
  scale_fill_gradient2(midpoint = 0,
                       mid="#eee8d5", high="#dc322f", low="#268bd2")
# c_index_median_dtw = apply(df_dtw[,protist_tricho_labelC],2,median,na.rm=T)
# consistency.fun <- function(annual_peak){1 - sd(annual_peak - mean(annual_peak))/mean(annual_peak)}
# consistency_index <- apply(annual_peak[,protist_tricho_labelC],2,consistency.fun)

c_index_median
c_index_sd
df_cor

plot(df_cor$temp_interpolated)

individual_year <- ref_year_interp %>% ungroup() %>% filter(year == 2006) %>% select(temp_interpolated)


save(df_env,file=paste0(basepath,"/data/r_objects/df_env_",Sys.Date(),".RData"))
